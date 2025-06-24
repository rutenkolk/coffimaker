(ns coffimaker.core
  "Functions for creating native library bindings for coffi"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.set :as sets]
   [clojure.pprint :as pprint]
   [clojure.java.shell :refer [sh with-sh-dir]]
   [cheshire.core :as json]
   [clj-http.client :as client]
   [me.raynes.fs :as rfs]
   [zigclj.core :as z]
   [clojure.edn :as edn]
   [coffi.ffi :as ffi]
   [coffi.mem :as mem]
   [coffi.layout :as layout]
   [clojure.core.async :as async]
   )
  (:import
   (clojure.lang
    IDeref IFn IMeta IObj IReference)
   (java.lang.invoke
    MethodHandle
    MethodHandles
    MethodType)
   (java.lang.foreign
    Linker
    Linker$Option
    FunctionDescriptor
    AddressLayout
    Arena
    MemoryLayout
    MemorySegment
    MemorySegment$Scope
    SegmentAllocator
    ValueLayout
    ValueLayout$OfByte
    ValueLayout$OfShort
    ValueLayout$OfInt
    ValueLayout$OfLong
    ValueLayout$OfChar
    ValueLayout$OfFloat
    ValueLayout$OfDouble)
   (java.nio ByteOrder)))

(defn- ffilter [pred coll]
  (some #(when (pred %) %) coll))

(defn doall-with-coffi-ns [ns-name forms]
  (do
    (create-ns ns-name)
     (binding [*ns* (the-ns ns-name)]
       (eval
        `(do
           (use '[~'clojure.core])
           (require
            '[~'clojure.java.io :as ~'io]
            '[~'clojure.string :as ~'s]
            '[~'clojure.pprint :as ~'pprint]
            '[~'clojure.edn :as ~'edn]
            '[~'coffi.ffi :as ~'ffi]
            '[~'coffi.mem :as ~'mem]
            '[~'coffi.layout :as ~'layout])
           (import
            '(~'clojure.lang
              ~'IDeref ~'IFn ~'IMeta ~'IObj ~'IReference)
            '(~'java.lang.invoke
              ~'MethodHandle
              ~'MethodHandles
              ~'MethodType)
            '(~'java.lang.foreign
              ~'Linker
              ~'Linker$Option
              ~'FunctionDescriptor
              ~'AddressLayout
              ~'Arena
              ~'MemoryLayout
              ~'MemorySegment
              ~'MemorySegment$Scope
              ~'SegmentAllocator
              ~'ValueLayout
              ~'ValueLayout$OfByte
              ~'ValueLayout$OfShort
              ~'ValueLayout$OfInt
              ~'ValueLayout$OfLong
              ~'ValueLayout$OfChar
              ~'ValueLayout$OfFloat
              ~'ValueLayout$OfDouble)
            '(~'java.nio ~'ByteOrder))))
       (eval (cons `do forms)))))

(defn- copy-resource-to
  "copies a resource file to a target directory, keeping it's name.
  Optionally takes a replacement argument, that can be used to replace strings in the original resource.
  This argument is passed directly to `clojure.string/replace`.
  Strings to be replaced must be of the form \"<<__.*?__>>\"
  In most cases, a map of string to string replacements is sufficient"
  ([resource target-dir]
   (let [target-file (rfs/file target-dir resource)]
     (io/copy (io/input-stream (io/resource resource)) target-file)
     target-file))
  ([resource target-dir replacement]
   (let [target-file (rfs/file target-dir resource)
         raw-content (slurp (io/input-stream (io/resource resource)))
         content (s/replace raw-content #"<<__.*?__>>" replacement)]
     (spit target-file content)
     target-file)))

(defn c-header-info [header opts]
  (z/prepare-zig!)
  (let [header-name (rfs/name (io/file header))
        tmp (rfs/temp-dir "coffimaker")
        _ (copy-resource-to "coffimaker.zig" tmp {"<<__ZIGCLJ_TRANSLATED_HEADER__>>" (str header-name ".zig")})
        build-file (copy-resource-to "build.zig" tmp)
        translated-file (rfs/file tmp (str header-name ".zig"))
        translate-results (z/translate-c-header! header opts)
        zig-source (:zig-source translate-results)
        ]
    (if (not (string? zig-source))
      translate-results
      (let [_ (spit translated-file zig-source)
            build-output (with-sh-dir (str tmp) (z/zig :build :run))]
        (if (= 0 (:exit build-output))
          (edn/read-string (:err build-output))
          build-output)))))

(defn translate-c-header! [header opts]
  (z/prepare-zig!)
  (let [header-name (rfs/name (io/file header))
        tmp (rfs/temp-dir "coffimaker")
        _ (copy-resource-to "coffimaker.zig" tmp {"<<__ZIGCLJ_TRANSLATED_HEADER__>>" (str header-name ".zig")})
        build-file (copy-resource-to "build.zig" tmp)
        translated-file (rfs/file tmp (str header-name ".zig"))
        translate-results (z/translate-c-header! header opts)
        zig-source (:zig-source translate-results)
        _ (if (string? zig-source) (spit translated-file zig-source))]
    (assoc
     translate-results
     :tmp tmp
     :translated-file translated-file)))

(defn- remove-zigclj-from-name [kw]
  (->> (-> kw name (s/split #"__") nnext) (s/join "__") (keyword (str (namespace kw)))))

(defmulti rewrite-zigclj-form #(if (-> % :name name (s/starts-with? "zigclj__")) (-> % :name name (s/split #"__") second (s/replace \_ \-) keyword)))

(defmethod rewrite-zigclj-form :default [x] x)

(defmethod rewrite-zigclj-form :extern-const [{:keys [type name alias-of]}]
  {:type :extern-const
   :name (remove-zigclj-from-name name)
   :kind alias-of})

(defmethod rewrite-zigclj-form :extern-var [{:keys [type name alias-of]}]
  {:type :extern-var
   :name (remove-zigclj-from-name name)
   :kind alias-of})

(defn- rewrite-zigclj-forms [info]
  (->> info (map rewrite-zigclj-form) vec))

(defn build-translate-dir! [dir]
  (let [build-output (with-sh-dir (str dir) (z/zig :build :run))]
    (if (= 0 (:exit build-output))
      (rewrite-zigclj-forms (edn/read-string (:err build-output)))
      build-output)))

(defmacro def- [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))

(defmacro do-with-meta [bindings form]
  (let [bindmap (->>
                  bindings
                  (partition 2 2)
                  (map (fn [[sym metamap]] [sym (with-meta sym metamap)]))
                  (into (hash-map)))]
    (clojure.walk/postwalk
      (fn [x] (get bindmap x x))
      form)))

(defmacro with-typehint [bindings form]
  (let [bindmap (->>
                  bindings
                  (partition 2 2)
                  (map (fn [[sym hint]] [sym (with-meta sym {:tag hint})]))
                  (into (hash-map)))]
    (clojure.walk/postwalk
      (fn [x] (get bindmap x x))
      form)))

(defn vector-nth ^long [v i]
  (.nth ^clojure.lang.IPersistentVector v ^int i))

(defn- f32     [name] [name ::mem/float])
(defn- uchar   [name] [name ::mem/char])
(defn- i8      [name] [name ::mem/byte])
(defn- i16     [name] [name ::mem/short])
(defn- i32     [name] [name ::mem/int])
(defn- i64     [name] [name ::mem/long])
(defn- ui8     [name] [name ::mem/byte])
(defn- ui16    [name] [name ::mem/short])
(defn- ui32    [name] [name ::mem/int])
(defn- ui64    [name] [name ::mem/long])
(defn- u8      [name] [name ::mem/byte])
(defn- u16     [name] [name ::mem/short])
(defn- u32     [name] [name ::mem/int])
(defn- bool    [name] [name ::mem/boolean])
(defn- pointer [name] [name ::mem/pointer])


(def- primitive-typename-conversion
  {:f64            ::mem/double
   :f32            ::mem/float
   :uchar          ::mem/char
   :i8             ::mem/byte
   :i16            ::mem/short
   :i32            ::mem/int
   :i64            ::mem/long
   :ui8            ::mem/byte
   :ui16           ::mem/short
   :ui32           ::mem/int
   :ui64           ::mem/long
   :u8             ::mem/byte
   :u16            ::mem/short
   :u32            ::mem/int
   :u64            ::mem/long
   :bool           ::mem/boolean
   [:pointer :u8]  ::mem/c-string
   :pointer        ::mem/pointer
   :void-pointer   ::mem/pointer
   :void           ::mem/void
   :noreturn       ::mem/void})

(def- primitive-class-conversion
  {::mem/float     'float
   ::mem/double    'double
   ::mem/char      'char
   ::mem/byte      'byte
   ::mem/short     'short
   ::mem/int       'int
   ::mem/long      'long
   ::mem/c-string  java.lang.String
   ::mem/pointer   'long
   [:pointer :u8]  java.lang.String})

(def- array-types-conversion
  {::mem/float     'floats
   ::mem/double    'doubles
   ::mem/char      'chars
   ::mem/byte      'bytes
   ::mem/short     'shorts
   ::mem/int       'ints
   ::mem/long      'longs
   ::mem/pointer   'longs})

(defn coffitype->class [typename]
  (get primitive-class-conversion typename
       (cond
         (and (vector? typename) (= ::mem/array (first typename))) (array-types-conversion (second typename))
         :else (symbol (namespace typename) (name typename)))))

(defn- typename-conversion [t]
  (cond
    (primitive-typename-conversion t) (primitive-typename-conversion t)
    (keyword? t)                       t
    (= :pointer (first t))             [::mem/pointer (typename-conversion (second t))]
    (= :array (first t))               [::mem/array (typename-conversion (second t)) (nth t 2)]
    (= :function-pointer (first t))    [::ffi/fn (vec (map typename-conversion (drop-last (second t)))) (typename-conversion (last (second t)))]
    :else t))

(defn- pointer-erasure [t]
  (if (and (vector? t) (= ::mem/pointer (first t))) ::mem/pointer t))

(defn- typed-decl [[t declname]]
  [declname (typename-conversion t)])

(defn gen-constant [{:keys [name value kind]}]
  (cond
    (#{:true :false} name) nil
    (qualified-keyword? kind)
    `(def ~(symbol (clojure.core/name name))
       (~(symbol (str (clojure.core/name kind) ".") ) ~@value))
    (= kind :bool) `(def ~(vary-meta (symbol (clojure.core/name name)) assoc :const true) ~(case value 0 false 1 true value))
    :else `(def ~(vary-meta (symbol (clojure.core/name name)) assoc :const true) ~value)))

(defn coffitype-to-array-fn [_type]
  (get
   {:coffi.mem/byte   `byte-array
    :coffi.mem/short  `short-array
    :coffi.mem/int    `int-array
    :coffi.mem/long   `long-array
    :coffi.mem/char   `char-array
    :coffi.mem/float  `float-array
    :coffi.mem/double `double-array}
   _type
   `object-array))

(defn- memberlist-typename-conversion [l]
  (vec (reduce concat (map (fn [[type name]] [(pointer-erasure (typename-conversion type)) (symbol (clojure.core/name name))]) l))))

(defn gen-struct [{:keys [name members] :as v}]
  `(mem/defstruct ~(symbol (clojure.core/name name)) ~(vec (reduce concat (map (fn [[type name]] [(symbol (clojure.core/name name)) (typename-conversion type)]) members)))))

(defn gen-alias [{:keys [name alias-of] :as v}]
  `(mem/defalias ~name ~(typename-conversion alias-of))
  )

(defn gen-opaque [{:keys [name] :as v}]
  `(mem/defalias ~name ~:coffi.mem/void))

(defn gen-type [{:keys [kind] :as v}]
  ((case kind
     :struct gen-struct
     :alias gen-alias
     :opaque gen-opaque) v))

(defn- argtype-to-str [v]
  (cond
    (keyword? v) (cond
                   (= (name v) "void") "()"
                   (= (name v) "c-string") "`string`"
                   (= (namespace v) "coffi.mem") (str "`" (name v) "`")
                   :else (str "`" (subs (str v) 1) "`"))
    (vector? v) (let [[fst snd trd & r] v]
                  (case (keyword (name fst))
                    :fn (str "(" (s/join " " (map argtype-to-str snd)) " -> " (argtype-to-str trd) ")")
                    :pointer (str "*" (argtype-to-str snd))
                    :array (str (argtype-to-str snd) "[" trd "]")
                    (str "[" (s/join " " (map argtype-to-str v)) "]")))))

(defn- gen-fn-simple [{:keys [name params return-type] :as fn-info}]
  (let [docstr (str
                (if (= params []) "()")
                (->> params
                     (map first)
                     (map typename-conversion)
                     (map argtype-to-str)
                     (s/join " "))
                " -> "
                (argtype-to-str (typename-conversion return-type)))]
    `(ffi/defcfn
       ~(symbol (clojure.core/name name))
       ~docstr
       {:arglists '~(list (vec (map (comp symbol #(subs % 1) str second) params)))}
       ~(symbol (clojure.core/name name))
       ~(vec (reduce concat (partition 1 2 (memberlist-typename-conversion params))))
       ~(pointer-erasure (typename-conversion return-type)))))

(defn mapp [pred f coll]
  (map #(if (pred %) (f %) %) coll))

(defn- in-list-info [generation-info]
  (let [info (filter #(-> % :type (= ::in-list)) generation-info)
        pairmap (->> info (mapv #(vector (:pointer-argument %) (:count-argument %))) (into (hash-map)))
        [list-args count-args] (map set (apply map vector pairmap))
        process-fn (fn [params]
                     (->> params
                          (filter (comp not count-args second))
                          (mapp #(-> % second list-args) #(update % 0 (fn [x] [(second x)])))))]
    {:info info
     :pairmap pairmap
     :list-args list-args
     :count-args count-args
     :process-params-fn process-fn}))

(defn- in-list-info [generation-info]
  (let [info (filter #(-> % :type (= ::in-list)) generation-info)
        pairmap (->> info (mapv #(vector (:pointer-argument %) (:count-argument %))) (into (hash-map)))
        [list-args count-args] (map set (if (seq pairmap) (apply map vector pairmap) [#{} #{}]))
        process-fn (fn [params]
                     (->> params
                          (filter (comp not count-args second))
                          (mapp #(-> % second list-args) #(update % 0 (fn [x] [(second x)])))))]
    {:info info
     :pairmap pairmap
     :list-args list-args
     :count-args count-args
     :process-params-fn process-fn}))

(defn- out-list-info [generation-info]
  (let [info (filter #(-> % :type (= ::out-list)) generation-info)
        pairmap (->> info (mapv #(vector (:pointer-argument %) (:count-argument %))) (into (hash-map)))
        [list-args count-args] (map set (if (seq pairmap) (apply map vector pairmap) [#{} #{}]))
        process-fn (fn [params]
                     (->> params
                          (filter (comp not count-args second))
                          (filter (comp not list-args second))))
        as-array? (->> info (mapv #(vector (:pointer-argument %) (:as-array %))) (into (hash-map)))
        with-ptr? (->> info (mapv #(vector (:pointer-argument %) (:with-ptr %))) (into (hash-map)))]

    {:info info
     :pairmap pairmap
     :list-args list-args
     :count-args count-args
     :as-array? as-array?
     :with-ptr? with-ptr?
     :process-params-fn process-fn}))

(defn- out-args-info [generation-info]
  (let [info (filter #(-> % :type (= ::out-argument)) generation-info)
        out-args (set (map :argument info))
        process-fn (fn [params]
                     (->> params (filter (comp not out-args second))))]

    {:info info
     :out-args out-args
     :process-params-fn process-fn}))

(defn- return-list-info [generation-info]
  (let [info (filter #(-> % :type (= ::return-list)) generation-info)
        count-args (set (map :count-argument info))
        process-fn (fn [params] (filter (comp not count-args second) params))
        as-array? (set (map :as-array info))
        with-ptr? (set (map :with-ptr info))]
    {:info info
     :count-args count-args
     :as-array? as-array?
     :with-ptr? with-ptr?
     :process-params-fn process-fn}))

(defn- mutating-args-info [generation-info]
  (let [info (filter #(-> % :type (= ::mutating-argument)) generation-info)
        args (set (map :argument info))
        process-fn (fn [params] (mapp #(-> % second args) #(update % 0 (fn [x] (second x))) params))]
    {:info info
     :args args
     :process-params-fn process-fn}))

(defn- gen-fn-alt [{:keys [params return-type generation-info] fn-name :name :as fn-info}]
  (letfn [(param->name
            ([x] (-> x second name symbol))
            ([x postfix] (-> x second name (str postfix) symbol)))
          (params-by-args
            ([args] (filter (comp args second) params))
            ([args p] (filter (comp args second) p)))]
   (let [alt-ident "'"
         {il-pairmap :pairmap ilp-args :list-args il-fn :process-params-fn} (in-list-info generation-info)
         {ol-pairmap :pairmap olp-args :list-args olc-args :count-args ol-fn :process-params-fn} (out-list-info generation-info)
         {rlc-args :count-args rl-fn :process-params-fn} (return-list-info generation-info)
         {out-args :out-args oa-fn :process-params-fn} (out-args-info generation-info)
         {mutating-args :args ma-fn :process-params-fn} (mutating-args-info generation-info)

         return-list? (seq rlc-args)

         params-processed (-> params il-fn ol-fn rl-fn oa-fn ma-fn)

         potential-external-write-args (sets/union olc-args rlc-args)
         external-write-args (->> (params-by-args potential-external-write-args)
                                  (filter (comp sequential? first))
                                  (filter #(-> % ffirst (= :pointer)))
                                  (map second)
                                  (set)
                                  (sets/union out-args))

         deserialize-args (sets/union external-write-args mutating-args)

         in-listptr-params (params-by-args ilp-args params-processed)

         [olp-params serialize-params alloc-params deserialize-params]
         (->> [olp-args mutating-args external-write-args deserialize-args]
              (map params-by-args))

         native-fn-symbol (symbol (str (name fn-name) "-native"))
         arglist (list (mapv (comp symbol second) params-processed))

         return-map (into (hash-map)
                          (concat (if (not= return-type :void) [[:return-value 'return-value]])
                                  (if return-list? [[:return-value-ptr 'return-value-raw]])
                                  (map #(vector % (symbol %)) (sets/union olp-args out-args mutating-args))))

         docstr (str (if (= params []) "()")
                     (->> params-processed
                          (map first)
                          (map typename-conversion)
                          (map argtype-to-str)
                          (s/join " "))
                     " -> "
                     (argtype-to-str
                      (typename-conversion
                       (cond
                         return-list? [(second return-type)]
                         (and (= 1 (count return-map)) (= :void return-type)) (ffirst (filter (comp (set (keys return-map)) second) params-processed))
                         :else return-type))))

         native-fn-call (->> params
                             (map #(param->name % (if ((second %) (sets/union ilp-args olp-args deserialize-args)) alt-ident "")))
                             (cons native-fn-symbol))
         return-type-processed (if return-list? ::mem/pointer (pointer-erasure (typename-conversion return-type)))]
     `(ffi/defcfn
        ~(symbol (name fn-name))
        ~docstr
        {:arglists '~arglist}
        ~(symbol (name fn-name))
        ~(vec (map (fn [[t n]] (if ((keyword n) ilp-args) ::mem/pointer t)) (partition 2 (memberlist-typename-conversion params))))
        ~return-type-processed
        ~native-fn-symbol
        ~(vec (first arglist))

        (with-open [~'arena (mem/confined-arena)]
          (let ~(vec
                 (concat

                  (->> il-pairmap
                       (mapcat (fn [[ptrarg countarg]] `[~(symbol (name countarg)) (count ~(symbol (name ptrarg)))])))

                  (->> in-listptr-params
                       (mapcat #(vector (param->name % alt-ident)
                                        `(mem/serialize ~(param->name %)
                                                        [::mem/array
                                                         ~(typename-conversion (ffirst %))
                                                         ~(symbol (il-pairmap (second %)))]
                                                        ~'arena))))

                  (->> serialize-params
                       (mapcat #(vector (param->name % alt-ident) `(mem/serialize ~(param->name %)
                                                                           ~(typename-conversion (second (first %)))
                                                                           ~'arena))))

                  (->> olp-params (mapcat #(vector (param->name % alt-ident) `(mem/alloc-instance ::mem/pointer ~'arena))))

                  (->> alloc-params
                       (mapcat #(vector (param->name % alt-ident)
                                        `(mem/alloc-instance ~(typename-conversion (second (first %))) ~'arena))))

                  ['return-value-raw native-fn-call]

                  (->> deserialize-params
                       (mapcat #(vector (param->name %)
                                        `(mem/deserialize-from ~(param->name % alt-ident) ~(typename-conversion (second (first %)))))))

                  ['return-value
                   (if return-list?
                     `(mem/deserialize ~'return-value-raw
                                       [::mem/array ~(typename-conversion (second return-type)) ~(symbol (first rlc-args))])
                     'return-value-raw)]

                  (->> olp-params
                       (mapcat #(vector (param->name %)
                                        `(mem/deserialize-from ~(param->name % alt-ident)
                                                               [::mem/array
                                                                ~(typename-conversion (get-in % [0 1 1]))
                                                                ~(symbol (ol-pairmap (second %)))]))))))
            ~(cond
               (= 1 (count return-map)) (first (vals return-map))
               (= 0 (count return-map)) nil
               :else return-map)))))))

(defn gen-fn [fn-info]
  (cond
    (= ::exclude (:generation-info fn-info)) nil
    (:generation-info fn-info) (gen-fn-alt fn-info)
    :else (gen-fn-simple fn-info)))

(defmulti generate-form :type)
(defmethod generate-form :default  [x] (println "no `generate-form` registered for type" (:type x) ".\nvalue was:\n" x))
(defmethod generate-form :type     [x] (gen-type x))
(defmethod generate-form :constant [x] (gen-constant x))
(defmethod generate-form :fn       [x] (gen-fn x))
(defmethod generate-form :extern-var [{:keys [name kind]}]
  `(ffi/defvar ~(symbol (clojure.core/name name)) ~(clojure.core/name name) ~(typename-conversion kind)))
(defmethod generate-form :extern-const [{:keys [name kind]}]
  `(ffi/defvar ~(symbol (clojure.core/name name)) ~(clojure.core/name name) ~(typename-conversion kind)))

(defn generate-from-header-info [header-info]
  (->> header-info
       (map generate-form)
       (filter identity)
       (doall)))

(defn find-all-header-symbols [header-info]
  (->> header-info
       (filter (comp #{:fn :external-var :external-const} :type))
       (map #(vector (:name %) (-> % :name name ffi/find-symbol)))
       (into {})))

(defn remove-missing-symbol-declarations [header-info]
  (filterv (comp not (->> header-info find-all-header-symbols (remove second) (map first) set) :name) header-info))

(defmethod mem/generate-deserialize :coffi.ffi/fn [_type offset segment-source-form] `(mem/deserialize* (mem/read-address ~segment-source-form ~offset) ~_type))
(defmethod mem/generate-serialize :coffi.ffi/fn [_type source-form offset segment-source-form] `(mem/write-address ~segment-source-form ~offset (mem/serialize* ~source-form ~_type (mem/auto-arena))))
(defmethod mem/deserialize-from ::mem/c-string [segment _] (mem/deserialize* segment ::mem/c-string))

(defn- transform-argmod [argmod]
  (if (seqable? (first argmod))
      (mapv transform-argmod argmod)
      (let [replacement-map {:! ::mutate :mutate ::mutate :in ::in :out ::out :as-array ::as-array :with-ptr ::with-ptr :- ::exclude}
         [pre-opts [pre-a b]] (split-at (- (count argmod) 2) argmod)
         a (get {:! ::mutate} pre-a pre-a)
         opts (->> pre-opts (map #(get replacement-map % %)) set)]
     (cond
       (::exclude opts) {:type ::exclude}
       (or (and (empty? opts) (-> a namespace nil?))
           (::in opts)) {:type ::in-list :pointer-argument a :count-argument b}
       (and (empty? opts) (= a ::mutate)) {:type ::mutating-argument :argument b}
       (and (empty? opts) (= a ::out)) {:type ::out-argument :argument b}
       (and (::out opts) (namespace a) (-> a name (= "return")))
         (cond-> {:type ::return-list :count-argument b}
           (::as-array opts) (assoc :as-array true)
           (::with-ptr opts) (assoc :with-ptr true))
       (::out opts) (cond-> {:type ::out-list :pointer-argument a :count-argument b}
                      (::as-array opts) (assoc :as-array true)
                      (::with-ptr opts) (assoc :with-ptr true))
       :else argmod))))

(defn add-generation-info [generation-info header-info]
  (let [processed-info (-> generation-info (update-vals transform-argmod) (update-vals #(if (sequential? %) % [%])))]
    (map #(or (some->> % :name processed-info (assoc % :generation-info)) %) header-info)))


(comment


  (ffi/load-library "sqlite3.dll")

  (def sqlite-info
    (-> "../../sqlite-autoconf/sqlite3.h"
        (translate-c-header! {:compile-error-replacements {"SQLITE_EXTERN" "\n"}})
        :tmp
        build-translate-dir!))

  (doall-with-coffi-ns 'sqlite3 (generate-from-header-info (remove-missing-symbol-declarations sqlite-info)))











  (def dbptr (mem/alloc-instance ::mem/pointer)) ; dbptr is sqlite3**
  (def stmtptr (mem/alloc-instance ::mem/pointer)) ;stmtptr is sqlite3_stmt**

  (sqlite3/sqlite3_open "expenses.db" dbptr)
  (def db (mem/read-address dbptr)) ; db is sqlite3*

  (sqlite3/sqlite3_prepare_v2 db "select * from Lmao" -1 stmtptr mem/null)

  (def stmt (mem/read-address stmtptr))

  (defn get-current-sqlite-col [stmt]
    (vec
     (for [i (range (sqlite3/sqlite3_column_count stmt))]
       (condp = (sqlite3/sqlite3_column_type stmt i)
         sqlite3/SQLITE_INTEGER (sqlite3/sqlite3_column_int stmt i)
         sqlite3/SQLITE_FLOAT   (sqlite3/sqlite3_column_double stmt i)
         sqlite3/SQLITE3_TEXT   (sqlite3/sqlite3_column_text stmt i)))))

  (defn sqlite-has-next [stmt]
    (not= sqlite3/SQLITE_DONE (sqlite3/sqlite3_step stmt)))

  (defn get-next-sqlite-col [stmt]
    (if (sqlite-has-next stmt) (get-current-sqlite-col stmt)))

  (defn get-all-sqlite-cols [stmt]
    (->> #(get-next-sqlite-col stmt) repeatedly (take-while some?) vec))

  (get-all-sqlite-cols stmt)

  (sqlite3/sqlite3_finalize stmt)
  (sqlite3/sqlite3_close db)











  (def raylib-header-info
    (c-header-info
     "../raylib/src/raylib.h"
     {:compile-error-replacements {"RL_MALLOC"  "\n"
                                   "RL_CALLOC"  "\n"
                                   "RL_REALLOC" "\n"
                                   "RL_FREE"    "\n"}}))

  (c-header-info
   "../cpu_features/include/cpuinfo_x86.h"
   {:remove-underscore false
    :compile-error-replacements {"__thiscall" "\n"
                                 "__INT64_C_SUFFIX__" "\n"
                                 "__UINTMAX_C_SUFFIX__" "\n"
                                 "__UINT32_C_SUFFIX__" "\n"
                                 "_cdecl" "\n"
                                 "__declspec" "\n"
                                 "_pascal" "\n"
                                 "_stdcall" "\n"
                                 "__seg_fs" "\n"
                                 "__fastcall" "\n"
                                 "_thiscall" "\n"
                                 "__stdcall" "\n"
                                 "__seg_gs" "\n"
                                 "_fastcall" "\n"
                                 "__pascal" "\n"
                                 "CPU_FEATURES_DEPRECATED" "\n"
                                 "__UINT64_C_SUFFIX__" "\n"
                                 "__cdecl" "\n"
                                 "__INTMAX_C_SUFFIX__" "\n"}})

  (let [{:keys [zig-source declarations tmp translated-file]}
        (translate-c-header!
         "../cpu_features/include/cpuinfo_x86.h"
         {:remove-underscore false
          :compile-error-replacements {"__thiscall" "\n"
                                       "__INT64_C_SUFFIX__" "\n"
                                       "__UINTMAX_C_SUFFIX__" "\n"
                                       "__UINT32_C_SUFFIX__" "\n"
                                       "_cdecl" "\n"
                                       "__declspec" "\n"
                                       "_pascal" "\n"
                                       "_stdcall" "\n"
                                       "__seg_fs" "\n"
                                       "__fastcall" "\n"
                                       "_thiscall" "\n"
                                       "__stdcall" "\n"
                                       "__seg_gs" "\n"
                                       "_fastcall" "\n"
                                       "__pascal" "\n"
                                       "CPU_FEATURES_DEPRECATED" "\n"
                                       "__UINT64_C_SUFFIX__" "\n"
                                       "__cdecl" "\n"
                                       "__INTMAX_C_SUFFIX__" "\n"}})]
    (->> declarations
         (filter (comp #{"X86Features"} :name))
         first
         :full-match
         (#(s/replace zig-source % "pub const X86Features = [10]u8;"))
         (spit translated-file))
    (->> (build-translate-dir! tmp)
         generate-from-header-info
         #_(doall-with-coffi-ns 'cpuinfo_x86))
   )

  (ffi/find-symbol 'GetX86Info)
  (ffi/find-symbol "GetX86Info")

  (ffi/load-library "cpu_features.dll")


  (doall-with-coffi-ns 'raylib (generate-from-header-info raylib-header-info))


(c-header-info
   "../cpu_features/include/cpuinfo_x86.h"
   {:remove-underscore false
    :compile-error-replacements {"__thiscall" "\n"
                                 "__INT64_C_SUFFIX__" "\n"
                                 "__UINTMAX_C_SUFFIX__" "\n"
                                 "__UINT32_C_SUFFIX__" "\n"
                                 "_cdecl" "\n"
                                 "__declspec" "\n"
                                 "_pascal" "\n"
                                 "_stdcall" "\n"
                                 "__seg_fs" "\n"
                                 "__fastcall" "\n"
                                 "_thiscall" "\n"
                                 "__stdcall" "\n"
                                 "__seg_gs" "\n"
                                 "_fastcall" "\n"
                                 }})

(c-header-info
   "../../sqlite-autoconf/sqlite3.h"
   {:remove-underscore true
    :compile-error-replacements {"SQLITE_EXTERN" "\n"
                                 }})

(generate-from-header-info sqlite-info)

(edn/read-string (slurp "/Users/Kristin/AppData/Local/Temp/coffimaker1742415018048-903378652/out.edn"))

(ffi/load-library "sqlite3.dll")

(ffi/defvar sqlite-temp-dir "sqlite3_temp_directory" ::mem/c-string)
@sqlite-temp-dir

(ffi/defvar sqlite3_version "sqlite3_version" ::mem/c-string)

(mem/deserialize* (ffi/find-symbol "sqlite3_version") ::mem/c-string)

(mem/deserialize (ffi/find-symbol "sqlite3_version") ::mem/c-string)

(mem/deserialize-from (ffi/find-symbol "sqlite3_version") ::mem/c-string)

(mem/size-of ::mem/long)

(into {} (.canonicalLayouts (java.lang.foreign.Linker/nativeLinker))) "long"

(defmethod mem/deserialize-from ::mem/c-string [segment _] (mem/deserialize* segment ::mem/c-string))

(mem/primitive-type ::mem/c-string)

(type sqlite3_version)

@sqlite3_version

(ffi/find-symbol "sqlite3_version")

((ffi/cfn "sqlite3_libversion" [] ::mem/c-string))

  (z/translate-c-header!
   "../cpu_features/include/cpuinfo_x86.h"
   {:remove-underscore false
    :compile-error-replacements {"__thiscall" "\n"
                                 "__INT64_C_SUFFIX__" "\n"
                                 "__UINTMAX_C_SUFFIX__" "\n"
                                 "__UINT32_C_SUFFIX__" "\n"
                                 "_cdecl" "\n"
                                 "__declspec" "\n"
                                 "_pascal" "\n"
                                 "_stdcall" "\n"
                                 "__seg_fs" "\n"
                                 "__fastcall" "\n"
                                 "_thiscall" "\n"
                                 "__stdcall" "\n"
                                 "__seg_gs" "\n"
                                 "_fastcall" "\n"
                                 "__pascal" "\n"
                                 "CPU_FEATURES_DEPRECATED" "\n"
                                 "__UINT64_C_SUFFIX__" "\n"
                                 "__cdecl" "\n"
                                 "__INTMAX_C_SUFFIX__" "\n"}})



(s/replace
 "pub extern fn sqlite3_exec(?*sqlite3, sql: [*c]const u8, callback: ?*const fn (?*anyopaque, c_int, [*c][*c]u8, [*c][*c]u8) callconv(.C) c_int, ?*anyopaque, errmsg: [*c][*c]u8) c_int; "
 #"pub extern fn ([^\)]+?)\((.*?)\)\s[^;()]+;"
 (fn [[full fn-name params-match]]
   (->> params-match
        (remove-parens)
        (re-seq #"\s?([^,]+)")
        (map second)
        (map #(first (re-seq #"\s?([^:,]+?):|," %)))
        (map #(if (nil? %) "_" (second %)))
        (map #(s/replace % #"\"" ""))
        (map #(s/replace % #"@" ""))
        (map #(str \" % \"))
        (s/join ", " ))))

(s/replace
 "?*sqlite3, sql: [*c]const u8, callback: ?*const fn (?*anyopaque, c_int, [*c][*c]u8, () [*c][*c]u8) callconv(.C) c_int, ?*anyopaque, errmsg: [*c][*c]u8 "
 #"\([^\(\)]*?\)"
 ""
 )

(->>
 ["?*sqlite3" " sql: [*c]const u8" " callback: ?*const fn  callconv c_int" " ?*anyopaque" " errmsg: [*c][*c]u8 " ]
 (map #(first (re-seq #"\s?([^:,]+?):|," %)))
 (map #(if (nil? %) "_" (second %)))
 )

(remove-parens "?*sqlite3, sql: [*c]const u8, callback: ?*const fn (?*anyopaque, c_int, [*c][*c]u8, ( () () ) [*c][*c]u8) callconv(.C) c_int, ?*anyopaque, errmsg: [*c][*c]u8 ")

(->>
 "?*sqlite3, sql: [*c]const u8, callback: ?*const fn (?*anyopaque, c_int, [*c][*c]u8, [*c][*c]u8 "
 (re-seq #"")
     )

  (slurp (io/input-stream (io/resource "coffimaker.zig")))

  (set! *print-meta* true)




























  (ffi/load-library "raylib.dll")

  (->>
   raylib-header-info
   (generate-from-header-info)
   (doall-with-coffi-ns 'raylib))

  (def state (atom {:last-time (System/nanoTime) :acc []}))

  (def factor 1000000000)

  (async/thread
    (raylib/InitWindow 800 450 "raylib-clj [core] example - basic window")
    (raylib/SetTargetFPS 10000)
    (while (not (raylib/WindowShouldClose))
      (let [{:keys [last-time acc f]} @state
            newtime (System/nanoTime)
            diff (- newtime last-time)
            newacc (vec (take-last 5000 (conj acc diff)))
            average-diff (/ (reduce + newacc) (count newacc))
            average-fps (long (/ 1000000000 average-diff))]
        (swap! state assoc :last-time newtime :acc newacc)
        (raylib/BeginDrawing)
        (raylib/ClearBackground raylib/RAYWHITE)
        (raylib/DrawText "Congrats! You created your first raylib window!" 190 200 20 raylib/BLACK)
        (raylib/DrawText "And you did it from clojure!" (int (+ 190 (* 50 (Math/sin (/ newtime factor))))) 240 20 raylib/DARKBLUE)
        (raylib/DrawText (str "fps: " average-fps ) 190 380 20 raylib/BLACK)
        (raylib/EndDrawing)))
    (raylib/CloseWindow))











  )


