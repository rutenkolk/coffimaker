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
   [coffimaker.runtime :as runtime]
   [clojure.core.async :as async])
  (:import
   (clojure.lang
    IDeref IFn IMeta IObj IReference Settable Named)
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

(defn- remove-meta [form] (clojure.walk/postwalk (fn [x] (if (meta x) (with-meta x {}) x)) form))

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
            '[~'coffi.layout :as ~'layout]
            '[~'coffimaker.runtime :as ~'runtime])
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

(defn generate-coffi-file [ns-name forms]
  (concat
   (remove-meta
    [(list
      'ns
      ns-name
      '(:require
        [clojure.java.io :as io]
        [clojure.string :as s]
        [clojure.set :as sets]
        [clojure.pprint :as pprint]
        [clojure.edn :as edn]
        [coffi.ffi :as ffi]
        [coffi.mem :as mem]
        [coffi.layout :as layout]
        [coffimaker.runtime :as runtime])
      '(:import
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
     `(set! *warn-on-reflection* true)
     `(ffi/load-library ~(str ns-name ".dll"))])
   forms))

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
(defn- bool    [name] [name ::runtime/bool])
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
   :bool           ::runtime/bool
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

(defn- coffitype->class [typename]
  (get primitive-class-conversion typename
       (cond
         (and (vector? typename) (= ::mem/array (first typename))) (array-types-conversion (second typename))
         :else (symbol (namespace typename) (name typename)))))

(defn- typename-conversion [t]
  (cond
    (primitive-typename-conversion t) (primitive-typename-conversion t)
    (keyword? t)                       t
    (let [[p m it] t] (and (= :pointer p) (= m :raw))) [::mem/pointer (typename-conversion (second (rest t)))]
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

(defn- coffitype-to-array-fn [_type]
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
  `(mem/defalias ~name ~(typename-conversion alias-of)))

(defn gen-opaque [{:keys [name] :as v}]
  `(mem/defalias ~name ~:coffi.mem/void))

(defn gen-type [{:keys [kind] tname :name :as v}]
  (letfn [(lsym
            ([] (->> tname symbol))
            ([p] (->> tname name (str "-->" p "-") symbol)))]
    [((case kind
        :struct gen-struct
        :alias gen-alias
        :opaque gen-opaque) v)]))

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
                    :pointer (str "*" (argtype-to-str (if (= :raw snd) trd snd)))
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

(defn- mapp [pred f coll]
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

(declare gen-fn)

(defn- callp [pred f v]
  (if (pred v) (f v) v))

(defn- gen-fn-alt [{:keys [params return-type generation-info] fn-name :name :as fn-info}]
  (if (seq (filter #(-> % :type (= ::no-str)) generation-info))
    (let [no-strs (->> generation-info
                       (filter #(-> % :type (= ::no-str)))
                       (map :argument)
                       set)]
      (-> fn-info
          (update :return-type #(if (::return no-strs) [:pointer :raw :u8] %))
          (update :generation-info (fn [x] (filter #(-> % :type (not= ::no-str)) x)))
          (update :params #(mapp
                            (comp no-strs second)
                            (fn [[[_ it] n]] [[:pointer :raw it] n])
                            %))
          (gen-fn)))

    (letfn [(param->name
              ([x] (-> x second name symbol))
              ([x postfix] (-> x second name (str postfix) symbol)))
            (params-by-args
              ([args] (filter (comp args second) params))
              ([args p] (filter (comp args second) p)))
            (kw-str
              ([kw] (kw-str nil kw nil))
              ([pre kw] (kw-str pre kw nil))
              ([pre kw post] (str pre (some-> kw namespace (s/replace "." "_") (str "__")) (name kw) post)))
            (kw-sym
              ([kw] (kw-sym nil kw nil))
              ([pre kw] (kw-sym pre kw nil))
              ([pre kw post] (symbol (kw-str pre kw post))))
            (reinter [x sz] `(.reinterpret ~(with-meta x {:tag 'java.lang.foreign.MemorySegment}) ~sz))
            (primitive-serde? [x] (and (get-method mem/generate-serialize x) (get-method mem/generate-deserialize x) (mem/primitive? x)))]
      (let [alt-ident "'"
            mut-ident "!"

            {il-pairmap :pairmap ilp-args :list-args il-fn :process-params-fn} (in-list-info generation-info)
            {ol-pairmap :pairmap olp-args :list-args olc-args :count-args ol-fn :process-params-fn} (out-list-info generation-info)
            {rlc-args :count-args rl-fn :process-params-fn} (return-list-info generation-info)
            {out-args :out-args oa-fn :process-params-fn} (out-args-info generation-info)
            {mutating-args :args ma-fn :process-params-fn} (mutating-args-info generation-info)

            mutating-symbols (set (map symbol mutating-args))
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
            return-type-processed (if return-list? ::mem/pointer (pointer-erasure (typename-conversion return-type)))

            alloc-multi (->> in-listptr-params
                             (map #(typename-conversion (ffirst %)))
                             (mapcat #(vector
                                       (kw-sym "size-of-" %) `(mem/size-of ~%)
                                       (kw-sym "alloc-" % "-list") `(fn [~'len ~'arena] (mem/alloc (* ~'len ~(kw-sym "size-of-" %)) ~'arena)))))

            alloc-single (->>
                          (concat serialize-params alloc-params)
                          (map #(typename-conversion (second (first %))))
                          (concat (if olp-args [::mem/pointer]))
                          (mapcat #(vector
                                    (kw-sym "size-of-" %) `(mem/size-of ~%)
                                    (kw-sym "alloc-" %) `(fn [~'arena] (mem/alloc ~(kw-sym "size-of-" %) ~'arena)))))

            serialize-fns (->>
                           (concat
                            (map #(typename-conversion (ffirst %)) in-listptr-params)
                            (map #(typename-conversion (second (first %))) serialize-params))
                           (mapcat #(vector (kw-sym "serialize-into-" %) `(get-method mem/serialize-into ~%))))

            deserialize-fns (->>
                           (concat
                            (map #(typename-conversion (second (first %))) deserialize-params)
                            (map #(typename-conversion (get-in % [0 1 1])) olp-params)
                            (if return-list? [(typename-conversion (second return-type))]))
                           (mapcat #(vector
                                     (kw-sym "size-of-" %) `(mem/size-of ~%)
                                     (kw-sym "deserialize-from-" %) `(get-method mem/deserialize-from ~%))))

            ]
        `(let
             ~(->> (concat alloc-single alloc-multi serialize-fns deserialize-fns)
                   (partition 2)
                   (map vec)
                   (map #(update % 0 symbol))
                   (distinct)
                   (sort-by first)
                   (reverse)
                   (reduce concat)
                   (vec))

             (ffi/defcfn
               ~(symbol (name fn-name))
               ~docstr
               {:arglists '~arglist}
               ~(symbol (name fn-name))
               ~(vec (map (fn [[t n]] (if ((keyword n) ilp-args) ::mem/pointer t)) (partition 2 (memberlist-typename-conversion params))))
               ~return-type-processed
               ~native-fn-symbol
               ~(vec (mapp mutating-symbols #(symbol (str % mut-ident)) (first arglist)))

               (with-open [~'arena (mem/thread-local-arena)]
                 (let ~(vec
                        (concat

                         (->> il-pairmap
                              (mapcat (fn [[ptrarg countarg]] `[~(symbol (name countarg)) (count ~(symbol (name ptrarg)))])))

                         (->> in-listptr-params
                              (mapcat #(let [obj (param->name %)
                                             member-type (typename-conversion (ffirst %))
                                             length (symbol (il-pairmap (second %)))]
                                         [(param->name % alt-ident)
                                          `(let [~'local-segment (~(kw-sym "alloc-" member-type "-list") ~length ~'arena)]
                                             (loop [~'xs (seq ~obj) ~'offset 0]
                                               (if ~'xs
                                                 (do
                                                   ~(if (primitive-serde? member-type)
                                                      (mem/generate-serialize member-type `(first ~'xs) 'offset 'local-segment)
                                                      `(~(kw-sym "serialize-into-" member-type) (first ~'xs) nil ~'local-segment nil))
                                                   (recur (next ~'xs) (+ ~'offset ~(kw-sym "size-of-" member-type))))))
                                             ~'local-segment)])))

                         (->> serialize-params
                              (mapcat #(let [obj (callp mutating-symbols (fn [x] (symbol (str x mut-ident))) (param->name %))
                                             member-type (typename-conversion (second (first %)))]
                                         [(param->name % alt-ident)
                                          `(let [~'local-segment (~(kw-sym "alloc-" member-type) ~'arena)]
                                             ~(if (primitive-serde? member-type)
                                                (mem/generate-serialize member-type obj 0 'local-segment)
                                                `(~(kw-sym "serialize-into-" member-type) ~obj nil ~'local-segment nil))
                                             ~'local-segment)])))

                         (->> olp-params (mapcat #(vector (param->name % alt-ident) `(~(kw-sym "alloc-" ::mem/pointer) ~'arena))))

                         (->> alloc-params
                              (mapcat #(let [member-type (typename-conversion (second (first %)))]
                                         [(param->name % alt-ident) `(~(kw-sym "alloc-" member-type) ~'arena)])))

                         ['return-value-raw native-fn-call]

                         (->> deserialize-params
                              (mapcat #(let [obj (param->name % alt-ident)
                                             t (typename-conversion (second (first %)))]
                                         [(param->name %)
                                          (cond
                                            (primitive-serde? t)
                                            (mem/generate-deserialize t 0 (reinter obj (mem/size-of t)))

                                            :else
                                            `(~(kw-sym "deserialize-from-" t) ~(reinter obj (kw-sym "size-of-" t)) nil))])))

                         ['return-value
                          (if return-list?
                            (let [member-type (typename-conversion (second return-type))
                                  length (symbol (first rlc-args))
                                  t `[::mem/array ~member-type ~length]
                                  segment (reinter 'return-value-raw `(* ~length ~(kw-sym "size-of-" member-type)))
                                  loop-deserialize (if (primitive-serde? member-type)
                                                     (mem/generate-deserialize member-type `(* ~(mem/size-of member-type) ~'i) segment)
                                                     `(~(kw-sym "deserialize-from-" member-type) (runtime/unsafe-offset ~segment (* ~(kw-sym "size-of-" member-type) ~'i)) nil))]
                              `(loop [~'i 0 ~'v (transient [])]
                                 (if (< ~'i ~length)
                                   (recur (unchecked-inc ~'i) (conj! ~'v ~loop-deserialize))
                                   (persistent! ~'v))))
                            'return-value-raw)]

                         (->> olp-params
                              (mapcat #(let [member-type (typename-conversion (get-in % [0 1 1]))
                                             length (symbol (ol-pairmap (second %)))
                                             t `[::mem/array ~member-type ~length]
                                             size (kw-sym "size-of-" member-type)
                                             segment-form (reinter (param->name % alt-ident) `(* ~length ~size))
                                             segment (gensym "segment")
                                             loop-deserialize (if (and (get-method mem/generate-deserialize member-type) (mem/primitive? member-type))
                                                                (mem/generate-deserialize
                                                                 member-type
                                                                 `(* ~size ~'i)
                                                                 segment)

                                                                `(~(kw-sym "deserialize-from-" member-type) (runtime/unsafe-offset ~segment-form (* ~size ~'i)) nil))]
                                         [(param->name %)
                                          `(loop [~'i 0 ~'v (transient [])]
                                             (if (< ~'i ~length)
                                               (recur (unchecked-inc ~'i) (conj! ~'v ~loop-deserialize))
                                               (persistent! ~'v)))])))))
                   ~(cond
                      (= 1 (count return-map)) (first (vals return-map))
                      (= 0 (count return-map)) nil
                      :else return-map)))))))))

(defn gen-fn [fn-info]
  (cond
    (::exclude (set (map :type (:generation-info fn-info)))) nil
    (seq (:generation-info fn-info)) (gen-fn-alt fn-info)
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

(defn change-ns-walk [from to form]
  (clojure.walk/postwalk
   (fn [x]
     (let [nspace (if (instance? Named x) (namespace x))
           n (if (instance? Named x) (name x))
           f (if (keyword? x) keyword symbol)]
       (if (= nspace (str from)) (f (str to) n) x)))
   form))

(defn generate-from-header-info [header-info]
  (->> header-info
       (map generate-form)
       (mapcat #(if (vector? %) % [%]))
       (filter identity)
       (doall)))

(defn find-all-header-symbols [header-info]
  (->> header-info
       (filter (comp #{:fn :external-var :external-const} :type))
       (map #(vector (:name %) (-> % :name name ffi/find-symbol)))
       (into {})))

(defn remove-missing-symbol-declarations [header-info]
  (filterv (comp not (->> header-info find-all-header-symbols (remove second) (map first) set) :name) header-info))

(defn- transform-argmod [argmod]
  (cond
    (not (sequential? argmod)) (transform-argmod [argmod])
    (sequential? (first argmod)) (mapv transform-argmod argmod)
    :else
    (let [replacement-map {:! ::mutate :mutate ::mutate :in ::in :out ::out :as-array ::as-array :with-ptr ::with-ptr :- ::exclude}
          [pre-opts [pre-a b]] (split-at (- (count argmod) 2) (if (not (sequential? argmod)) [argmod] argmod))
          a (get replacement-map pre-a pre-a)
          opts (->> pre-opts (map #(get replacement-map % %)) set)]
      (cond
        (or (::exclude opts) (= a ::exclude)) {:type ::exclude}


        (and (empty? opts) (= a ::no-str)) {:type ::no-str :argument b}

        (or (and (empty? opts) (-> a namespace nil?))
            (::in opts))
        {:type ::in-list :pointer-argument a :count-argument b}

        (and (empty? opts) (= a ::mutate))
        {:type ::mutating-argument :argument b}

        (and (empty? opts) (= a ::out))
        {:type ::out-argument :argument b}

        (and (::out opts) (namespace a) (-> a name (= "return")))
        (cond-> {:type ::return-list :count-argument b}
          (::as-array opts) (assoc :as-array true)
          (::with-ptr opts) (assoc :with-ptr true))

        (::out opts)
        (cond-> {:type ::out-list :pointer-argument a :count-argument b}
          (::as-array opts) (assoc :as-array true)
          (::with-ptr opts) (assoc :with-ptr true))

        :else argmod))))

(defn add-generation-info [generation-info header-info]
  (let [processed-info (-> generation-info (update-vals transform-argmod) (update-vals #(if (sequential? %) % [%])))]
    (map #(or (some->> % :name processed-info (assoc % :generation-info)) %) header-info)))

(defn get-header-info-with-pointer-parameters [header-info]
  (->>
   header-info
   (filter #(= (:type %) :fn))
   (filter
    (fn [x]
      (->> x
           :params
           (map first)
           (map #(if (= % [:pointer :u8]) :str %))
           (map #(get % 0))
           (some #{:pointer}))))))

