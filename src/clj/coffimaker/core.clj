(ns coffimaker.core
  "Functions for creating native library bindings for coffi"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
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
   (java.nio ByteOrder)
   )
  )

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
        translated-file (rfs/file tmp (str header-name ".zig"))]
    (spit
     translated-file
     (-> header
         (z/translate-c-header! opts)
         (z/post-process-header-translation)))
    (->
     (with-sh-dir (str tmp) (z/zig :build :run))
     (:err)
     (edn/read-string))))

(defmacro def- [name & decls]
  (list* `def (with-meta name (assoc (meta name) :private true)) decls))

(defmacro defconst [name & decls]
  (list* `def (with-meta name (assoc (meta name) :const true)) decls))

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
(defn- ui8     [name] [name ::mem/byte])
(defn- ui16    [name] [name ::mem/short])
(defn- ui32    [name] [name ::mem/int])
(defn- u8      [name] [name ::mem/byte])
(defn- u16     [name] [name ::mem/short])
(defn- u32     [name] [name ::mem/int])
(defn- bool    [name] [name ::mem/byte])
(defn- pointer [name] [name ::mem/pointer])


(def- primitive-typename-conversion
  {:f32            ::mem/float
   :uchar          ::mem/char
   :i8             ::mem/byte
   :i16            ::mem/short
   :i32            ::mem/int
   :ui8            ::mem/byte
   :ui16           ::mem/short
   :ui32           ::mem/int
   :u8             ::mem/byte
   :u16            ::mem/short
   :u32            ::mem/int
   :bool           ::mem/byte
   [:pointer :u8]  ::mem/c-string
   :pointer        ::mem/pointer
   :void-pointer   ::mem/pointer
   })

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
   [:pointer :u8]  java.lang.String
   }
  )

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
    (not (vector? t))                  :t
    (= :pointer (first t))             ::mem/pointer
    (= :array (first t))              [::mem/array (typename-conversion (second t)) (nth t 2)]
    (= :function-pointer (first t))    :idk-something-something-fn-pointer ; TODO
    ))

(defn- typed-decl [[t declname]]
  [declname (typename-conversion t)])

(defn get-constant-defs [header-info]
  (->>
   header-info
   (:constant)
   (filter (comp not resolve symbol name :name))
   (filter #(not (#{:true :false} (:name %))))
   (map (fn [v] (list 'defconst (symbol (name (:name v))) (:value v))))))

(defn def-constants! [header-info]
  ((cons `do (get-constant-defs header-info))))

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

(defn gen-struct [{:keys [name members] :as v}]
  `(mem/defstruct ~name ~(vec (reduce concat (map (fn [[type name]] [(typename-conversion type) (symbol (clojure.core/name name))]) members)))))

(defn gen-alias [{:keys [name alias-of] :as v}]
  `(mem/defalias ~name ~(typename-conversion alias-of))
  )

(defn gen-opaque [{:keys [name] :as v}]
  `(mem/defalias ~name ~:coffi.mem/byte))

(defn gen-type [{:keys [kind] :as v}]
  ((case kind
     :struct gen-struct
     :alias gen-alias
     :opaque gen-opaque) v))

(comment
  (def raylib-header-info
    (c-header-info
     "../raylib/src/raylib.h"
     {:compile-error-replacements {"RL_MALLOC"  "\n"
                                   "RL_CALLOC"  "\n"
                                   "RL_REALLOC" "\n"
                                   "RL_FREE"    "\n"}}))

  (coffitype->class :raylib/MyType)
  (coffitype->class ::mem/int)
  (Integer/TYPE)

  (eval (gen-struct-type :raylib/MyType
                    [[:id :coffi.mem/int]
                     [:weird :coffi.mem/byte]
                     [:weird2 [::mem/array :coffi.mem/double 4]]
                     [:width :coffi.mem/double]
                     [:mipmaps :coffi.mem/short]
                     [:format :coffi.mem/float]]

                    )


        )

  (.getType (.getField MyType "id"))
  (.getType (.getField MyType "weird"))

  (do-with-meta [c {:tag short}]
    (defrecord MyType
        [^int a
         ^{:tag double} b
         c
         ]))

  (with-typehint [c short]
    (defrecord MyType
        [^int a
         ^{:tag double} b
         c
         ]))

  (.getType (.getField MyType "c"))

  (->>
   raylib-header-info
   (group-by :type)
   (:type)
   (map gen-type)
   )

  (->>
   raylib-header-info
   (group-by :type)
   (get-constant-defs)
   )

   (mem/defalias ::CustomStructType
     (layout/with-c-layout
       [:coffi.mem/struct
        '([:id :coffi.mem/int]
          [:weird :coffi.mem/byte]
          [:weird2 :coffi.mem/double]
          [:width :coffi.mem/double]
          [:mipmaps :coffi.mem/short]
          [:format :coffi.mem/float])]))

   (gen-serialize-into ::Texture2D
                       (coffi.layout/with-c-layout
                         [:coffi.mem/struct
                          '([:id :coffi.mem/int]
                            [:width :coffi.mem/double]
                            [:height :coffi.mem/int]
                            [:some_other_struct ::CustomStructType]
                            [:weird :coffi.mem/byte]
                            [:weird2 [::mem/array :coffi.mem/double 4]]
                            [:mipmaps :coffi.mem/int]
                            [:format :coffi.mem/float])])
                       )

(with-typehint
   [x float y float]
   (clojure.core/deftype
    Vector2
    [x y]
    clojure.lang.IPersistentVector
    clojure.lang.IPersistentMap
    (length [this] 2)
    (assocN [this i value] (clojure.core/assoc i [(.x this) (.y this)] value))
    (cons [this o] [o (.x this) (.y this)])
    (peek [this] (.x this))
    (pop [this] [(.y this)])
    (count [this] 2)
    (empty [this] [])
    (equiv
     [this o]
     (clojure.core/or
      (clojure.core/= [(.x this) (.y this)] o)
      (clojure.core/= {:x (.x this), :y (.y this)} o)))
    (seq [this] (clojure.core/seq [(.x this) (.y this)]))
    (rseq [this] [(.y this) (.x this)])
    (nth [this i] (clojure.core/case i 0 (.x this) 1 (.y this)))
    (nth [this i o] (clojure.core/case i 0 (.x this) 1 (.y this) o))
    (assoc
     [this i value]
     (if
      (clojure.core/number? i)
      (clojure.core/assoc [(.x this) (.y this)] i value)
      {:x (.x this), :y (.y this), i value}))
    (assocEx
     [this i value]
     (if
      (#{:y :x} i)
      (throw (java.lang.Exception. "key already exists"))
      {:x (.x this), :y (.y this), i value}))
    (without
     [this k]
     (clojure.core/dissoc
      {:x (.x this), :y (.y this)}
      (if (clojure.core/number? k) ([:x :y] k) k)))
    (containsKey
     [this k]
     (if
      (clojure.core/number? k)
      (clojure.core/and (clojure.core/>= k 0) (clojure.core/< k 2))
      (#{:y :x} k)))
    (entryAt
     [this k]
     (clojure.lang.MapEntry/create
      k
      (clojure.core/case k 0 (.x this) 1 (.y this) :x (.x this) :y (.y this))))
    (valAt
     [this k]
     (clojure.core/case k 0 (.x this) 1 (.y this) :x (.x this) :y (.y this)))
    (valAt
     [this k o]
     (clojure.core/case k 0 (.x this) 1 (.y this) :x (.x this) :y (.y this) o))
    (iterator [this] (.iterator {:x (.x this), :y (.y this)}))
    (forEach [this action] (action (.x this)) (action (.y this)))))


(cons :test (Vector2. 1.0 2.0)) => (:test 1.0 2.0)
(map #(+ 2 %) (Vector2. 1.0 2.0)) => (3.0 4.0)
(assoc (Vector2. 1.0 2.0) :thats :nice) => {:x 1.0, :y 2.0, :thats :nice}
(dissoc (Vector2. 1.0 2.0) :x) => {:y 2.0}
(dissoc (Vector2. 1.0 2.0) 0) => {:y 2.0}

   (defmethod clojure.pprint/simple-dispatch Vector2 [obj] (clojure.pprint/simple-dispatch (into {} obj)))

   (defn len [x]
     (if (vector? x)
       (.size x)
       (.length x)))

   (defn len2 [x]
     (if (vector? x)
       (.size ^clojure.lang.IPersistentVector x)
       (.length ^java.lang.String x)))

   (defn len3 [x]
     (cond
       (vector? x) (.size ^clojure.lang.IPersistentVector x)
       (string? x) (.length ^java.lang.String x)))

   (defn len4 [x]
     (condp = (type x)
       clojure.lang.PersistentVector (.size ^clojure.lang.IPersistentVector x)
       java.lang.String (.length ^java.lang.String x)))

   (defprotocol proto-len5 (len5 [x]))
   (extend-protocol proto-len5
     clojure.lang.IPersistentVector (len5 [x] (.size ^clojure.lang.IPersistentVector x))
     java.lang.String (len5 [x] (.length ^java.lang.String x)))

   (defn vecsize [x]
     (.size ^clojure.lang.IPersistentVector x))
   (defn strlen [x]
     (.length ^java.lang.String x))

   (let [testvec [57 856 25  856 25 "hallo" "test" 0.4 1.9 2]
         teststring "ergdrgnsdng"
         n 64000000
         testcoll_vec (vec (repeat (int (/ n 16)) testvec))
         testcoll_str (vec (repeat (* 15 (int (/ n 16))) teststring))
         testcoll (vec (interleave testcoll_vec testcoll_str))
         _ (println (.repeat "-" 80))
         _ (print "t1: ")
         t1 (time (reduce + (map len  testcoll)))
         _ (print "t2: ")
         t2 (time (reduce + (map len2 testcoll)))
         _ (print "t3: ")
         t3 (time (reduce + (map len3 testcoll)))
         _ (print "t4: ")
         t4 (time (reduce + (map len4 testcoll)))
         _ (print "t5: ")
         t5 (time (reduce + (map len5 testcoll)))
         _ (print "t5_mono_vec: ")
         t5_mono_vec (time (reduce + (map len5 testcoll_vec)))
         _ (print "t5_mono_str: ")
         t5_mono_str (time (reduce + (map len5 testcoll_str)))
         _ (print "vecsize time: ")
         vecsize_t (time (reduce + (map vecsize testcoll_vec)))
         _ (print "strlen time: ")
         strlen_t (time (reduce + (map strlen testcoll_str)))
         _ (println (.repeat "-" 80))
         ])
   (float (/ (* 1000000 (- 590 544) ) (* 15 (/ 64000000 16)) ))

   (len4 "aifuhsrigh")
   (len4 [3 4 76 8 ])

   (len3 "aifuhsrigh")

   (len5 "aifuhsrigh")

  )


