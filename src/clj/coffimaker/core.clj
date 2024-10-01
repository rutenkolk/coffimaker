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

(defn write-bytes
  "Writes a [[byte]] array to the `segment`, at an optional `offset`."
  {:inline
   (fn write-byte-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy value# 0  ^MemorySegment segment# ^ValueLayout$OfByte mem/byte-layout 0 ^int (alength value#))))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfByte mem/byte-layout offset# ^int (alength value#)))))}
  ([^MemorySegment segment ^bytes value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfByte mem/byte-layout 0 (alength value)))
  ([^MemorySegment segment offset ^bytes value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfByte mem/byte-layout ^long offset ^int (alength value))))

(defn write-shorts
  "Writes a [[short]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-shorts-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfShort mem/short-layout 0 ^int (alength value#))))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfShort mem/short-layout ^long offset ^int (alength value#))))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (MemorySegment/copy value# 0 segment# (.withOrder ^ValueLayout$OfShort mem/short-layout ^ByteOrder byte-order#) ^long offset ^int (alength value#)))))}
  ([^MemorySegment segment ^shorts value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfShort mem/short-layout 0 (alength value)))
  ([^MemorySegment segment ^long offset ^shorts value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfShort mem/short-layout ^long offset (alength value)))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order ^shorts value]
   (MemorySegment/copy value 0 segment (.withOrder ^ValueLayout$OfShort mem/short-layout byte-order) ^long offset (alength value))))

(defn write-ints
  "Writes a [[int]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-ints-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfInt mem/int-layout 0 ^int (alength value#))
         ))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfInt mem/int-layout ^long offset ^int (alength value#))
         ))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (MemorySegment/copy value# 0 segment# (.withOrder ^ValueLayout$OfInt mem/int-layout ^ByteOrder byte-order#) ^long offset ^int (alength value#)))))}
  ([^MemorySegment segment ^ints value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfInt mem/int-layout 0 (alength value)))
  ([^MemorySegment segment ^long offset ^ints value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfInt mem/int-layout ^long offset (alength value)))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order ^ints value]
   (MemorySegment/copy value 0 segment (.withOrder ^ValueLayout$OfInt mem/int-layout byte-order) ^long offset (alength value))))

(defn write-longs
  "Writes a [[long]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-longs-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfLong mem/long-layout 0 ^int (alength value#))
         ))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfLong mem/long-layout ^long offset ^int (alength value#))
         ))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# (.withOrder ^ValueLayout$OfLong mem/long-layout ^ByteOrder byte-order#) ^long offset ^int (alength value#)))))}
  ([^MemorySegment segment ^longs value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfLong mem/long-layout 0 ^int (alength value)))
  ([^MemorySegment segment ^long offset ^longs value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfLong mem/long-layout ^long offset ^int (alength value)))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order ^longs value]
   (MemorySegment/copy value 0 segment (.withOrder ^ValueLayout$OfLong mem/long-layout byte-order) ^long offset ^int (alength value))))


(defn write-chars
  "Writes a [[char]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-chars-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy (bytes (byte-array (map unchecked-int value#))) 0 segment# ^ValueLayout$OfChar mem/char-layout 0 ^int (alength value#))))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy (bytes (byte-array (map unchecked-int value#))) 0 segment# ^ValueLayout$OfChar mem/char-layout ^long offset ^int (alength value#))))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (MemorySegment/copy (bytes (byte-array (map unchecked-int value#))) 0 segment# (.withOrder ^ValueLayout$OfChar mem/char-layout ^ByteOrder byte-order#) ^long offset ^int (alength value#)))))}
  ([^MemorySegment segment ^chars value]
   (MemorySegment/copy (bytes (byte-array (map unchecked-int value))) 0 segment ^ValueLayout$OfChar mem/char-layout 0 (alength value)))
  ([^MemorySegment segment ^long offset ^chars value]
   (MemorySegment/copy (bytes (byte-array (map unchecked-int value))) 0 segment ^ValueLayout$OfChar mem/char-layout ^long offset (alength value)))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order ^chars value]
   (MemorySegment/copy (bytes (byte-array (map unchecked-int value))) 0 segment (.withOrder ^ValueLayout$OfChar mem/char-layout byte-order) ^long offset (alength value))))

(defn write-floats
  "Writes a [[float]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-floats-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfFloat mem/float-layout 0 ^int (alength value#))
         ))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfFloat mem/float-layout ^long offset ^int (alength value#))
         ))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# (.withOrder ^ValueLayout$OfFloat mem/float-layout ^ByteOrder byte-order#) ^long offset ^int (alength value#)))))}
  ([^MemorySegment segment ^floats value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfFloat mem/float-layout 0 ^int (alength value)))
  ([^MemorySegment segment ^long offset ^floats value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfFloat mem/float-layout ^long offset ^int (alength value)))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order ^floats value]
   (MemorySegment/copy value 0 segment (.withOrder ^ValueLayout$OfFloat mem/float-layout byte-order) ^long offset ^int (alength value))))

(defn write-doubles
  "Writes a [[double]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-doubles-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfDouble mem/double-layout 0 ^int (alength value#))
         ))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfDouble mem/double-layout ^long offset ^int (alength value#))
         ))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (MemorySegment/copy value# 0 ^MemorySegment segment# (.withOrder ^ValueLayout$OfDouble mem/double-layout ^ByteOrder byte-order#) ^long offset ^int (alength value#)))))}
  ([^MemorySegment segment ^doubles value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfDouble mem/double-layout 0 ^int (alength value)))
  ([^MemorySegment segment ^long offset ^doubles value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfDouble mem/double-layout ^long offset ^int (alength value)))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order ^doubles value]
   (MemorySegment/copy value 0 segment (.withOrder ^ValueLayout$OfDouble mem/double-layout byte-order) ^long offset ^int (alength value))))

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
    (= :array (first t))              [::mem/array (typename-conversion (second t)) (nth t 2)]))

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

(defn- gen-opaque [types]
  (->>
   types
   (:opaque)
   (map (fn [v] (list `mem/defalias (symbol (name (:name v))) :coffi.mem/byte)))))


(defn gen-struct-type [typename fields]
  (let [metabinds (->>
                   fields
                   (filter #(not= ::layout/padding (first %)))
                   (map (fn [[fieldname coffitype]] [(symbol (name fieldname)) (coffitype->class coffitype)]))
                   (apply concat)
                   (vec))
        as-vec (vec (partition 2 (interleave (map (comp symbol (partial str ".") name first) fields) (repeat 'this) )))
        as-map (into {} (map (fn [f] [(first f) (list (->> f (first) (name) (str ".") (symbol)) 'this)]) fields))
        ]
    (list 'with-typehint metabinds
     (list `deftype (symbol (name typename))
           (vec (map (comp symbol name first) fields))
     clojure.lang.IPersistentVector
     clojure.lang.IPersistentMap
     (list 'length      ['this]           (count fields))
     (list 'assocN      ['this 'i 'value] (list `assoc 'i as-vec 'value))
     (list 'cons        ['this 'o]        (vec (cons 'o as-vec)))
     (list 'peek        ['this]           (first as-vec))
     (list 'pop         ['this]           (vec (rest as-vec)))
     (list 'count       ['this]           (count fields))
     (list 'empty       ['this]           [])
     (list 'equiv       ['this 'o]        (list `or (list `=  as-vec 'o) (list `= as-map 'o)))
     (list 'seq         ['this]           (list `seq as-vec))
     (list 'rseq        ['this]           (vec (reverse as-vec)))
     (list 'nth         ['this 'i]        (concat [`case 'i] (interleave (range) as-vec)))
     (list 'nth         ['this 'i 'o]     (concat [`case 'i] (interleave (range) as-vec) ['o]))

     (list 'assoc       ['this 'i 'value] (list `if (list `number? 'i) (list `assoc as-vec 'i 'value) (assoc as-map 'i 'value)))
     (list 'assocEx     ['this 'i 'value] (list `if (list (set (map first fields)) 'i) (list `throw (list `Exception. "key already exists")) (assoc as-map 'i 'value)))
     (list 'without     ['this 'k]        (list `dissoc as-map (list `if (list `number? 'k) (list (vec (map first fields)) 'k) 'k)))
     (list 'containsKey ['this 'k]    (list `if (list `number? 'k) (list `and (list `>= 'k 0) (list `< 'k (count fields))) (list (set (map first fields)) 'k)))
     (list 'entryAt     ['this 'k]    (list `clojure.lang.MapEntry/create 'k (concat [`case 'k] (interleave (range) as-vec) (interleave (map first fields) as-vec))))

     (list 'valAt       ['this 'k]    (concat [`case 'k] (interleave (range) as-vec) (interleave (map first fields) as-vec)))
     (list 'valAt       ['this 'k 'o] (concat [`case 'k] (interleave (range) as-vec) (interleave (map first fields) as-vec) ['o]))
     (list 'iterator    ['this]       (list '.iterator as-map))
     (concat ['forEach  ['this 'action]]  (partition 2 (interleave (repeat 'action) as-vec)))
     )

     )))


(defn gen-serialize-into-single [obj-form coffitype offset]
  (if (vector? coffitype)
    (cond
      (= :coffi.mem/array (first coffitype))
      (concat
       (list `let ['array-obj obj-form])
       (map
        (fn [index]
          (gen-serialize-into-single (list 'vector-nth 'array-obj index) (second coffitype) (+ offset (* (mem/size-of (second coffitype)) index))))
        (range (second (rest coffitype))))))
    (condp = coffitype
      :coffi.mem/byte    (list `mem/write-byte    'segment offset obj-form)
      :coffi.mem/short   (list `mem/write-short   'segment offset obj-form)
      :coffi.mem/int     (list `mem/write-int     'segment offset obj-form)
      :coffi.mem/long    (list `mem/write-long    'segment offset obj-form)
      :coffi.mem/char    (list `mem/write-char    'segment offset obj-form)
      :coffi.mem/float   (list `mem/write-float   'segment offset obj-form)
      :coffi.mem/double  (list `mem/write-double  'segment offset obj-form)
      :coffi.mem/pointer (list `mem/write-address 'segment offset obj-form)
      (list (symbol (str "serialize-" (name coffitype)))
            obj-form
            (list `mem/slice 'segment offset (mem/size-of coffitype))))))


(defn gen-serialize-into [typename [_struct fields]]
  (let [protocol-name (symbol (str "proto-serialize-" (name typename)))
        protocol-fn (symbol (str "serialize-" (name typename)))
        typelist (->>
                  (partition 2 2 (interleave (reductions + 0 (map (comp mem/size-of second) fields)) fields))
                  (filter (fn [[_ [_ field-type]]] (not (and (vector? field-type) (= :coffi.mem/padding (first field-type)))))))
        ]
    (list
     `(defprotocol ~protocol-name (~protocol-fn ~['obj 'segment]))
     (list `extend-protocol protocol-name
           clojure.lang.IPersistentVector
           (list protocol-fn ['obj 'segment]
                 (->>
                  typelist
                  (map-indexed
                   (fn [index [offset [_ field-type]]]
                     (gen-serialize-into-single
                      (list 'vector-nth 'obj (list `int index)) field-type offset)))
                  (cons `do)))
           clojure.lang.IPersistentMap
           (list protocol-fn ['obj 'segment]
                 (->>
                  typelist
                  (map
                   (fn [[offset [field-name field-type]]]
                     (gen-serialize-into-single
                      (list field-name 'obj) field-type offset)))
                  (cons `do)))
           (symbol (name typename))
           (list protocol-fn ['obj 'segment]
                 (->>
                  typelist
                  (map
                   (fn [[offset [field-name field-type]]]
                     (gen-serialize-into-single
                      (list (symbol (str "." (name field-name))) 'obj) field-type offset)))
                  (cons `do)))

           )
     `(defmethod mem/serialize-into ~typename
           ~['obj '_struct 'segment '_session]
           (~protocol-fn ~'obj ~'segment)))))

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

(defmulti gen-deserialize-from-single (fn [& xs] (if (vector? (first xs)) (first (first xs)) (first xs))))

(defmethod gen-deserialize-from-single :coffi.mem/byte     [_type offset] [`(mem/read-byte    ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/short    [_type offset] [`(mem/read-short   ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/int      [_type offset] [`(mem/read-int     ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/long     [_type offset] [`(mem/read-long    ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/char     [_type offset] [`(mem/read-char    ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/float    [_type offset] [`(mem/read-float   ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/double   [_type offset] [`(mem/read-double  ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/pointer  [_type offset] [`(mem/read-address ~'segment ~offset)])
(defmethod gen-deserialize-from-single :coffi.mem/c-string [_type offset] [`(list 'with-typehint ['addr java.lang.foreign.MemorySegment] (list `.getString (list `.reinterpret 'addr `Integer/MAX_VALUE) 0))])
(defmethod gen-deserialize-from-single :coffi.mem/array    [_type offset]
  (let [outer-code `(let [arr# (~(coffitype-to-array-fn (second _type)) ~(second (rest _type)))] arr# )
        gen-arr (nth outer-code 2)
        ]
    [(concat (butlast outer-code)
             [`aset gen-arr]
             (reduce concat (map (fn [index] (let [deserialize-instructions
                                     (gen-deserialize-from-single (second _type) (+ offset (* (mem/size-of (second _type)) index)))]
                                               (if (vector? deserialize-instructions)
                                                 (list index (first deserialize-instructions))
                                                 (list index deserialize-instructions))))
                   (range (second (rest _type)))))
             [gen-arr]
             )]))

(defn gen-deserialize-from [typename [_struct fields]]
  (let [typelist (->>
                  (partition 2 2 (interleave (reductions + 0 (map (comp mem/size-of second) fields)) fields))
                  (filter (fn [[_ [_ field-type]]] (not (and (vector? field-type) (= :coffi.mem/padding (first field-type)))))))]
    (defmethod gen-deserialize-from-single typename [_type global-offset]
      (->> typelist
           (map-indexed
            (fn [index [offset [_ field-type]]]
              (gen-deserialize-from-single field-type (+ global-offset offset))))
           (reduce concat)
           (cons (symbol (str (name typename) ".")))
           )

      ))
  [`(defmethod mem/deserialize-from ~typename ~['segment '_type]
      ~(cons (symbol (str (name typename) ".")) (gen-deserialize-from-single typename 0)))]
  )

(defn- gen-struct-types [typename fields]
  (list 'do-with-meta (interleave (symbol (name (map first fields))) ) `defrecord)
  )

(defn- gen-structs [types]
  (->>
   types
   (:struct)
   (map (fn [v]
     (let [struct-layout (layout/with-c-layout [::mem/struct (map typed-decl (:members v))])]
       (eval (list `mem/defalias (:name v) struct-layout))
       (concat
        [(gen-struct-type (:name v) (second struct-layout))
         (list `defmethod `clojure.pprint/simple-dispatch 'Vector2 ['obj] (list `clojure.pprint/simple-dispatch (list `into {} 'obj)))
         (list `mem/defalias (:name v) struct-layout)
         `(defmethod mem/c-layout ~(:name v) [~'_type] ~struct-layout)

         ]
        (gen-serialize-into (:name v) struct-layout)
        (gen-deserialize-from (:name v) struct-layout)
        ))))
   (reduce concat)
   ))


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
   (group-by :kind)
   (gen-structs)
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


