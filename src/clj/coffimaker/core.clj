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
    MemoryLayout
    MemorySegment
    SegmentAllocator)
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
   :void-pointer   ::mem/pointer})

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

(defn- gen-structs [types]
  (->>
   types
   (:struct)
   (map (fn [v] (list
     `mem/defalias
     (:name v)
     (list `layout/with-c-layout [::mem/struct (map typed-decl (:members v))]))))))


(defn gen-serialize-into-single [obj-form coffitype offset]
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
          (list `mem/slice 'segment offset (mem/size-of coffitype)))))

(defn gen-serialize-into [typename [_struct fields]]
  (let [protocol-name (symbol (str "proto-serialize-" (name typename)))
        protocol-fn (symbol (str "serialize-" (name typename)))
        typelist (->>
                  (partition 2 2 (interleave (reductions + 0 (map (comp mem/size-of second) fields)) fields))
                  (filter (fn [[_ [_ field-type]]] (not (and (vector? field-type) (= :coffi.mem/padding (first field-type)))))))
        ]
    (list
     `do
     (list `defprotocol protocol-name (list protocol-fn ['obj 'segment]))
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
                  (cons `do))))
     (list `defmethod `serialize-into typename
           ['obj '_struct 'segment '_session]
           (list protocol-fn 'obj 'segment)))))

(comment
  (def raylib-header-info
    (c-header-info
     "../raylib/src/raylib.h"
     {:compile-error-replacements {"RL_MALLOC"  "\n"
                                   "RL_CALLOC"  "\n"
                                   "RL_REALLOC" "\n"
                                   "RL_FREE"    "\n"}}))

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
                            [:mipmaps :coffi.mem/int]
                            [:format :coffi.mem/float])])
                       )

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


