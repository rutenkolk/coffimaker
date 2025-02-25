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
   (java.nio ByteOrder)))

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
        translate-result (z/translate-c-header! header opts)
        ]
    (if (not (string? translate-result))
      translate-result
      (let [_ (spit translated-file (z/post-process-header-translation translate-result))
            build-output (with-sh-dir (str tmp) (z/zig :build :run))]
        (if (= 0 (:exit build-output))
          (edn/read-string (:err build-output))
          build-output)))))

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
(defn- ui8     [name] [name ::mem/byte])
(defn- ui16    [name] [name ::mem/short])
(defn- ui32    [name] [name ::mem/int])
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
   :ui8            ::mem/byte
   :ui16           ::mem/short
   :ui32           ::mem/int
   :u8             ::mem/byte
   :u16            ::mem/short
   :u32            ::mem/int
   :bool           ::mem/boolean
   [:pointer :u8]  ::mem/c-string
   :pointer        ::mem/pointer
   :void-pointer   ::mem/pointer
   :void           ::mem/void})

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
    ))

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
  (vec (reduce concat (map (fn [[type name]] [(typename-conversion type) (symbol (clojure.core/name name))]) l))))

(defn gen-struct [{:keys [name members] :as v}]
  `(mem/defstruct ~(symbol (clojure.core/name name)) ~(vec (reduce concat (map (fn [[type name]] [(symbol (clojure.core/name name)) (typename-conversion type)]) members)))))

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

(defn gen-fn [{:keys [name params return-type]}]
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
       ~(typename-conversion return-type))))

(defn generate-from-header-info [header-info]
  (->>
   header-info
   (map #(case (:type %)
           :type     (gen-type %)
           :constant (gen-constant %)
           :fn       (gen-fn %)))
   (filter identity)
   (doall)))

(comment
  (def raylib-header-info
    (c-header-info
     "../raylib/src/raylib.h"
     {:compile-error-replacements {"RL_MALLOC"  "\n"
                                   "RL_CALLOC"  "\n"
                                   "RL_REALLOC" "\n"
                                   "RL_FREE"    "\n"}}))

  (set! *print-meta* true)

  (ffi/load-library "raylib.dll")

  (->>
   raylib-header-info
   (generate-from-header-info)
   (doall-with-coffi-ns 'raylib))

  (do
    (raylib/InitWindow 800 450 "raylib-clj [core] example - basic window")
    (raylib/SetTargetFPS 10000)
    (while (not (raylib/WindowShouldClose))
      (let [[last-time acc] @state
            newtime (System/nanoTime)
            diff (- newtime last-time)
            newacc (vec (take-last 500 (conj acc diff)))
            average-diff (/ (reduce + newacc) (count newacc))
            average-fps (long (/ 1000000000 average-diff))]
        (reset! state [newtime newacc])
        (raylib/BeginDrawing)
        (raylib/ClearBackground raylib/RAYWHITE)
        (raylib/DrawText "Congrats! You created your first raylib window!" 190 200 20 raylib/BLACK)
        (raylib/DrawText "And you did it from clojure!" (int (+ 190 (rand 5))) 240 20 raylib/DARKBLUE)
        (raylib/DrawText (str "fps: " average-fps ) 190 380 20 raylib/BLACK)
        (raylib/EndDrawing)))
    (raylib/CloseWindow))

  )


