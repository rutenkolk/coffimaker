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
   [clojure.edn :as edn])
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

(defn- copy-resource-to [resource target-dir]
  (let [target-file (rfs/file target-dir resource)]
    (io/copy (io/input-stream (io/resource resource)) target-file)
    target-file))

(defn c-header-info [header opts]
  (z/prepare-zig!)
  (let [header-name (rfs/name (io/file header))
        tmp (rfs/temp-dir "coffimaker")
        _ (copy-resource-to "coffimaker.zig" tmp)
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

(defn get-constant-defs [header-info]
  (->>
   header-info
   (:constant)
   (filter (comp not resolve symbol name :name))
   (filter #(not (#{:true :false} (:name %))))
   (map (fn [v] (list 'defconst (symbol (name (:name v))) (:value v))))))

(defn def-constants! [header-info]
  ((cons `do (get-constant-defs header-info))))

(comment
  (def raylib-header-info
    (c-header-info
     "../raylib/src/raylib.h"
     {:compile-error-replacements {"RL_MALLOC"  "\n"
                                   "RL_CALLOC"  "\n"
                                   "RL_REALLOC" "\n"
                                   "RL_FREE"    "\n"}}))
  ((comp not resolve symbol name) :mappp)

  (resolve (symbol "map"))

  (->>
   raylib-header-info
   (group-by :type)
   (:constant)
   )

  (->>
   raylib-header-info
   (group-by :type)
   (get-constant-defs)

       )


  )


