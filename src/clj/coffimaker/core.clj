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

(defn- copy-resource-to [resource target-dir]
  (let [target-file (rfs/file target-dir resource)]
    (io/copy (io/input-stream (io/resource resource)) target-file)
    target-file))

(comment

  (println (slurp (io/resource "coffimaker.zig")))

  (let [tmp (rfs/temp-dir "coffimaker")
        _ (copy-resource-to "coffimaker.zig" tmp)
        build-file (copy-resource-to "build.zig" tmp)
        ]
    (->
     "c:/Users/Kristin/repos/zigclj/raylib.h"
     (z/translate-c-header!
      {:compile-error-replacements {"RL_MALLOC"  "\n"
                                    "RL_CALLOC"  "\n"
                                    "RL_REALLOC" "\n"
                                    "RL_FREE"    "\n"}})
     (z/post-process-header-translation)
     (z/zig :build (.getAbsolutePath ^java.io.File build-file) :run)
     )
    ;(.getAbsolutePath ^java.io.File build-file)

    )

  (z/translate-c-header!)
  z/current-platform

  (z/extract-zig-from-resources!)
  (z/prepare-zig!)

  (z/zig-command)

  (+ 1 2)
  ;NOTE: zigclj require runtime error: core.clj at (76:8): Keyword cannot be cast CharSequence

  )


