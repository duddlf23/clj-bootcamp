(ns util.file
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn read-edn [file-name]
  (-> (io/resource file-name)
      slurp
      edn/read-string))