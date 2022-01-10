(ns stacktracer.utils
  (:import [java.util.regex Pattern])
  (:require [clojure.string :as str]))

(def ^:private ^String file-separator
  (System/getProperty "file.separator"))

(def ^:private file-separator-re
  (Pattern/compile (Pattern/quote file-separator)))

(defn file-basename [path]
  (-> path (str/split file-separator-re) last))

(defn path->ns-name [path]
  (-> path
      (str/replace #"\.(?:clj|cljc)$" "")
      (str/replace file-separator ".")))

(defn ns-name->path [ns-name ext]
  (-> (munge ns-name)
      (str/replace "." file-separator)
      (str ext)))
