(ns stacktracer.api
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [stacktracer.repl :as st])
  (:import [java.io PushbackReader]))

(defn convert-from-edn [{:keys [path] :as opts}]
  (let [edn (if path
              (with-open [r (PushbackReader. (io/reader (str path)))]
                (edn/read r))
              (edn/read *in*))
        args (apply concat opts)]
    (apply st/pst-for (:clojure.main/trace edn) args)))
