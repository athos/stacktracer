(ns stacktracer.fallback
  (:require [clojure.repl :as repl]
            [stacktracer.protocols :as proto])
  (:import [clojure.lang AFunction]))

(extend-protocol proto/IFallback
  nil
  (fallback [_ _ stacktracer-error]
    (throw stacktracer-error))
  AFunction
  (fallback [this original-error _]
    (this original-error)))

(defn default-fallback-fn [e]
  (binding [*out* *err*]
    (println "[ERROR] Stacktracer failed to process the exception. Falls back to clojure.repl/pst."))
  (repl/pst e))
