(ns stacktracer.errors
  (:require [clojure.main :as main]
            [stacktracer.protocols :as proto]
            [stacktracer.utils :as utils]))

(defn ex-data->compiler-exception-trace [{:clojure.error/keys [source line]}]
  (let [nsname (utils/path->ns-name source)
        filename (utils/file-basename source)]
    [[(symbol (str nsname \$ "<toplevel>")) '<none> filename line]]))

(defrecord Wrapped [e]
  proto/IStacktrace
  (ex-message [_]
    (proto/ex-message e))
  (ex-trace [_]
    (proto/ex-trace e))
  (ex-cause [_]
    (when-let [cause (proto/ex-cause e)]
      (Wrapped. cause)))
  (wrapped? [_] true))

(extend-protocol proto/IStacktrace
  Object
  (ex-message [this]
    (when (and (map? this) (:trace this))
      (main/ex-str (main/ex-triage this))))
  (ex-trace [this]
    (when (and (map? this) (:trace this))
      (:trace this)))
  (ex-cause [_])
  (wrapped? [_] false)
  Throwable
  (ex-message [this]
    (main/err->msg this))
  (ex-trace [this]
    (:trace (Throwable->map this)))
  (ex-cause [_])
  (wrapped? [_] false)
  clojure.lang.Compiler$CompilerException
  (ex-message [this]
    (-> (Throwable->map this)
        main/ex-triage
        main/ex-str))
  (ex-trace [this]
    (ex-data->compiler-exception-trace (ex-data this)))
  (ex-cause [this]
    (when (= (:clojure.error/phase (ex-data this)) :macroexpansion)
      (some-> (ex-cause this) ->Wrapped)))
  (wrapped? [_] false))
