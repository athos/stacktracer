(ns stacktracer.errors
  (:require [clojure.main :as main]
            [stacktracer.protocols :as proto]
            [stacktracer.utils :as utils]))

(defn ex-data->compiler-exception-trace [{:clojure.error/keys [source line]}]
  (let [nsname (utils/path->ns-name source)
        filename (utils/file-basename source)]
    [[(symbol (str nsname \$ "<toplevel>")) '<none> filename line]]))

(extend-protocol proto/IStacktrace
  Object
  (ex-message [this]
    (when (and (map? this) (:trace this))
      (main/ex-str (main/ex-triage this))))
  (ex-trace [this]
    (when (and (map? this) (:trace this))
      (:trace this)))
  (ex-cause [_])
  Throwable
  (ex-message [this]
    (main/err->msg this))
  (ex-trace [this]
    (:trace (Throwable->map this)))
  (ex-cause [_])
  clojure.lang.Compiler$CompilerException
  (ex-message [this]
    (-> (Throwable->map this)
        main/ex-triage
        main/ex-str))
  (ex-trace [this]
    (ex-data->compiler-exception-trace (ex-data this)))
  (ex-cause [this]
    (when (= (:clojure.error/phase (ex-data this)) :macroexpansion)
      (ex-cause this))))
