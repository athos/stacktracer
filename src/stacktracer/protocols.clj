(ns stacktracer.protocols
  (:refer-clojure :exclude [ex-cause ex-data ex-message flush newline print]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-color-type [this color-type f])
  (flush [this]))

(defprotocol IRenderer
  (start-rendering [this])
  (render-trace [this e elems])
  (end-rendering [this]))

(defprotocol IStacktrace
  (ex-message [this])
  (ex-data [this])
  (ex-trace [this])
  (ex-cause [this])
  (wrapped? [this]))

(defprotocol IFallback
  (fallback [this original-error stacktracer-error]))
