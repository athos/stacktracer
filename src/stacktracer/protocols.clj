(ns stacktracer.protocols
  (:refer-clojure :exclude [ex-cause ex-message newline print]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-color-type [this color-type f]))

(defprotocol IRenderer
  (render-start [this e])
  (render-trace [this elems])
  (render-trace-element [this elem])
  (render-end [this e]))

(defprotocol IStacktrace
  (ex-message [this])
  (ex-trace [this])
  (ex-cause [this]))
