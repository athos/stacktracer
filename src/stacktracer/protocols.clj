(ns stacktracer.protocols
  (:refer-clojure :exclude [ex-message newline print]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-color-type [this color-type f]))

(defprotocol IRenderer
  (render-start [this e])
  (render-trace [this elems contents])
  (render-trace-element [this elem content])
  (render-end [this e]))

(defprotocol IStacktrace
  (ex-message [this])
  (ex-trace [this]))
