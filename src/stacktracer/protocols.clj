(ns stacktracer.protocols
  (:refer-clojure :exclude [print newline]))

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
  (ex-message-lines [this])
  (ex-trace [this]))
