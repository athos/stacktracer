(ns stacktracer.protocols
  (:refer-clojure :exclude [print newline ex-message]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-color-type [this color-type f]))

(defprotocol IRenderer
  (render-start [this e])
  (render-content [this info content])
  (render-end [this e]))

(defprotocol IStacktrace
  (ex-message [this])
  (ex-trace [this]))
