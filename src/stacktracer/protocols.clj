(ns stacktracer.protocols
  (:refer-clojure :exclude [print newline]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-alert [this alert f]))

(defprotocol IRenderer
  (render-start [this e])
  (render-content [this info content])
  (render-end [this e]))
