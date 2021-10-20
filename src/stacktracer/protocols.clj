(ns stacktracer.protocols)

(defprotocol IRenderer
  (render-start [this e])
  (render-content [this info content])
  (render-end [this e]))
