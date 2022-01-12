(ns stacktracer.protocols
  (:refer-clojure :exclude [ex-cause ex-message newline print]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-color-type [this color-type f]))

(defprotocol IRenderer
  (start-rendering [this])
  (render-trace [this e elems])
  (end-rendering [this]))

(defprotocol IStacktrace
  (ex-message [this])
  (ex-trace [this])
  (ex-cause [this])
  (wrapped? [this]))

(defprotocol IFallback
  (fallback [this original-error stacktracer-error]))
