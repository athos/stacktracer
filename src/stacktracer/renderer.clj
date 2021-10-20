(ns stacktracer.renderer
  (:require [stacktracer.printer :as printer]))

(defmulti make-renderer (fn [opts] (:render opts)))

(defmethod make-renderer :default [opts]
  (if-let [renderer (:render opts)]
    renderer
    (let [opts' (assoc opts :render (if (:color opts) :color :monochrome))]
      (make-renderer opts'))))

(defmethod make-renderer :color [opts]
  (printer/make-print-renderer (printer/->AsciiColorPrinter) opts))

(defmethod make-renderer :monochrome [opts]
  (printer/make-print-renderer (printer/->MonochromePrinter) opts))
