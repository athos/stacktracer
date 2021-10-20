(ns stacktracer.renderer
  (:require [stacktracer.print :as print]))

(defmulti make-renderer (fn [opts] (:render opts)))

(defmethod make-renderer :default [opts]
  (if-let [renderer (:render opts)]
    renderer
    (let [opts' (assoc opts :render (if (:color opts) :color :monochrome))]
      (make-renderer opts'))))

(defmethod make-renderer :color [opts]
  (print/make-print-renderer (print/->AsciiColorPrinter) opts))

(defmethod make-renderer :monochrome [opts]
  (print/make-print-renderer (print/->MonochromePrinter) opts))
