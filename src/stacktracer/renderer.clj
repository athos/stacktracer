(ns stacktracer.renderer
  (:require [stacktracer.printer :as printer]
            [stacktracer.renderer.pretty :as pretty]))

(defmulti make-renderer (fn [opts] (:format opts)))

(defmethod make-renderer :default [opts]
  (if-let [renderer (:format opts)]
    renderer
    (make-renderer (assoc opts :format :pretty))))

(defmethod make-renderer :pretty [opts]
  (let [printer (if (:color opts)
                  (printer/->AsciiColorPrinter)
                  (printer/->MonochromePrinter))]
    (pretty/->PrettyRender printer opts)))
