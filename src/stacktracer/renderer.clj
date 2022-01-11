(ns stacktracer.renderer
  (:require [stacktracer.printer :as printer]
            [stacktracer.renderer.compact :as compact]
            [stacktracer.renderer.pretty :as pretty]))

(defmulti make-renderer (fn [opts] (:format opts)))

(defmethod make-renderer :default [opts]
  (if-let [renderer (:format opts)]
    renderer
    (make-renderer (assoc opts :format :pretty))))

(defmethod make-renderer :pretty [opts]
  (pretty/make-pretty-renderer (printer/make-printer opts) opts))

(defmethod make-renderer :compact [opts]
  (compact/make-compact-renderer (printer/make-printer opts) opts))
