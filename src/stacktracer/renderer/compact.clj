(ns stacktracer.renderer.compact
  (:require [clojure.string :as str]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer.common :as common]))

(defn- render-trace-element [{:keys [printer]} elem]
  (proto/with-color-type printer :info
    #(common/printf printer "[%d/%d] %s:%d "
                    (:id elem) (:total elem) (:file elem) (:line elem)))
  (doto printer
    (proto/print (str/trim (:focused elem)))
    (proto/newline)))

(defrecord CompactRenderer [printer opts]
  proto/IRenderer
  (render-trace [this e elems]
    (when (and (:show-message opts) (not (:reverse opts)))
      (common/render-error-message printer e))
    (doseq [elem elems]
      (render-trace-element this elem))
    (when (and (:show-message opts) (:reverse opts))
      (common/render-error-message printer e))))
