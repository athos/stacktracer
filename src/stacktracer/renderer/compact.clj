(ns stacktracer.renderer.compact
  (:require [clojure.string :as str]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer.common :as common]))

(defn- render-trace-elements [{:keys [printer]} elems]
  (let [idigits (count (str (apply max (map :id elems))))
        ldigits (count (str (apply max (map :line elems))))
        file-max-len (apply max (map (comp count :file) elems))]
    (doseq [elem elems]
      (proto/with-color-type printer :info
        #(common/printf printer "[%s/%d] %s:%s| "
                        (common/pad idigits (:id elem))
                        (:total elem)
                        (common/pad file-max-len (:file elem))
                        (common/pad ldigits (:line elem))))
      (doto printer
        (proto/print (str/trim (:focused elem)))
        (proto/newline)))))

(defrecord CompactRenderer [printer opts]
  proto/IRenderer
  (render-trace [this e elems]
    (when (:show-messages opts)
      (if (:reverse opts)
        (proto/with-color-type printer :info
          #(doto printer
             (proto/print "Traceback (most recent call last):")
             (proto/newline)))
        (common/render-error-message printer e)))
    (when (seq elems)
      (render-trace-elements this elems))
    (when (and (:show-messages opts) (:reverse opts))
      (common/render-error-message printer e))))
