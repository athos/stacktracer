(ns stacktracer.renderer.pretty
  (:require [stacktracer.protocols :as proto]
            [stacktracer.renderer.common :as common]))

(defn- render-trace-element [{:keys [printer]} elem]
  (let [{:keys [line before focused after]} elem
        ndigits (count (str (+ line (count after))))
        pad #(common/pad ndigits %)]
    (proto/with-color-type printer :info
      #(doto printer
         (common/printf "   ---- [%d/%d] %s (%s:%d) ----"
                 (:id elem) (:total elem) (common/compact-fn-name (:fn elem))
                 (:file elem) line)
         (proto/newline)))
    (doseq [[i text] (map-indexed vector before)
            :let [i' (- line (count before) (- i))]]
      (doto printer
        (common/printf "   %s| %s" (pad i') text)
        (proto/newline)))
    (proto/with-color-type printer :error
      #(doto printer
         (common/printf "=> %s| %s" (pad line) focused)
         (proto/newline)))
    (proto/with-color-type printer :error
      #(if-let [i (->> focused
                       (map-indexed vector)
                       (drop-while (fn [[_ c]] (Character/isWhitespace (char c))))
                       (ffirst))]
         (doto printer
           (common/printf "   %s|%s%s" (pad "") (common/times (inc i) \space)
                   (common/times (- (count focused) i) \^))
           (proto/newline))
         (doto printer
           (common/printf "   %s| ^^" (pad ""))
           (proto/newline))))
    (doseq [[i text] (map-indexed vector after)]
      (doto printer
        (common/printf "   %s| %s" (pad (+ line i 1)) text)
        (proto/newline)))))

(defrecord PrettyRenderer [printer opts]
  proto/IRenderer
  (start-rendering [_]
    (when (and (:show-messages opts) (:reverse opts))
      (proto/with-color-type printer :info
        #(doto printer
           (proto/print "Traceback (most recent call last):")
           (proto/newline)))))
  (render-trace [this e elems]
    (when (:show-messages opts)
      (when-not (:reverse opts)
        (common/render-error-message printer e))
      (proto/newline printer))
    (if (seq elems)
      (doseq [elem elems]
        (render-trace-element this elem)
        (proto/newline printer))
      (when (:show-messages opts)
        (doto printer
          (proto/print "   << No stack trace available for this throwable >>")
          (proto/newline)
          (proto/newline))))
    (when (and (:show-messages opts) (:reverse opts))
      (common/render-error-message printer e)))
  (end-rendering [_]
    (proto/flush printer)))

(defn make-pretty-renderer [printer opts]
  (->PrettyRenderer printer opts))
