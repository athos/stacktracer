(ns stacktracer.renderer.pretty
  (:refer-clojure :exclude [printf])
  (:require [stacktracer.protocols :as proto]
            [stacktracer.renderer.common :as common]))

(defrecord PrettyRenderer [printer opts]
  proto/IRenderer
  (render-start [_ e]
    (when (and (:show-message opts)
               (not (:reverse opts)))
      (common/render-error-message printer e)))
  (render-trace [this elems contents]
    (when (and (:show-message opts)
                (not (:reverse opts))
                (seq elems))
      (proto/newline printer))
    (let [len (count elems)
          [elems contents] (if (:reverse opts)
                             [(reverse elems) (reverse contents)]
                             [elems contents])]
      (doseq [[i elem content] (map vector (range) elems contents)]
        (proto/render-trace-element this elem content)
        (when (< i (dec len))
          (proto/newline printer))))
    (when (and (:show-message opts)
               (:reverse opts)
               (seq elems))
      (proto/newline printer)))
  (render-trace-element [_ {fname :fn :keys [id total file line]} content]
    (let [{:keys [before focused after]} content
          ndigits (count (str (+ line (count after))))
          pad #(common/pad ndigits %)]
      (proto/with-color-type printer :info
        #(doto printer
           (common/printf "   ---- [%d/%d] %s (%s:%d) ----"
                   id total (common/compact-fn-name fname) file line)
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
  (render-end [_ e]
    (when (and (:show-message opts)
               (:reverse opts))
      (common/render-error-message printer e))))
