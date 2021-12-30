(ns stacktracer.renderer.pretty
  (:refer-clojure :exclude [printf])
  (:require [stacktracer.protocols :as proto]
            [stacktracer.renderer.common :as common]))

(defrecord PrettyRenderer [printer opts]
  proto/IRenderer
  (render-trace [this e elems]
    (when (and (:show-message opts) (not (:reverse opts)))
      (common/render-error-message printer e)
      (when (seq elems)
        (proto/newline printer)))
    (let [len (count elems)
          elems (cond-> elems (:reverse opts) reverse)]
      (doseq [[i elem] (map-indexed vector elems)]
        (proto/render-trace-element this e elem)
        (when (< i (dec len))
          (proto/newline printer))))
    (when (and (:show-message opts) (:reverse opts))
      (when (seq elems)
        (proto/newline printer))
      (common/render-error-message printer e)))
  (render-trace-element [_ _ {:keys [line before focused after] :as elem}]
    (let [ndigits (count (str (+ line (count after))))
          pad #(common/pad ndigits %)]
      (proto/with-color-type printer :info
        #(doto printer
           (common/printf "   ---- [%d/%d] %s (%s:%d) ----"
                          (:id elem) (:total elem)
                          (common/compact-fn-name (:fn elem))
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
          (proto/newline))))))
