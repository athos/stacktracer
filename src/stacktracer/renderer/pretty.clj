(ns stacktracer.renderer.pretty
  (:refer-clojure :exclude [printf])
  (:require [clojure.string :as str]
            [stacktracer.protocols :as proto]))

(defn- printf [printer fmt & args]
  (proto/print printer (apply format fmt args)))

(defn- compact-fn-name [qualified-fname]
  (let [[_ nsname fname] (re-matches #"([^/]+)/(.*)" qualified-fname)
        names (str/split nsname #"\.")]
    (if (or (> (count names) 2)
            (> (count (first names)) 20))
      (with-out-str
        (doseq [name (butlast names)]
          (print (first name))
          (print \.))
        (print (last names))
        (print \/)
        (print fname))
      qualified-fname)))

(defn- times [n c]
  (with-out-str
    (dotimes [_ n]
      (print c))))

(defn- pad [n x]
  (let [text (str x), len (count text)]
    (with-out-str
      (print (times (- n len) \space))
      (print text))))

(defn- render-error-message [printer e]
  (proto/with-color-type printer :error
    #(doseq [line (proto/ex-message-lines e)]
       (doto printer
         (proto/print line)
         (proto/newline)))))

(defrecord PrettyRenderer [printer opts]
  proto/IRenderer
  (render-start [_ e]
    (when (and (:show-message opts)
               (not (:reverse opts)))
      (render-error-message printer e)))
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
          pad #(pad ndigits %)]
      (proto/with-color-type printer :info
        #(doto printer
           (printf "   ---- [%d/%d] %s (%s:%d) ----"
                   id total (compact-fn-name fname) file line)
           (proto/newline)))
      (doseq [[i text] (map-indexed vector before)
              :let [i' (- line (count before) (- i))]]
        (doto printer
          (printf "   %s| %s" (pad i') text)
          (proto/newline)))
      (proto/with-color-type printer :error
        #(doto printer
           (printf "=> %s| %s" (pad line) focused)
           (proto/newline)))
      (proto/with-color-type printer :error
        #(if-let [i (->> focused
                         (map-indexed vector)
                         (drop-while (fn [[_ c]] (Character/isWhitespace c)))
                         (ffirst))]
           (doto printer
             (printf "   %s|%s%s" (pad "") (times (inc i) \space)
                     (times (- (count focused) i) \^))
             (proto/newline))
           (doto printer
             (printf "   %s| ^^" (pad ""))
             (proto/newline))))
      (doseq [[i text] (map-indexed vector after)]
        (doto printer
          (printf "   %s| %s" (pad (+ line i 1)) text)
          (proto/newline)))))
  (render-end [_ e]
    (when (and (:show-message opts)
               (:reverse opts))
      (render-error-message printer e))))
