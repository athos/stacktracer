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

(defrecord PrettyRenderer [printer opts]
  proto/IRenderer
  (render-start [_ e]
    (when (:show-message opts)
      (proto/with-alert printer :danger
        #(printf printer "%s: %s\n\n"
                 (.getSimpleName (class e))
                 (.getMessage ^Throwable e)))))
  (render-content [_ {fname :fn :keys [file line]} content]
    (let [{:keys [before focused after]} content
          ndigits (count (str (+ line (count after))))
          pad #(pad ndigits %)]
      (proto/with-alert printer :info
        #(printf printer "   ---- %s (%s:%d) ----\n"
                 (compact-fn-name fname) file line))
      (doseq [[i text] (map-indexed vector before)
              :let [i' (- line (count before) (- i))]]
        (printf printer "   %s| %s\n" (pad i') text))
      (proto/with-alert printer :danger
        #(printf printer "=> %s| %s\n" (pad line) focused))
      (let [i (->> focused
                   (map-indexed vector)
                   (drop-while (fn [[_ c]] (Character/isWhitespace c)))
                   (ffirst))]
        (proto/with-alert printer :danger
          #(printf printer "   %s|%s%s\n" (pad "") (times (inc i) \space)
                   (times (- (count focused) i) \^))))
      (doseq [[i text] (map-indexed vector after)]
        (printf printer "   %s| %s\n" (pad (+ line i 1)) text)))
    (proto/newline printer))
  (render-end [_ _]))
