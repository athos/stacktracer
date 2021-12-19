(ns stacktracer.renderer.common
  (:refer-clojure :exclude [printf])
  (:require [clojure.string :as str]
            [stacktracer.protocols :as proto]))

(defn printf [printer fmt & args]
  (proto/print printer (apply format fmt args)))

(defn times [n c]
  (with-out-str
    (dotimes [_ n]
      (print c))))

(defn pad [n x]
  (let [text (str x), len (count text)]
    (with-out-str
      (print (times (- n len) \space))
      (print text))))

(defn compact-fn-name [qualified-fname]
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

(defn render-error-message [printer e]
  (proto/with-color-type printer :error
    #(doseq [line (str/split-lines (proto/ex-message e))]
       (doto printer
         (proto/print line)
         (proto/newline)))))
