(ns stacktracer.loader
  (:require [clojure.java.io :as io]
            [stacktracer.protocols :as proto]))

(defn load-content [path line {nlines :lines}]
  (with-open [r (io/reader path)]
    (let [line' (dec line)]
      (->> (line-seq r)
           (map-indexed vector)
           (reduce (fn [ret [i text]]
                     (cond (<= (- line' nlines) i (dec line'))
                           (update ret :before (fnil conj []) text)

                           (= i line')
                           (assoc ret :focused text)

                           (<= (inc line') i (+ line' nlines))
                           (update ret :after (fnil conj []) text)

                           :else ret))
                   {:focused ""})))))

(defrecord ResourceLoader [opts]
  proto/ILoader
  (load-content [_ path line]
    (some-> (clojure.lang.RT/baseLoader)
            (.getResource path)
            (load-content line opts))))

(defmulti make-loader (fn [opts] (:loader opts)))

(defmethod make-loader :default [opts]
  (make-loader (assoc opts :loader :resource)))

(defmethod make-loader :resource [opts]
  (->ResourceLoader opts))
