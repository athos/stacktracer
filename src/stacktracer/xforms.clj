(ns stacktracer.xforms
  (:refer-clojure :exclude [drop-last])
  (:require [clojure.core :as cc]))

(defn drop-last
  ([] (drop-last 1))
  ([n]
   (fn [rf]
     (let [buf (volatile! [])]
       (fn
         ([] (rf))
         ([result]
          (reduce rf result (cc/drop-last n @buf)))
         ([result input]
          (vswap! buf conj input)
          result))))))
