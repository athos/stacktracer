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

(defn dedupe-by [f]
  (fn [rf]
    (let [pv (volatile! ::none)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [v (f input)
               prior @pv]
           (vreset! pv v)
           (if (= prior v)
             result
             (rf result input))))))))
