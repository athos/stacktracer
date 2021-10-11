(ns stacktracer.repl
  (:require [stacktracer.core :as st]))

(defn- dedupe-by [f]
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

(def default-xform
  (comp (remove #(re-matches #"^clojure\.(?:core|main)/.*" (:fn %)))
        (dedupe-by (juxt :fn :method :file :line))
        (dedupe-by (juxt :class
                         (fn [{m :method}]
                           (get '{invoke invokeStatic
                                  doInvoke invokeStatic}
                                m m))))))

(def ^:private default-options
  (atom {:xform default-xform :lines 7 :color true}))

(defn set-default-options! [opts]
  (reset! default-options opts))

(defn update-default-options! [f & args]
  (apply swap! default-options f args))

(defn pst [& args]
  (let [[e [& {:as opts}]] (if (instance? Throwable (first args))
                             [(first args) (rest args)]
                             [*e args])]
    (st/pst e (merge @default-options opts))))

(defn nav [& args]
  (let [[e [& {:as opts}]] (if (instance? Throwable (first args))
                             [(first args) (rest args)]
                             [*e args])]
    (st/nav e (merge @default-options opts))))
