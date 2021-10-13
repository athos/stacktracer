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
  (comp (remove #(re-matches #"(?:clojure|nrepl)\..*" (:fn %)))
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

(defn pst-for [e & {:as opts}]
  (st/pst e (merge @default-options opts)))

(defn pst [& args]
  (apply pst-for *e args))

(defn nav-for [e & {:as opts}]
  (st/nav e (merge @default-options opts)))

(defn nav [& args]
  (apply nav-for *e args))
