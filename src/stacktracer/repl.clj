(ns stacktracer.repl
  (:require [stacktracer.core :as st]
            [stacktracer.xforms :as sx]))

(defn exclude-fns [& patterns]
  (if (seq patterns)
    (->> patterns
         (map (fn [pat] (remove #(re-matches pat (:fn %)))))
         (apply comp))
    identity))

(defn skip-internal-calls []
  (sx/dedupe-by (juxt :class
                      (fn [{m :method}]
                        (get '{invoke invokeStatic
                               doInvoke invokeStatic}
                             m m)))))

(defn skip-duplicate-sites []
  (sx/dedupe-by (juxt :file :line)))

(def default-xform
  (comp (exclude-fns #"clojure\..*" #"nrepl\..*")
        (skip-internal-calls)
        (skip-duplicate-sites)))

(def ^:private default-options
  (atom {:xform default-xform :lines 5 :limit 10
         :color true :show-message true}))

(defn set-default-options! [opts]
  (reset! default-options opts))

(defn update-default-options! [f & args]
  (apply swap! default-options f args))

(defn pst-for [e & {:as opts}]
  (st/pst e (merge @default-options opts)))

(defn pst [& args]
  (apply pst-for *e args))

(defn capture-for [e & {:as opts}]
  (fn [& {:as opts'}]
    (st/pst e (merge @default-options opts opts'))))

(defn capture [& args]
  (apply capture-for *e args))
