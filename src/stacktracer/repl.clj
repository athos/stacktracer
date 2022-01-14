(ns stacktracer.repl
  (:require [stacktracer.core :as st]
            [stacktracer.fallback :as fallback]
            [stacktracer.protocols :as proto]
            [stacktracer.xforms :as sx]))

(defn exclude-fns [& patterns]
  (if (seq patterns)
    (->> patterns
         (map (fn [pat] (remove #(re-matches pat (:fn %)))))
         (apply comp))
    identity))

(def skip-internal-calls
  (sx/dedupe-by (juxt :class
                      (fn [{m :method}]
                        (get '{invoke invokeStatic
                               doInvoke invokeStatic}
                             m m)))))

(def skip-duplicate-sites
  (sx/dedupe-by (juxt :file :line)))

(def default-xform
  (comp (exclude-fns #"clojure\..*" #"nrepl\..*")
        skip-internal-calls
        skip-duplicate-sites))

(def default-fallback-fn fallback/default-fallback-fn)

(def ^:private default-options
  (atom {:xform default-xform :lines 3 :limit 3
         :color true :show-messages true :buffered true
         :fallback default-fallback-fn}))

(defn set-default-options! [opts]
  (reset! default-options opts))

(defn update-default-options! [f & args]
  (apply swap! default-options f args))

(defn- pst* [e {:keys [fallback] :as opts}]
  (when e
    (try
      (st/render-error e opts)
      (catch Throwable t
        (proto/fallback fallback e t)))))

(defn pst-for [e & {:as opts}]
  (pst* e (merge @default-options opts)))

(defn pst [& args]
  (apply pst-for *e args))

(defn capture-for [e & {:as opts}]
  (fn [& {:as opts'}]
    (pst* e (merge @default-options opts opts'))))

(defn capture [& args]
  (apply capture-for *e args))
