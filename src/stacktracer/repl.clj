(ns stacktracer.repl
  (:require [clojure.repl :as repl]
            [stacktracer.core :as st]
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
  (atom {:xform default-xform :lines 5 :limit 5
         :color true :show-message true}))

(defn set-default-options! [opts]
  (reset! default-options opts))

(defn update-default-options! [f & args]
  (apply swap! default-options f args))

(defn default-fallback-fn [e]
  (binding [*out* *err*]
    (println "[ERROR] Stacktracer failed to process the exception. Falls back to clojure.repl/pst."))
  (repl/pst e))

(defn- pst* [e {:keys [fallback-fn] :as opts}]
  (when e
    (try
      (st/render-error e opts)
      (catch Throwable _
        ((or fallback-fn default-fallback-fn) e)))))

(defn pst-for [e & {:as opts}]
  (pst* e (merge @default-options opts)))

(defn pst [& args]
  (apply pst-for *e args))

(defn capture-for [e & {:as opts}]
  (fn [& {:as opts'}]
    (pst* e (merge @default-options opts opts'))))

(defn capture [& args]
  (apply capture-for *e args))
