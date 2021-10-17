(ns stacktracer.repl
  (:require [stacktracer.core :as st]
            [stacktracer.xforms :as sx]))

(def default-xform
  (comp (remove #(re-matches #"(?:clojure|nrepl)\..*" (:fn %)))
        (sx/dedupe-by (juxt :fn :method :file :line))
        (sx/dedupe-by (juxt :class
                            (fn [{m :method}]
                              (get '{invoke invokeStatic
                                     doInvoke invokeStatic}
                                   m m))))))

(def ^:private default-options
  (atom {:xform default-xform :lines 7 :limit 10
         :color true :show-message true}))

(defn set-default-options! [opts]
  (reset! default-options opts))

(defn update-default-options! [f & args]
  (apply swap! default-options f args))

(defn root-cause [^Throwable t]
  (when t
    (if-let [cause (.getCause t)]
      (recur cause)
      t)))

(defn pst-for [e & {:as opts}]
  (st/pst e (merge @default-options opts)))

(defn pst [& args]
  (apply pst-for (root-cause *e) args))

(defn nav-for [e & {:as opts}]
  (st/nav e (merge @default-options opts)))

(defn nav [& args]
  (apply nav-for (root-cause *e) args))
