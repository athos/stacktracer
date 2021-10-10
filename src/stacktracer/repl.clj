(ns stacktracer.repl
  (:require [stacktracer.core :as st]))

(def default-xform
  (comp (remove #(re-find #"^clojure\.(:?core|main)\$" (name (:class %))))
        (remove #(= (:method %) 'invoke))))

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
