(ns stacktracer.core
  (:require [clojure.repl :as repl]
            stacktracer.errors
            [stacktracer.loader :as loader]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer :as renderer]
            [stacktracer.utils :as utils]
            [stacktracer.xforms :as sx]))

(defn- build-xform [state {:keys [xform start end include exclude]}]
  (cond-> (or xform identity)
    include (comp (filter #(re-matches include (:fn %))))
    exclude (comp (remove #(re-matches exclude (:fn %))))
    true (comp (map #(assoc % :id (swap! state inc))))
    end (as-> x (if (nat-int? end)
                  (comp x (take (dec end)))
                  (comp x (sx/drop-last (- end)))))
    start (comp (drop (dec start)))))

(defn- prepare-trace-elements [loader es opts]
  (let [state (atom 0)]
    (->> (for [e es
               [class method file line] (proto/ex-trace e)
               :when (not= file "NO_SOURCE_FILE")
               :let [[_ nsname fname] (re-matches #"([^$]+)\$([^$]+)(?:\$.*)?"
                                                  (name class))
                     [_ simple-name] (some->> nsname
                                              (re-matches #"(?:.*\.)?([^.]+)$"))
                     [_ basename ext] (re-matches #"(.+)(\.cljc?)" file)]
               :when (and simple-name basename
                          (= (munge simple-name) basename))
               :let [path (utils/ns-name->path nsname ext)
                     content (proto/load-content loader path line)]
               :when content]
           (-> {:error e :class class :method method :file path :line line
                :fn (repl/demunge (str nsname \$ fname))}
               (merge content)))
         (into [] (build-xform state opts))
         (into [] (comp (map #(assoc % :total @state))
                        (if-let [limit (:limit opts)]
                          (take limit)
                          identity)))
         (group-by :error))))

(defn render-error [e opts]
  (let [loader (loader/make-loader opts)
        renderer (renderer/make-renderer opts)
        errs (take-while some? (iterate proto/ex-cause e))
        err->elems (prepare-trace-elements loader errs opts)]
    (proto/start-rendering renderer)
    (doseq [err (cond-> errs (:reverse opts) reverse)]
      (->> (cond-> (err->elems err)
             (:reverse opts)
             reverse)
           (proto/render-trace renderer err)))
    (proto/end-rendering renderer)))
