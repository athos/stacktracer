(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.repl :as repl]
            stacktracer.errors
            [stacktracer.protocols :as proto]
            [stacktracer.renderer :as renderer]
            [stacktracer.utils :as utils]
            [stacktracer.xforms :as sx]))

(defn- load-element-content [{:keys [resource line]} {nlines :lines}]
  (with-open [r (io/reader resource)]
    (let [line' (dec line)]
      (->> (line-seq r)
           (map-indexed vector)
           (reduce (fn [ret [i text]]
                     (cond (<= (- line' nlines) i (dec line'))
                           (update ret :before (fnil conj []) text)

                           (= i line')
                           (assoc ret :focused text)

                           (<= (inc line') i (+ line' nlines))
                           (update ret :after (fnil conj []) text)

                           :else ret))
                   {:focused ""})))))

(defn- build-xform [state {:keys [xform start end include exclude]}]
  (cond-> (or xform identity)
    include (comp (filter #(re-matches include (:fn %))))
    exclude (comp (remove #(re-matches exclude (:fn %))))
    true (comp (map #(assoc % :id (swap! state inc))))
    end (as-> x (if (nat-int? end)
                  (comp x (take (dec end)))
                  (comp x (sx/drop-last (- end)))))
    start (comp (drop (dec start)))))

(defn- collect-elements [opts es]
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
                     res (-> (clojure.lang.RT/baseLoader)
                             (.getResource path))]
               :when res]
           {:error e :class class :method method
            :fn (repl/demunge (str nsname \$ fname))
            :resource res :file path :line line})
         (into [] (build-xform state opts))
         (into [] (comp (map #(assoc % :total @state))
                        (if-let [limit (:limit opts)]
                          (take limit)
                          identity)))
         (group-by :error))))

(defn render-error [e opts]
  (let [renderer (renderer/make-renderer opts)
        errs (take-while some? (iterate proto/ex-cause e))
        err->elems (collect-elements opts errs)]
    (doseq [err (cond-> errs (:reverse opts) reverse)
            :let [elems (->> (err->elems err)
                             (map #(merge % (load-element-content % opts)))
                             (#(cond-> % (:reverse opts) reverse)))]]
      (proto/render-trace renderer err elems))))
