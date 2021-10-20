(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer :as renderer]
            [stacktracer.xforms :as sx]))

(defn- load-entry-content [{:keys [resource line]} {nlines :lines}]
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
                   {})))))

(defn- build-xform [{:keys [xform start end limit include exclude]}]
  (cond-> (or xform identity)
    include (comp (filter #(re-matches include (:fn %))))
    exclude (comp (remove #(re-matches exclude (:fn %))))
    end (as-> x (if (nat-int? end)
                  (comp x (take end))
                  (comp x (sx/drop-last (- end)))))
    start (comp (drop start))
    limit (comp (take limit))))

(defn- collect-available-entries [opts stacktrace]
  (->> (for [[class method file line] (map StackTraceElement->vec stacktrace)
             :when (not= file "NO_SOURCE_FILE")
             :let [[_ nsname fname] (re-matches #"([^$]+)\$([^$]+)(?:\$.*)?"
                                                (name class))
                   [_ simple-name] (some->> nsname (re-matches #".*\.([^.]+)$"))
                   [_ basename ext] (re-matches #"(.+)(\.cljc?)" file)]
             :when (and simple-name basename
                        (= simple-name basename))
             :let [path (-> (munge nsname)
                            (str/replace #"\." "/")
                            (str ext))
                   res (-> (clojure.lang.RT/baseLoader)
                           (.getResource path))]
             :when res]
         {:class class :method method
          :fn (repl/demunge (str nsname \$ fname))
          :resource res :file path :line line})
       (sequence (build-xform opts))
       (#(cond-> % (:reverse opts) reverse))))

(defn pst [e opts]
  (when e
    (let [renderer (renderer/make-renderer opts)]
      (proto/render-start renderer e)
      (->> (.getStackTrace e)
           (collect-available-entries opts)
           (run! (fn [file]
                   (let [content (load-entry-content file opts)]
                     (proto/render-content renderer file content)))))
      (proto/render-end renderer e))))

(defn nav [e opts]
  (let [entries (or (some->> e
                             (.getStackTrace)
                             (collect-available-entries opts)
                             vec)
                    [])
        index (atom -1)
        renderer (renderer/make-renderer opts)]
    (fn self
      ([] (self :next))
      ([arg]
       (case arg
         :reset (reset! index -1)
         (let [i (case arg
                   :prev (max (dec @index) 0)
                   :next (inc @index)
                   (if (neg? arg)
                     (+ (count entries) arg)
                     arg))]
           (when (< i (count entries))
             (let [entry (get entries i)
                   content (load-entry-content entry opts)]
               (proto/render-content renderer entry content)
               (reset! index i)))))
       nil))))
