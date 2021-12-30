(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.main :as main]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer :as renderer]
            [stacktracer.xforms :as sx])
  (:import [java.util.regex Pattern]))

(def ^:private ^String file-separator
  (System/getProperty "file.separator"))

(def ^:private file-separator-re
  (Pattern/compile (Pattern/quote file-separator)))

(defn ex-data->compiler-exception-trace [{:clojure.error/keys [source line]}]
  (let [nsname (-> source
                   (str/replace #"\.(?:clj|cljc)$" "")
                   (str/replace file-separator "."))
        filename (-> source (str/split file-separator-re) last)]
    [[(symbol (str nsname \$ "<toplevel>")) '<none> filename line]]))

(extend-protocol proto/IStacktrace
  Object
  (ex-message [this]
    (when (and (map? this) (:trace this))
      (main/ex-str (main/ex-triage this))))
  (ex-trace [this]
    (when (and (map? this) (:trace this))
      (:trace this)))
  (ex-cause [_])
  Throwable
  (ex-message [this]
    (main/err->msg this))
  (ex-trace [this]
    (:trace (Throwable->map this)))
  (ex-cause [_])
  clojure.lang.Compiler$CompilerException
  (ex-message [this]
    (-> (Throwable->map this)
        main/ex-triage
        main/ex-str))
  (ex-trace [this]
    (ex-data->compiler-exception-trace (ex-data this)))
  (ex-cause [this]
    (when (= (:clojure.error/phase (ex-data this)) :macroexpansion)
      (ex-cause this))))

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

(defn- collect-elements [opts e]
  (let [state (atom 0)]
    (as-> (proto/ex-trace e) elems
      (for [[class method file line] elems
            :when (not= file "NO_SOURCE_FILE")
            :let [[_ nsname fname] (re-matches #"([^$]+)\$([^$]+)(?:\$.*)?"
                                               (name class))
                  [_ simple-name] (some->> nsname (re-matches #"(?:.*\.)?([^.]+)$"))
                  [_ basename ext] (re-matches #"(.+)(\.cljc?)" file)]
            :when (and simple-name basename
                       (= (munge simple-name) basename))
            :let [path (-> (munge nsname)
                           (str/replace "." file-separator)
                           (str ext))
                  res (-> (clojure.lang.RT/baseLoader)
                          (.getResource path))]
            :when res]
        {:class class :method method
         :fn (repl/demunge (str nsname \$ fname))
         :resource res :file path :line line})
      (into [] (build-xform state opts) elems)
      (into []
            (comp (map #(assoc % :total @state))
                  (if-let [limit (:limit opts)]
                    (take limit)
                    identity))
            elems))))

(defn render-error [e opts]
  (let [renderer (renderer/make-renderer opts)]
    (doseq [e (take-while identity (iterate proto/ex-cause e))
            :let [elems (map #(merge % (load-element-content % opts))
                             (collect-elements opts e))]
            :when (seq elems)]
      (proto/render-trace renderer e elems))))
