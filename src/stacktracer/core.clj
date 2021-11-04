(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.main :as main]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer :as renderer]
            [stacktracer.xforms :as sx]))

(extend-protocol proto/IStacktrace
  Object
  (ex-message-lines [this]
    (when (and (map? this) (:trace this))
      (-> (main/ex-triage this)
          main/ex-str
          str/split-lines)))
  (ex-trace [this]
    (when (and (map? this) (:trace this))
      (:trace this)))
  Throwable
  (ex-message-lines [this]
    (str/split-lines (main/err->msg this)))
  (ex-trace [this]
    (:trace (Throwable->map this)))
  clojure.lang.Compiler$CompilerException
  (ex-message-lines [this]
    (-> (Throwable->map this)
        main/ex-triage
        main/ex-str
        str/split-lines))
  (ex-trace [this]
    (let [{:clojure.error/keys [source line]} (ex-data this)
          nsname (-> source
                     (str/replace #"\.(?:clj|cljc)$" "")
                     (str/replace #"/" "."))
          filename (str/replace source #"(?:.*\/)?([^/]+)$" "$1")]
      [[(symbol (str nsname \$ "<toplevel>")) '<none> filename line]])))

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

(defn- collect-available-elements [opts stacktrace]
  (let [state (atom 0)]
    (as-> stacktrace elems
      (for [[class method file line] elems
            :when (not= file "NO_SOURCE_FILE")
            :let [[_ nsname fname] (re-matches #"([^$]+)\$([^$]+)(?:\$.*)?"
                                               (name class))
                  [_ simple-name] (some->> nsname (re-matches #"(?:.*\.)?([^.]+)$"))
                  [_ basename ext] (re-matches #"(.+)(\.cljc?)" file)]
            :when (and simple-name basename
                       (= (munge simple-name) basename))
            :let [path (-> (munge nsname)
                           (str/replace #"\." "/")
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
  (let [renderer (renderer/make-renderer opts)
        elems (collect-available-elements opts (proto/ex-trace e))
        contents (map #(load-element-content % opts) elems)]
    (proto/render-start renderer e)
    (proto/render-trace renderer elems contents)
    (proto/render-end renderer e)))
