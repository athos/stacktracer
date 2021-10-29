(ns stacktracer.reformat
  (:require [clojure.edn :as edn]
            [stacktracer.repl :as st])
  (:import [java.io PushbackReader]))

(defn- parse-java-stacktrace [lines]
  (reduce (fn [acc line]
            (condp re-matches line
              #"([^\s:]+)(?:: (.*))?"
              :>> (fn [[_ ex message]]
                    (conj acc {:type ex :message message}))

              #"Caused by: ([^:]+)(?:: (.*))?"
              :>> (fn [[_ ex message]]
                    (conj acc {:type ex :message message}))

              #"\s+at ([^(]+)\(([^:]+):(\d+)\)"
              :>> (fn [[_ method file line]]
                    (let [[_ class method] (re-matches #"(.*?)\.([^.]+)" method)]
                      (update-in acc [(dec (count acc)) :trace]
                                 (fnil conj [])
                                 [(symbol class) (symbol method)
                                  file (Long/parseLong line)])))

              acc))
          [] lines))

(defn- convert-from-java-stacktrace [r]
  (let [parsed (parse-java-stacktrace (line-seq r))
        {:keys [message trace]} (last parsed)]
    {:cause message
     :trace trace
     :via (mapv (fn [{:keys [type message trace]}]
                  {:type type :message message :at (first trace)})
                parsed)
     :data {}}))

(defn reformat-report-edn [r & {:as opts}]
  (let [edn (edn/read r)
        args (apply concat opts)]
    (apply st/pst-for (:clojure.main/trace edn) args)))

(defn reformat-java-stacktrace [r & {:as opts}]
  (let [converted (convert-from-java-stacktrace r)
        args (apply concat opts)]
    (apply st/pst-for converted args)))

(defn guess-format [^PushbackReader r]
  (when-let [c (loop []
                 (let [c (.read r)]
                   (cond (neg? c) nil
                         (Character/isSpaceChar c) (recur)
                         :else c)))]
    (.unread r c)
    (case (char c)
      \{ :report-edn
      :java-stacktrace)))
