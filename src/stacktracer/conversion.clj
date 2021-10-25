(ns stacktracer.conversion
  (:require [clojure.java.io :as io]))

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

(defn convert-from-java-stacktrace [r]
  (let [parsed (parse-java-stacktrace (line-seq r))
        {:keys [message trace]} (last parsed)]
    {:cause message
     :trace trace
     :via (mapv (fn [{:keys [type message trace]}]
                  {:type type :message message :at (first trace)})
                parsed)
     :data {}}))
