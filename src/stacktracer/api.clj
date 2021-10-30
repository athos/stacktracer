(ns stacktracer.api
  (:require [clojure.java.io :as io]
            [stacktracer.reformat :as reformat])
  (:import [java.util.regex Pattern]))

(defn- fixup-opts [{:keys [include exclude] :as opts}]
  (cond-> opts
    include (assoc :include (Pattern/compile (str include)))
    exclude (assoc :exclude (Pattern/compile (str exclude)))))

(defn reformat [{:keys [from file] :or {from :guess} :as opts}]
  (let [content (with-open [r (io/reader (or file *in*))]
                  (slurp r))
        format (if (= from :guess)
                 (or (with-in-str content
                       (reformat/guess-format *in*))
                     :java-stacktrace)
                 (keyword from))
        args (apply concat (fixup-opts opts))]
    (with-in-str content
      (case format
        :report-edn
        (apply reformat/reformat-report-edn *in* args)

        :java-stacktrace
        (apply reformat/reformat-java-stacktrace *in* args)))))
