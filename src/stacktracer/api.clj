(ns stacktracer.api
  (:require [clojure.java.io :as io]
            [stacktracer.reformat :as reformat])
  (:import [java.io PushbackReader]
           [java.util.regex Pattern]))

(defn- fixup-opts [{:keys [include exclude] :as opts}]
  (cond-> opts
    include (assoc :include (Pattern/compile (str include)))
    exclude (assoc :exclude (Pattern/compile (str exclude)))))

(defn reformat [{:keys [from file] :or {from :guess} :as opts}]
  (with-open [r (PushbackReader. (io/reader (or file *in*)))]
    (let [format (if (= from :guess)
                   (or (reformat/guess-format r)
                       :java-stacktrace)
                   (keyword from))
          args (apply concat (fixup-opts opts))]
      (case format
        :report-edn
        (apply reformat/reformat-report-edn r args)

        :java-stacktrace
        (apply reformat/reformat-java-stacktrace (io/reader r) args)))))
