(ns stacktracer.api
  (:require [stacktracer.reformat :as reformat]))

(defn reformat [{:keys [from file] :or {from :error-report} :as opts}]
  (let [in (or file *in*)
        args (apply concat opts)]
    (case (keyword from)
      :report-edn
      (apply reformat/reformat-report-edn in args)

      :java-stacktrace
      (apply reformat/reformat-java-stacktrace in args))))
