(ns stacktracer.reformat
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [stacktracer.errors :as errors]
            [stacktracer.protocols :as proto]
            [stacktracer.repl :as st])
  (:import [java.io PushbackReader]))

(defrecord ReportEdn [data]
  proto/IStacktrace
  (ex-message [_]
    (:clojure.main/message data))
  (ex-trace [_]
    (let [top-elem (first (:via (:clojure.main/trace data)))]
      (if (= (:type top-elem) 'clojure.lang.Compiler$CompilerException)
        (errors/ex-data->compiler-exception-trace (:data top-elem))
        (:trace (:clojure.main/trace data)))))
  (ex-cause [_]))

(defn from-report-edn [r]
  (->ReportEdn (edn/read r)))

(defn reformat-report-edn [in & {:as opts}]
  (with-open [r (PushbackReader. (io/reader in))]
    (let [args (apply concat opts)]
      (apply st/pst-for (from-report-edn r) args))))

(defrecord JavaStacktrace [type message trace]
  proto/IStacktrace
  (ex-message [_]
    (format "%s: %s" type message))
  (ex-trace [_] trace)
  (ex-cause [_]))

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

(defn from-java-stacktrace [r]
  (let [parsed (parse-java-stacktrace (line-seq r))
        {:keys [type message trace]} (last parsed)]
    (->JavaStacktrace type message trace)))

(defn reformat-java-stacktrace [in & {:as opts}]
  (with-open [r (io/reader in)]
    (let [args (apply concat opts)]
      (apply st/pst-for (from-java-stacktrace r) args))))

(defn guess-format [^PushbackReader r]
  (when-let [c (loop []
                 (let [c (.read r)]
                   (cond (neg? c) nil
                         (Character/isSpaceChar c) (recur)
                         :else c)))]
    (.unread r (int c))
    (case (char c)
      \{ :report-edn
      :java-stacktrace)))
