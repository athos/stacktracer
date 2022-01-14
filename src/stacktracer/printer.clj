(ns stacktracer.printer
  (:require [stacktracer.protocols :as proto])
  (:import [java.io PrintWriter StringWriter]))

(defrecord MonochromeConsolePrinter []
  proto/IPrinter
  (print [_ text]
    (print text))
  (newline [_]
    (newline))
  (with-color-type [_ _ f]
    (f))
  (flush [_]
    (flush)))

(def default-colors
  {:info "\u001b[36m"
   :error "\u001b[31;1m"})

(def ^:private ^:dynamic *current-color-type* nil)

(defrecord AsciiColorConsolePrinter [colors]
  proto/IPrinter
  (print [_ text]
    (print text))
  (newline [_]
    (newline))
  (with-color-type [_ color-type f]
    (binding [*current-color-type* color-type]
      (print (get colors color-type))
      (f))
    (print (or (some-> color-type (get colors)) "\u001b[0m")))
  (flush [_]
    (flush)))

(defrecord BufferedPrinter [printer ^PrintWriter pw ^StringWriter sw]
  proto/IPrinter
  (print [_ text]
    (binding [*out* pw]
      (proto/print printer text)))
  (newline [_]
    (binding [*out* pw]
      (proto/newline printer)))
  (with-color-type [_ color-type f]
    (binding [*out* pw]
      (proto/with-color-type printer color-type f)))
  (flush [_]
    (binding [*out* pw]
      (proto/flush printer))
    (print (.toString sw))
    (flush)))

(defn make-buffered-printer [printer]
  (let [sw (StringWriter.)
        pw (PrintWriter. sw)]
    (->BufferedPrinter printer pw sw)))

(defmulti make-printer (fn [opts] (:printer opts)))

(defmethod make-printer :default [opts]
  (if-let [printer (:printer opts)]
    printer
    (make-printer (assoc opts :printer :console))))

(defmethod make-printer :console [opts]
  (binding [*out* (or (:output-to opts) *err*)]
    (cond-> (if (:color opts)
              (let [colors (merge default-colors (:colors opts))]
                (->AsciiColorConsolePrinter colors))
              (->MonochromeConsolePrinter))
      (:buffered opts)
      make-buffered-printer)))
