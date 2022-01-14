(ns stacktracer.printer
  (:require [stacktracer.protocols :as proto]))

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

(defmulti make-printer (fn [opts] (:printer opts)))

(defmethod make-printer :default [opts]
  (if-let [printer (:printer opts)]
    printer
    (make-printer (assoc opts :printer :console))))

(defmethod make-printer :console [opts]
  (binding [*out* (or (:output-to opts) *err*)]
    (if (:color opts)
      (let [colors (merge default-colors (:colors opts))]
        (->AsciiColorConsolePrinter colors))
      (->MonochromeConsolePrinter))))
