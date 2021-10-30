(ns stacktracer.printer
  (:require [stacktracer.protocols :as proto]))

(defrecord MonochromeConsolePrinter []
  proto/IPrinter
  (print [_ text]
    (print text))
  (newline [_]
    (newline))
  (with-alert [_ _ f]
    (f)))

(def default-colors
  {:info "\u001b[36m"
   :danger "\u001b[31;1m"})

(def ^:private ^:dynamic *current-alert* nil)

(defrecord AsciiColorConsolePrinter [colors]
  proto/IPrinter
  (print [_ text]
    (print text))
  (newline [_]
    (newline))
  (with-alert [_ alert f]
    (binding [*current-alert* alert]
      (print (get colors alert))
      (f))
    (print (or (some-> alert (get colors)) "\u001b[0m"))))

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
