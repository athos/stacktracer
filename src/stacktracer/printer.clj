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

(defrecord AsciiColorConsolePrinter []
  proto/IPrinter
  (print [_ text]
    (print text))
  (newline [_]
    (newline))
  (with-alert [_ alert f]
    (case alert
      :info (print "\u001b[36m")
      :danger (print "\u001b[31m"))
    (f)
    (print "\u001b[0m")))

(defmulti make-printer (fn [opts] (:printer opts)))

(defmethod make-printer :default [opts]
  (if-let [printer (:printer opts)]
    printer
    (make-printer (assoc opts :printer :console))))

(defmethod make-printer :console [opts]
  (if (:color opts)
    (->AsciiColorConsolePrinter)
    (->MonochromeConsolePrinter)))
