(ns stacktracer.printer
  (:require [stacktracer.protocols :as proto]))

(defrecord MonochromePrinter []
  proto/IPrinter
  (print [_ text]
    (print text))
  (newline [_]
    (newline))
  (with-alert [_ _ f]
    (f)))

(defrecord AsciiColorPrinter []
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
