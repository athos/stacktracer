(ns stacktracer.printer
  (:refer-clojure :exclude [print printf newline])
  (:require [clojure.core :as cc]
            [clojure.string :as str]
            [stacktracer.protocols :as proto]))

(defprotocol IPrinter
  (print [this text])
  (newline [this])
  (with-alert [this alert f]))

(defrecord MonochromePrinter []
  IPrinter
  (print [_ text]
    (cc/print text))
  (newline [_]
    (cc/newline))
  (with-alert [_ _ f]
    (f)))

(defrecord AsciiColorPrinter []
  IPrinter
  (print [_ text]
    (cc/print text))
  (newline [_]
    (cc/newline))
  (with-alert [_ alert f]
    (case alert
      :info (cc/print "\u001b[36m")
      :danger (cc/print "\u001b[31m"))
    (f)
    (cc/print "\u001b[0m")))

(defn- printf [printer fmt & args]
  (print printer (apply format fmt args)))

(defn- compact-fn-name [qualified-fname]
  (let [[_ nsname fname] (re-matches #"([^/]+)/(.*)" qualified-fname)
        names (str/split nsname #"\.")]
    (if (or (> (count names) 2)
            (> (count (first names)) 20))
      (with-out-str
        (doseq [name (butlast names)]
          (cc/print (first name))
          (cc/print \.))
        (cc/print (last names))
        (cc/print \/)
        (cc/print fname))
      qualified-fname)))

(defn- times [n c]
  (with-out-str
    (dotimes [_ n]
      (cc/print c))))

(defn- pad [n x]
  (let [text (str x), len (count text)]
    (with-out-str
      (cc/print (times (- n len) \space))
      (cc/print text))))

(defn make-print-renderer [printer opts]
  (reify proto/IRenderer
    (render-start [_ e]
      (when (:show-message opts)
        (with-alert printer :danger
          #(printf printer "%s: %s\n\n"
                   (.getSimpleName (class e))
                   (.getMessage ^Throwable e)))))
    (render-content [_ {fname :fn :keys [file line]} content]
      (let [{:keys [before focused after]} content
            ndigits (count (str (+ line (count after))))
            pad #(pad ndigits %)]
        (with-alert printer :info
          #(printf printer "   ---- %s (%s:%d) ----\n"
                   (compact-fn-name fname) file line))
        (doseq [[i text] (map-indexed vector before)
                :let [i' (- line (count before) (- i))]]
          (printf printer "   %s| %s\n" (pad i') text))
        (with-alert printer :danger
          #(printf printer "=> %s| %s\n" (pad line) focused))
        (let [i (->> focused
                     (map-indexed vector)
                     (drop-while (fn [[_ c]] (Character/isWhitespace c)))
                     (ffirst))]
          (with-alert printer :danger
            #(printf printer "   %s|%s%s\n" (pad "") (times (inc i) \space)
                     (times (- (count focused) i) \^))))
        (doseq [[i text] (map-indexed vector after)]
          (printf printer "   %s| %s\n" (pad (+ line i 1)) text)))
      (newline printer))
    (render-end [_ _])))
