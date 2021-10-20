(ns stacktracer.render
  (:require [clojure.string :as str]
            [stacktracer.protocols :as proto]))

(defn- compact-fn-name [qualified-fname]
  (let [[_ nsname fname] (re-matches #"([^/]+)/(.*)" qualified-fname)
        names (str/split nsname #"\.")]
    (if (or (> (count names) 2)
            (> (count (first names)) 20))
      (with-out-str
        (doseq [name (butlast names)]
          (print (first name))
          (print \.))
        (print (last names))
        (print \/)
        (print fname))
      qualified-fname)))

(defn make-print-renderer [opts]
  (reify proto/IRenderer
    (render-start [_ e]
      (when (:show-message opts)
        (printf "%s: %s\n"
                (.getSimpleName (class e))
                (.getMessage ^Throwable e))
        (newline)))
    (render-content [_ {fname :fn :keys [file line]} content]
      (let [{:keys [before focused after]} content
            ndigits (count (str (+ line (count after))))
            times (fn [n c]
                    (with-out-str
                      (dotimes [_ n]
                        (print c))))
            pad (fn [x]
                  (let [text (str x), len (count text)]
                    (with-out-str
                      (print (times (- ndigits len) \space))
                      (print text))))
            cprintf (if (:color opts)
                      (fn [attr fmt & args]
                        (case attr
                          :info (print "\u001b[36m")
                          :error (print "\u001b[31m"))
                        (apply printf fmt args)
                        (print "\u001b[0m"))
                      (fn [_ fmt & args]
                        (apply printf fmt args)))]
        (cprintf :info "   ---- %s (%s:%d) ----\n"
                 (compact-fn-name fname) file line)
        (doseq [[i text] (map-indexed vector before)
                :let [i' (- line (count before) (- i))]]
          (printf "   %s| %s\n" (pad i') text))
        (cprintf :error "=> %s| %s\n" (pad line) focused)
        (let [i (->> focused
                     (map-indexed vector)
                     (drop-while (fn [[_ c]] (Character/isWhitespace c)))
                     (ffirst))]
          (cprintf :error "   %s|%s%s\n" (pad "") (times (inc i) \space)
                   (times (- (count focused) i) \^)))
        (doseq [[i text] (map-indexed vector after)]
          (printf "   %s| %s\n" (pad (+ line i 1)) text)))
      (newline))
    (render-end [_ _])))
