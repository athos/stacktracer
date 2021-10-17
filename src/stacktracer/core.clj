(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [stacktracer.xforms :as sx]))

(defn- load-entry-content [{:keys [resource line]} {nlines :lines}]
  (with-open [r (io/reader resource)]
    (let [line' (dec line)]
      (->> (line-seq r)
           (map-indexed vector)
           (reduce (fn [ret [i text]]
                     (cond (<= (- line' nlines) i (dec line'))
                           (update ret :before (fnil conj []) text)

                           (= i line')
                           (assoc ret :focus-line text)

                           (<= (inc line') i (+ line' nlines))
                           (update ret :after (fnil conj []) text)

                           :else ret))
                   {})))))

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

(defn- print-entry-content [{fname :fn :keys [file line]} content opts]
  (let [{:keys [before focus-line after]} content
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
    (cprintf :error "=> %s| %s\n" (pad line) focus-line)
    (let [i (->> focus-line
                 (map-indexed vector)
                 (drop-while (fn [[_ c]] (Character/isWhitespace c)))
                 (ffirst))]
      (cprintf :error "   %s|%s%s\n" (pad "") (times (inc i) \space)
               (times (- (count focus-line) i) \^)))
    (doseq [[i text] (map-indexed vector after)]
      (printf "   %s| %s\n" (pad (+ line i 1)) text))))

(defn- build-xform [{:keys [xform start end limit include exclude]}]
  (cond-> (or xform identity)
    include (comp (filter #(re-matches include (:fn %))))
    exclude (comp (remove #(re-matches exclude (:fn %))))
    end (as-> x (if (nat-int? end)
                  (comp x (take end))
                  (comp x (sx/drop-last (- end)))))
    start (comp (drop start))
    limit (comp (take limit))))

(defn- collect-available-entries [opts stacktrace]
  (->> (for [[class method file line] (map StackTraceElement->vec stacktrace)
             :when (not= file "NO_SOURCE_FILE")
             :let [[_ nsname fname] (re-matches #"([^$]+)\$([^$]+)(?:\$.*)?"
                                                (name class))
                   [_ simple-name] (some->> nsname (re-matches #".*\.([^.]+)$"))
                   [_ basename ext] (re-matches #"(.+)(\.cljc?)" file)]
             :when (and simple-name basename
                        (= simple-name basename))
             :let [path (-> (munge nsname)
                            (str/replace #"\." "/")
                            (str ext))
                   res (-> (clojure.lang.RT/baseLoader)
                           (.getResource path))]
             :when res]
         {:class class :method method
          :fn (repl/demunge (str nsname \$ fname))
          :resource res :file path :line line})
       (sequence (build-xform opts))
       (#(cond-> % (:reverse opts) reverse))))

(defn pst [e opts]
  (when e
    (when (:show-message opts)
      (printf "%s %s\n" (.getSimpleName (class e)) (.getMessage e))
      (newline))
    (->> (.getStackTrace e)
         (collect-available-entries opts)
         (run! (fn [file]
                 (let [content (load-entry-content file opts)]
                   (print-entry-content file content opts)
                   (newline)))))))

(defn nav [e opts]
  (let [entries (or (some->> e
                             (.getStackTrace)
                             (collect-available-entries opts)
                             vec)
                    [])
        index (atom -1)]
    (fn self
      ([] (self :next))
      ([arg]
       (case arg
         :reset (reset! index -1)
         (let [i (case arg
                   :prev (max (dec @index) 0)
                   :next (inc @index)
                   (if (neg? arg)
                     (+ (count entries) arg)
                     arg))]
           (when (< i (count entries))
             (let [entry (get entries i)
                   content (load-entry-content entry opts)]
               (print-entry-content entry content opts)
               (reset! index i)))))
       nil))))
