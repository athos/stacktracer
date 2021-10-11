(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- load-file-content [file line nlines]
  (when-let [res (-> (clojure.lang.RT/baseLoader)
                     (.getResource file))]
    (with-open [r (io/reader res)]
      (let [line' (dec line)]
        (->> (line-seq r)
             (map-indexed vector)
             (reduce (fn [ret [i text]]
                       (cond (<= (- line' nlines) i (dec line'))
                             (update-in ret [:content :before] (fnil conj []) text)

                             (= i line')
                             (assoc-in ret [:content :focus-line] text)

                             (<= (inc line') i (+ line' nlines))
                             (update-in ret [:content :after] (fnil conj []) text)

                             :else ret))
                     {}))))))

(defn- print-file-content [{:keys [file line content]} opts]
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
    (cprintf :info "   ---- %s:%d ----\n" file line)
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

(defn- ns+fn-name-matches? [pattern class-sym]
  (-> (name class-sym)
      (str/replace #"([^$]+\$[^$]+)(?:\$.*)?" "$1")
      (str/replace \$ \/)
      (#(re-matches pattern %))))

(defn- build-xform [{:keys [xform from limit include exclude]}]
  (cond-> (or xform identity)
    include (comp (filter #(ns+fn-name-matches? include (:class %))))
    exclude (comp (remove #(ns+fn-name-matches? exclude (:class %))))
    from (comp (drop from))
    limit (comp (take limit))))

(defn- collect-stacktrace-relevant-contents [opts stacktrace]
  (->> (for [[class method file line] (map StackTraceElement->vec stacktrace)
             :when (not= file "NO_SOURCE_FILE")
             :let [path (-> (name class)
                            (str/replace #"\$.*$" "")
                            munge
                            (str/replace #"\." "/")
                            (str ".clj"))
                   content (load-file-content path line (:lines opts))]
             :when content]
         (assoc content
                :class class
                :method method
                :file path
                :line line))
       (sequence (build-xform opts))
       (#(cond-> % (:reversed? opts) reverse))))

(defn pst [e opts]
  (->> (.getStackTrace e)
       (collect-stacktrace-relevant-contents opts)
       (run! (fn [content]
               (print-file-content content opts)
               (newline)))))

(defn nav [e opts]
  (let [contents (->> (.getStackTrace e)
                      (collect-stacktrace-relevant-contents opts)
                      vec)
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
                     (+ (count contents) arg)
                     arg))]
           (when (< i (count contents))
             (print-file-content (get contents i) opts)
             (reset! index i))))
       nil))))
