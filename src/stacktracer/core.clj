(ns stacktracer.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- load-file-content [file line ctx-lines]
  (when-let [res (io/resource file)]
    (with-open [r (io/reader res)]
      (let [line' (dec line)]
        (->> (line-seq r)
             (map-indexed vector)
             (reduce (fn [ret [i text]]
                       (cond (<= (- line' ctx-lines) i (dec line'))
                             (update-in ret [:content :before] (fnil conj []) text)

                             (= i line')
                             (assoc-in ret [:content :focus-line] text)

                             (<= (inc line') i (+ line' ctx-lines))
                             (update-in ret [:content :after] (fnil conj []) text)

                             :else ret))
                     {}))))))

(defn- print-file-content [{:keys [file line content]}]
  (printf "   \u001b[36m---- %s:%d ----\u001b[0m\n" file line)
  (let [{:keys [before focus-line after]} content]
    (doseq [[i text] (map-indexed vector before)
            :let [i' (- line (count before) (- i))]]
      (printf "   %d| %s\n" i' text))
    (printf "\u001b[31m=> %d| %s\u001b[0m\n" line focus-line)
    (doseq [[i text] (map-indexed vector after)]
      (printf "   %d| %s\n" (+ line i 1) text))))

(defn- collect-stacktrace-relevant-contents [xform stacktrace]
  (->> (for [[class method file line] (map StackTraceElement->vec stacktrace)
             :when (not= file "NO_SOURCE_FILE")
             :let [path (-> (name class)
                            (str/replace #"\$.*$" "")
                            munge
                            (str/replace #"\." "/")
                            (str ".clj"))
                   content (load-file-content path line 10)]
             :when content]
         (assoc content
                :class class
                :method method
                :file path
                :line line))
       (sequence xform)))

(def default-xform
  (comp (remove #(re-find #"^clojure\.(:?core|main)\$" (name (:class %))))
        (remove #(= (:method %) 'invoke))))

(defn esp
  ([] (esp *e))
  ([e]
   (->> (.getStackTrace e)
        (collect-stacktrace-relevant-contents default-xform)
        (run! (fn [content]
                (print-file-content content)
                (newline))))))

(defn nav
  ([] (nav *e))
  ([e]
   (let [contents (->> (.getStackTrace e)
                       (collect-stacktrace-relevant-contents default-xform)
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
              (print-file-content (get contents i))
              (reset! index i))))
        nil)))))
