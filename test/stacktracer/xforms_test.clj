(ns stacktracer.xforms-test
  (:require [clojure.test :refer [deftest is]]
            [stacktracer.xforms :as sx]))

(deftest drop-last-test
  (is (= [0 1 2 3] (into [] (sx/drop-last) (range 5))))
  (is (= [0 1] (into [] (sx/drop-last 3) (range 5)))))

(deftest dedupe-by-test
  (is (= [{:x 1} {:x 2} {:x 3}]
         (into [] (sx/dedupe-by :x)
               [{:x 1} {:x 1 :y 2} {:x 2} {:x 2 :y 3} {:x 2} {:x 3}]))))
