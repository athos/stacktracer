(ns stacktracer.utils-test
  (:require [stacktracer.utils :as utils]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]))

(defn- file-path [& paths]
  (.getPath (apply io/file paths)))

(deftest file-basename-test
  (is (= "baz.clj"
         (utils/file-basename (file-path "foo" "bar" "baz.clj")))))

(deftest path->ns-name-test
  (is (= "foo.bar.baz"
         (utils/path->ns-name (file-path "foo" "bar" "baz"))))
  (is (= "foo.bar.baz"
         (utils/path->ns-name (file-path "foo" "bar" "baz.clj"))))
  (is (= "foo.bar.baz"
         (utils/path->ns-name (file-path "foo" "bar" "baz.cljc")))))

(deftest ns-name->path-test
  (is (= (file-path "foo" "bar" "baz.clj")
         (utils/ns-name->path "foo.bar.baz" ".clj"))))
