(ns stacktracer.renderer.common-test
  (:require [clojure.test :refer [deftest is]]
            [stacktracer.protocols :as proto]
            [stacktracer.renderer.common :as common]))

(defrecord MockPrinter [^StringBuilder sb]
  proto/IPrinter
  (print [_ text]
    (.append sb ^String text))
  (newline [_]
    (.append sb \newline))
  (with-color-type [_ _ f]
    (f))
  (flush [_]))

(defn- make-mock-printer []
  (->MockPrinter (StringBuilder.)))

(deftest printf-test
  (let [printer (make-mock-printer)]
    (common/printf printer "%d-%02d-%02d Hello, %s!" 2020 1 1 "Clojure")
    (is (= "2020-01-01 Hello, Clojure!" (str (:sb printer))))))

(deftest times-test
  (is (= "aaa" (common/times 3 \a)))
  (is (= "bbbbb" (common/times 5 \b)))
  (is (= "" (common/times 0 \a))))

(deftest pad-test
  (is (= "  foo" (common/pad 5 "foo")))
  (is (= "   42" (common/pad 5 42)))
  (is (= "foobar" (common/pad 5 "foobar"))))

(deftest compact-fn-name
  (is (= "foo.bar/fn" (common/compact-fn-name "foo.bar/fn")))
  (is (= "f.b.baz/fn" (common/compact-fn-name "foo.bar.baz/fn")))
  (is (= "t.foo/fn" (common/compact-fn-name "tooooooo-long-segment.foo/fn"))))

(deftest render-error-message-test
  (let [printer (make-mock-printer)
        e (reify proto/IStacktrace
            (ex-message [_]
              (str "Execution error (ExceptionInfo) at foo.bar/fn (bar.clj:42)\n"
                   "error!!"))
            (wrapped? [_] false))]
    (common/render-error-message printer e)
    (is (= (str "Execution error (ExceptionInfo) at foo.bar/fn (bar.clj:42)\n"
                "error!!\n")
           (str (:sb printer)))))
  (let [printer (make-mock-printer)
        e (reify proto/IStacktrace
            (ex-message [_]
              (str "Execution error (ExceptionInfo) at foo.bar/fn (bar.clj:42)\n"
                   "error!!"))
            (wrapped? [_] true))]
    (common/render-error-message printer e)
    (is (= (str "Caused by: Execution error (ExceptionInfo) at foo.bar/fn (bar.clj:42)\n"
                "error!!\n")
           (str (:sb printer))))))
