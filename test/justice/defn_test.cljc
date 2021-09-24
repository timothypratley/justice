(ns justice.defn-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.defn :as defn]
            [meander.strategy.epsilon :as s]))

(def defn-identity (defn/wrap-defn identity))

(deftest defn-test
  (testing "valid defn forms parse and are rewritten to themselves"
    (let [valid-defn-forms
          '[(foo "docstring" {:private true} [a b] baz)
            (foo "docstring" {:private true} [a b])
            (foo "docstring" {:private true} [])
            (foo {:private true} [a b] baz)
            (foo "docstring" [a b] baz)
            (foo [a b] baz)
            (foo ([a b] baz))
            (foo ([a] baz) ([a b] baz))
            (foo ([]) ([a b] baz))
            (foo [])]]
      (doseq [test-case valid-defn-forms]
        (is (defn-identity test-case)
          "should be rewritten in canonical form"))))

  (testing "invalid defn forms do not parse"
    (let [invalid-defn-forms
          '[(foo bar [a b] baz)]]
      (doseq [test-case invalid-defn-forms]
        (is (= s/*fail* (defn-identity test-case))
          "should fail to parse")))))

(deftest defrule-test
  #_(testing "defrule expands to expected defn forms"
    (is (= '(defn foo "bar" {} [x] 1)
          (defn/defrule* '(foo "bar" {} [x] 1))))
    (is (= '(defn foo {} [x] 1)
          (defn/defrule* '(foo {} [x] 1))))
    (is (= '(defn foo {} [])
          (defn/defrule* '(foo {} []))))))
