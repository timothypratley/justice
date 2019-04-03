(ns justice.core-test
  (:require [clojure.test :refer :all]
            [justice.core :as j]
            [meander.strategy.gamma :as s]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ungensym
  (s/bottom-up
    (s/attempt
      (s/match
        (and ?gensym
             (guard (and (symbol? ?gensym)
                         (re-matches #"\?.*_\d+" (name ?gensym)))))
        (symbol (first (str/split (name ?gensym) #"_")))))))

(deftest datascript-rule-expansions-test
  (testing "syntax transformations from justice to datascript"

    (-> (j/datascript-rule "rule-name" '[?x] '(:entity/parent ?x))
        (= '[(rule-name ?x ?result)
             [?x :entity/parent ?result]])
        (is "justice 'get keyword' style translates to Datascript 'relate to result' style"))

    (-> '(and (:k1 ?x) (:k2 ?x))
        (j/datascript-rule-body)
        (= '(and [?x :k1 ?result]
                 [?x :k2 ?result]))
        (is "sibling clauses are both expanded"))

    (-> '(:k2 (:k1 ?x))
        (j/datascript-rule-body)
        (ungensym)
        (= '(and [?x :k1 ?bridge]
                 [?bridge :k2 ?result]))
        (is "nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(or (:entity/parent ?x)
             (ancestor (:entity/parent ?x)))
        (j/datascript-rule-body)
        (ungensym)
        (= '(or [?x :entity/parent ?result]
                (and [?x :entity/parent ?bridge]
                     (ancestor ?bridge ?result))))
        (is "expressions nested in logic expand with a bridge"))

    (-> '(or (:entity/_parent ?x)
             (descendant (:entity/_parent ?x)))
        (j/datascript-rule-body)
        (ungensym)
        (= '(or [?result :entity/parent ?x]
                (and [?result :entity/parent ?bridge]
                     (descendant ?x ?bridge))))
        (is "reverse traversal is mapped correctly"))

    (-> '[[rule-head
           (or :a :b)]]
        (j/datascript-rules)
        (= '[[rule-head :a]
             [rule-head :b]])
        (is "or expressions transform to disjunction clauses"))

    (-> '[[rule-head
           (and :a :b)]]
        (j/datascript-rules)
        (= '[[rule-head :a :b]])
        (is "and expressions transform to conjunction clauses"))))

(deftest register-rule-test
  (testing "registering rules"
    (is (= '{"justice.core-test/ancestor"
             [[(justice.core-test/ancestor ?x ?result)
               [?x :entity/parent ?result]]
              [(justice.core-test/ancestor ?x ?result)
               [?x :entity/parent ?bridge]
               (justice.core-test/ancestor ?bridge ?result)]]}
           (ungensym
             (j/register-rule "justice.core-test/ancestor" '[?x]
                              '(or (:entity/parent ?x) (justice.core-test/ancestor (:entity/parent ?x)))))))))

(deftest defrule-test
  (testing "defrule behaves like defn"
    (is (function? @(j/defrule ancestor [?x]
                      (or (:entity/parent ?x)
                          (ancestor (:entity/parent ?x))))))
    (is (contains? j/*rule-registry* "justice.core-test/ancestor"))
    (is (empty? (ancestor (d/empty-db) '?x '?y)))))
