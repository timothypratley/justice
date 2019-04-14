(ns justice.translation-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.translation :as t]
            [clojure.string :as string]))

(def ungensym
  (t/rewrite-all
    (and ?gensym
      (guard (and (symbol? ?gensym)
               (re-matches #"\?.*_\d+" (name ?gensym)))))
    ~(symbol (first (string/split (name ?gensym) #"_")))))

(deftest datascript-rule-expansions-test
  (testing "syntax transformations from justice to datascript"

    (-> (t/datascript-rule 'translation-test/rule-name '[?x] '(:entity/parent ?x))
      (= '[(translation-test/rule-name ?x ?result)
           [?x :entity/parent ?result]])
      (is "justice 'get keyword' style translates to Datascript 'relate to result' style"))

    (-> '(and (:k1 ?x) (:k2 ?x))
      (t/datascript-rule-body)
      (= '(and [?x :k1 ?result]
            [?x :k2 ?result]))
      (is "sibling clauses are both expanded"))

    (-> '(:k2 (:k1 ?x))
      (t/datascript-rule-body)
      (ungensym)
      (= '(and [?x :k1 ?bridge]
            [?bridge :k2 ?result]))
      (is "nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(or (:entity/parent ?x)
           (ancestor (:entity/parent ?x)))
      (t/datascript-rule-body)
      (ungensym)
      (= '(or [?x :entity/parent ?result]
            (and [?x :entity/parent ?bridge]
              (ancestor ?bridge ?result))))
      (is "expressions nested in logic expand with a bridge"))

    (-> '(or (:entity/_parent ?x)
           (descendant (:entity/_parent ?x)))
      (t/datascript-rule-body)
      (ungensym)
      (= '(or [?result :entity/parent ?x]
            (and [?result :entity/parent ?bridge]
              (descendant ?x ?bridge))))
      (is "reverse traversal is mapped correctly"))

    (-> '[[rule-head
           (or :a :b)]]
      (t/datascript-rules)
      (= '[[rule-head :a]
           [rule-head :b]])
      (is "or expressions transform to disjunction clauses"))

    (-> '[[rule-head
           (and :a :b)]]
      (t/datascript-rules)
      (= '[[rule-head :a :b]])
      (is "and expressions transform to conjunction clauses"))))