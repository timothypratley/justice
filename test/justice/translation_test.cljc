(ns justice.translation-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.translation :as t]
            [clojure.string :as string]
            [datascript.core :as d]))

(def ungensym
  (t/rewrite-all
    (and ?gensym
      (guard (and (symbol? ?gensym)
               (re-matches #"\?.*_\d+" (name ?gensym)))))
    ~(symbol (first (string/split (name ?gensym) #"_")))))

(deftest uninverse-test
  (testing "symbols and keywords from inverse to forward style"
    (is (= :k (@#'t/uninverse :_k)))
    (is (= 'ancestor (@#'t/uninverse '_ancestor)))
    (is (= 'foo.bar/ancestor (@#'t/uninverse 'foo.bar/_ancestor)))))

(deftest datascript-rule-expansions-test
  (testing "syntax transformations from justice to datascript"

    (-> (t/datascript-rule 'translation-test/rule-name '[?x] '(:entity/parent ?x))
      (= '[(translation-test/rule-name ?x ?result)
           [?x :entity/parent ?result]])
      (is "justice 'get keyword' style translates to Datascript 'relate to result' style"))

    (-> '(:foo/bar ?x)
      (t/from-justice)
      (= '[?x :foo/bar ?result])
      (is "justice translates function application style to bridged triples"))

    (-> '(and (:k1 ?x) (:k2 ?x))
      (t/from-justice)
      (= '(and
            [?x :k1 ?result]
            [?x :k2 ?result]))
      (is "sibling clauses are both expanded"))

    (-> '(:k2 (:k1 ?x))
      (t/from-justice)
      (ungensym)
      (= '(and
            [?x :k1 ?bridge]
            [?bridge :k2 ?result]))
      (is "nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(:k3 (:k2 (:k1 ?x)))
      (t/from-justice)
      (ungensym)
      (= '(and
            [?x :k1 ?bridge]
            [?bridge :k2 ?bridge]
            [?bridge :k3 ?result]))
      (is "deeply nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(:k4 (:k3 (:k2 (:k1 ?x))))
      (t/from-justice)
      (ungensym)
      (= '(and
            [?x :k1 ?bridge]
            [?bridge :k2 ?bridge]
            [?bridge :k3 ?bridge]
            [?bridge :k4 ?result]))
      (is "very deeply nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(or (:entity/parent ?x)
           (ancestor (:entity/parent ?x)))
      (t/from-justice)
      (ungensym)
      (= '(or [?x :entity/parent ?result]
            (and
              [?x :entity/parent ?bridge]
              (ancestor ?bridge ?result))))
      (is "expressions nested in logic expand with a bridge"))

    (-> '(or (:entity/_parent ?x)
           (descendant (:entity/_parent ?x)))
      (t/from-justice)
      (ungensym)
      (= '(or [?result :entity/parent ?x]
            (and
              [?bridge :entity/parent ?x]
              (descendant ?bridge ?result))))
      (is "reverse traversal inverts the triples"))

    (-> '(:_k3 (:k2 (:_k1 ?x)))
      (t/from-justice)
      (ungensym)
      (= '(and
            [?bridge :k1 ?x]
            [?bridge :k2 ?bridge]
            [?result :k3 ?bridge]))
      (is "very deeply nested inverse get style translates to forward style"))

    (-> '(basic.main/_ancestor 1)
      (t/from-justice)
      (ungensym)
      (= '(basic.main/ancestor ?result 1))
      (is "inverted fully qualified rule names are uninverted."))

    (-> '(:entity/parent (_ancestor 1))
      (t/from-justice)
      (ungensym)
      (= '(and
            (ancestor ?bridge 1)
            [?bridge :entity/parent ?result]))
      (is "rules can be called inside triples"))

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

(deftest entity-result?-test
  (testing "can detect scalars from schema"
    (let [schema {:entity/parent {:db/valueType :db.type/ref}}
          conn (d/create-conn schema)
          rule-name 'justice.core/q
          args ['?result]]
      (let [rules '[[(justice.core/q ?result)
                     (basic.main/ancestor ?result 1)]
                    [(basic.main/ancestor ?x ?result)
                     [?x :entity/parent ?result]]
                    [(basic.main/ancestor ?x ?result)
                     [?x :entity/parent ?bridge_14745]
                     (basic.main/ancestor ?bridge_14745 ?result)]]]
        (is (= true
              (t/entity-result? @conn rules rule-name args))
          "query is for entity results"))

      (let [rules '[[(justice.core/q ?result)
                     (basic.main/ancestor ?result 1)]
                    [(basic.main/ancestor ?x ?result)
                     [?x :entity/name ?result]]
                    [(basic.main/ancestor ?x ?result)
                     [?x :entity/name ?bridge_14745]
                     (basic.main/ancestor ?bridge_14745 ?result)]]]
        (is (= true
              (t/entity-result? @conn rules rule-name args))
          "query is for ?x"))

      (let [rules '[[(justice.core/q ?result)
                     (basic.main/ancestor 1 ?result)]
                    [(basic.main/ancestor ?x ?result)
                     [?x :k ?result]]
                    [(basic.main/ancestor ?x ?result)
                     [?x :k ?bridge_14745]
                     (basic.main/ancestor ?bridge_14745 ?result)]]]
        (is (= false
              (t/entity-result? @conn rules rule-name args))
          "query is for scalar results")))))
