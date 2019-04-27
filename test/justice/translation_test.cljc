(ns justice.translation-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.translation :as t]
            [clojure.string :as string]
            [datascript.core :as d]
            [meander.strategy.gamma :as m]))

(def ungensym
  (t/rewrite-all
    (and ?gensym
      (guard (and (symbol? ?gensym)
               (re-matches #"\?.*_\d+" (name ?gensym)))))
    ~(symbol (first (string/split (name ?gensym) #"_")))))

(deftest uninverse-test
  (testing "symbols and keywords from inverse to forward style"
    (is (= :k (@#'t/inverse :_k)))
    (is (= :_k (@#'t/inverse :k)))
    (is (= :a/k (@#'t/inverse :a/_k)))
    (is (= :a/_k (@#'t/inverse :a/k)))
    (is (= 'ancestor (@#'t/inverse '_ancestor)))
    (is (= '_ancestor (@#'t/inverse 'ancestor)))
    (is (= 'foo.bar/ancestor (@#'t/inverse 'foo.bar/_ancestor)))
    (is (= 'foo.bar/_ancestor (@#'t/inverse 'foo.bar/ancestor)))))

(deftest pattern-matching-expansions-test
  (testing "pattern matching syntax"

    (-> '{:foo/bar ?x}
      (t/from-justice)
      (= '[?result :foo/bar ?x])
      (is "justice translates entity pattern matching style to bridged triples"))

    (-> '{:k4 {:k3 {:k2 {:k1 ?x}}}}
      (t/from-justice)
      (ungensym)
      (= '(and
            [?bridge :k1 ?x]
            [?bridge :k2 ?bridge]
            [?bridge :k3 ?bridge]
            [?result :k4 ?bridge]))
      (is "justice translates deeply nested patterns"))

    (-> '{:k2 {:k1 ?x}
          :k4 {:k3 ?x}}
      (t/from-justice)
      (ungensym)
      (= '(and
            [?bridge :k1 ?x]
            [?result :k2 ?bridge]
            [?bridge :k3 ?x]
            [?result :k4 ?bridge]))
      (is "justice translates function application style to bridged triples"))

    (-> '{:foo/bar ?result}
      (t/from-justice)
      (= '[_ :foo/bar ?result])
      (is "justice allows using a different special ?result target, base is removed"))

    (-> '{:entity/parent {:entity/parent [:entity/name "Grandmother"]}
          :entity/name ?result}
      (t/from-justice)
      (ungensym)
      (= '(and
            [?bridge :entity/parent [:entity/name "Grandmother"]]
            [?justice-pattern-base :entity/parent ?bridge]
            [?justice-pattern-base :entity/name ?result]))
      (is "justice handles nested use of ?result, where base cannot be removed"))))

(deftest datascript-rule-expansions-test
  (testing "syntax transformations from justice to datascript"

    (-> (t/datascript-rule 'translation-test/rule-name '[?x] '(:entity/parent ?x))
      (= '[(translation-test/rule-name ?x ?result)
           [?x :entity/parent ?result]])
      (is "justice 'get keyword' style translates to Datascript 'relate to result' style"))

    (-> '(:foo/bar 1)
      (t/from-justice)
      (= '[1 :foo/bar ?result])
      (is "justice translates function application style to bridged triples"))

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

(deftest as-rules-test
  (is (= '[[(justice.translation-test/q
             ?result)
            [1 :entity/parent ?result]]]
        (t/as-rules `q [] '(:entity/parent 1)))))

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

(deftest rearrange-logic-test
  (testing "Logic rearrangement and simplification to match Datalog `or` on outside rule convention"
    (-> '(:a :b :c (:d :e))
      (#'t/rearrange-logic)
      (= '(:a :b :c (:d :e)))
      (is "non-logic expressions are preserved"))

    (-> '(and :a (and :b) :c)
      (#'t/rearrange-logic)
      (= '(and :a :b :c))
      (is "nested commutative logic is raised"))

    (-> '(or :a (or :b :c) :d)
      (#'t/rearrange-logic)
      (= '(or :a :b :c :d))
      (is "nested commutative logic is raised"))

    (-> '(or :a (and :b) :c)
      (#'t/rearrange-logic)
      (= '(or :a :b :c))
      (is "identity logic expressions are flattened"))

    (-> '(and :a (or :b) :c)
      (#'t/rearrange-logic)
      (= '(and :a :b :c))
      (is "idenity logic expressions are flattened"))

    (-> '(or :a (and :b :c) :d)
      (#'t/rearrange-logic)
      (= '(or :a (and :b :c) :d))
      (is "keeps `or` on the outside, and `and` on the inside to match Datalog rule convention"))

    (-> '(and :a (or :b :c) :d)
      (#'t/rearrange-logic)
      (= '(or
            (and :a :b :d)
            (and :a :c :d)))
      (is "moves `or` to the outside, and `and` to the inside to match Datalog rule convention"))

    (-> '(not (not :a))
      (#'t/rearrange-logic)
      (= :a)
      (is "double negatives are removed"))

    (-> '(not (or :a :b))
      (#'t/rearrange-logic)
      (= '(and (not :a) (not :b)))
      (is "moves `or` to the outside, and `not` to the inside to match Datalog rule convention"))

    (-> '(not (and :a :b))
      (#'t/rearrange-logic)
      (= '(or (not :a) (not :b)))
      (is "moves `and` to the outside, and `not` to the inside to match Datalog rule convention"))

    (-> '(not (not :a))
      (#'t/rearrange-logic)
      (= :a)
      (is "double negatives are removed"))

    ;; TODO: moving not inside (and complex)


    (-> '(and :c (or :d (not (not (:a :b)))))
      (#'t/rearrange-logic)
      (= '(or
            (and :c :d)
            (and :c (:a :b))))
      (is "complex combinations are resolved"))))
