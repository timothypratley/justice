(ns justice.translation-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.translation :as t]
            [datascript.core :as d]))

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

(deftest t
  (testing "pattern matching"
    (-> '{:k1 "foo"
          :k2 {:k3 "bar"
               :k4 "baz"}}
        (#'t/map-terms)
        (t/with-incremental-gensym)
        (= '(and
             [?e1 :k1 "foo"]
             [?e1 :k2 ?e2]
             [?e2 :k3 "bar"]
             [?e2 :k4 "baz"]))
        (is "justice handles nested complex maps"))

    (-> '{:entity/name "Justice"
          :entity/parent {:entity/parent {:entity/name "Grandmother"}
                          :entity/name ?result}}
        (t/from-justice)
        (= '(and
             [?e1 :entity/name "Justice"]
             [?e1 :entity/parent ?e2]
             [?e2 :entity/parent ?e3]
             [?e3 :entity/name "Grandmother"]
             [?e2 :entity/name ?result]))
        (is "justice handles nesting up and nesting down"))))

(deftest pattern-matching-expansions-test
  (testing "pattern matching syntax"

    (-> '{:foo/bar ?x}
      (t/from-justice)
      (= '[?result :foo/bar ?x])
      (is "justice translates entity pattern matching style to bridged triples"))

    (-> '{:k4 {:k3 {:k2 {:k1 ?x}}}}
      (t/from-justice)
      (= '(and
           [?result :k4 ?e2]
           [?e2 :k3 ?e3]
           [?e3 :k2 ?e4]
           [?e4 :k1 ?x]))
      (is "justice translates deeply nested patterns"))

    (-> '{:k2 {:k1 ?x}
          :k4 {:k3 ?x}}
      (t/from-justice)
      (= '(and
           [?result :k2 ?e2]
           [?e2 :k1 ?x]
           [?result :k4 ?e3]
           [?e3 :k3 ?x]))
      (is "justice translates function application style to bridged triples"))

    (-> '{:foo/bar ?result}
      (t/from-justice)
      (= '[?e1 :foo/bar ?result])
      (is "justice allows using a different special ?result target"))

    (-> '{:entity/parent {:entity/parent [:entity/name "Grandmother"]}
          :entity/name ?result}
      (t/from-justice)
      (= '(and
           [?e1 :entity/parent ?e2]
           [?e2 :entity/parent [:entity/name "Grandmother"]]
           [?e1 :entity/name ?result]))
      (is "justice handles nested use of ?result, where base cannot be removed"))))

(deftest datascript-rule-expansions-test
  (testing "syntax transformations from justice to datascript"

    (-> (t/datascript-rule 'translation-test/rule-name '[?x] '(:entity/parent ?x))
      (= '[(translation-test/rule-name ?x ?result)
           [?x :entity/parent ?result]])
      (is "justice 'get keyword' style translates to Datascript 'relate to result' style"))

    (-> '(:k1 {:k2 "foo"})
        (t/from-justice)
        (= '(and [?e2 :k2 "foo"]
                 [?e2 :k1 ?result]))
        (is "justice handles both function application style with pattern style"))

    (-> '{:k1 (:k2 ?x)}
        (t/from-justice)
        (= '(and [?result :k1 ?v1]
                 [?x :k2 ?v1]))

        (is "justice handles both function application style with pattern style"))

    (-> '{:a (:k1 (:k2 {}))}
        (t/from-justice)
        (= '(and [?e2 :k1 ?x]
                 [?e1 :k2 ?z]
                 [?result :a ?x]))
        (is "justice handles extracting nested attribute references"))

    (-> '{:a (:k {:k ?x})}
        (t/from-justice)
        (= '(and [?e1 :k ?x]
                 [?result :a ?x]))
        (is "justice handles extracting an existing attribute"))

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
      (= '(and
            [?x :k1 ?e2]
            [?e2 :k2 ?result]))
      (is "nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(:k3 (:k2 (:k1 ?x)))
      (t/from-justice)
      (= '(and
            [?x :k1 ?e2]
            [?e2 :k2 ?e3]
            [?e3 :k3 ?result]))
      (is "deeply nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(:k4 (:k3 (:k2 (:k1 ?x))))
      (t/from-justice)
      (= '(and
            [?x :k1 ?e2]
            [?e2 :k2 ?e3]
            [?e3 :k3 ?e4]
            [?e4 :k4 ?result]))
      (is "very deeply nested 'get keyword' style translates to 'bridged triples'"))

    (-> '(or (:entity/parent ?x)
           (ancestor (:entity/parent ?x)))
      (t/from-justice)
      (= '(or [?x :entity/parent ?result]
            (and
              [?x :entity/parent ?e3]
              (ancestor ?e3 ?result))))
      (is "expressions nested in logic expand with a bridge"))

    (-> '(or (:entity/_parent ?x)
           (descendant (:entity/_parent ?x)))
      (t/from-justice)
      (= '(or [?result :entity/parent ?x]
            (and
              [?e3 :entity/parent ?x]
              (descendant ?e3 ?result))))
      (is "reverse traversal inverts the triples"))

    (-> '(:_k3 (:k2 (:_k1 ?x)))
      (t/from-justice)
      (= '(and
            [?e2 :k1 ?x]
            [?e2 :k2 ?e3]
            [?result :k3 ?e3]))
      (is "very deeply nested inverse get style translates to forward style"))

    (-> '(basic.main/_ancestor 1)
      (t/from-justice)
      (= '(basic.main/ancestor ?result 1))
      (is "inverted fully qualified rule names are uninverted."))

    (-> '(:entity/parent (_ancestor 1))
      (t/from-justice)
      (= '(and
            (ancestor ?e2 1)
            [?e2 :entity/parent ?result]))
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
                     [?x :entity/parent ?e1]
                     (basic.main/ancestor ?e1 ?result)]]]
        (is (= true
              (t/entity-result? @conn rules rule-name args))
          "query is for entity results"))

      (let [rules '[[(justice.core/q ?result)
                     (basic.main/ancestor ?result 1)]
                    [(basic.main/ancestor ?x ?result)
                     [?x :entity/name ?result]]
                    [(basic.main/ancestor ?x ?result)
                     [?x :entity/name ?e1]
                     (basic.main/ancestor ?e1 ?result)]]]
        (is (= true
              (t/entity-result? @conn rules rule-name args))
          "query is for ?x"))

      (let [rules '[[(justice.core/q ?result)
                     (basic.main/ancestor 1 ?result)]
                    [(basic.main/ancestor ?x ?result)
                     [?x :k ?result]]
                    [(basic.main/ancestor ?x ?result)
                     [?x :k ?e1]
                     (basic.main/ancestor ?e1 ?result)]]]
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

#_(deftest maybe-replace-base-clause-test
  (testing "Base is ?result if no ?result specified"
    (-> '{:a {:b ?result}}
        (#'t/maybe-replace-base-clause)
        (= '{:a {:b ?result}})
        (is "When ?result is specified, nothing happens"))
    (-> '{:a {:b 1}}
        (#'t/maybe-replace-base-clause)
        (= '{:a {:b ?result}
             :db/id ?result})
        (is "When ?result is not specified, and the base expression is an entity, result? will be the entity id"))
    (-> '{:a {:b 1}
          :db/id ?foo}
        (#'t/maybe-replace-base-clause)
        (->> (thrown? 1))
        (is "Cannot determine what ?result should be"))
    (-> '(:a {:a {:b 1}})
        (#'t/maybe-replace-base-clause)
        (= '{:a {:b 1
                 :db/id ?result}})
        ;; TODO: maybe this happens first
        (is "Function call style implies an entity (or value)..."))))

#_(deftest ttt
  ;; depends if k3 is value or entity :(
  (-> '(:k3 (:k2 (:k1 ?x)))
      (#'t/f2m)
      (= '{:db/id ?result
           :k3 {:k2 {:k1 ?x}}}
         '(and
           [?x :k1 ?e2]
           [?e2 :k2 ?e3]
           [?e3 :k3 ?result]))
      (is "deeply nested 'get keyword' style translates to 'bridged triples'")))

(deftest entity-clauses-test
  (-> '{:db/id ?result
        :size 1
        :color "blue"}
      (#'t/entity-clauses)
      (= '[[?result :size 1]
           [?result :color "blue"]])
      (is "A flat entity creates a clause for every property"))
  (-> '{:db/id ?result
        :parent {:size 1
                 :color "blue"}}
      (#'t/entity-clauses)
      (t/with-incremental-gensym)
      (= '[[?result :parent ?e1]
           [?e1 :size 1]
           [?e1 :color "blue"]])
      (is "A nested entity is flattened to clauses"))
  (-> '{:db/id ?result
        :parent {:color "blue"
                 :parent {:color "green"}}}
      (#'t/entity-clauses)
      (t/with-incremental-gensym)
      (= '[[?result :parent ?e1]
           [?e1 :color "blue"]
           [?e1 :parent ?e2]
           [?e2 :color "green"]])
      (is "A deeply nested entity is flattened to clauses")))
