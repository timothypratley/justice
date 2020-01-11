(ns justice.translation-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.translation :as t]
            [datascript.core :as d]))

(deftest nested-map-patterns-expand-test
  "Justice expands from nested map patterns to EAV triple clauses"

  (testing "Given a nested pattern map"
    (-> '{:k1 "foo"
          :k2 {:k3 "bar"
               :k4 "baz"}}
        (t/from-justice)
        (= '(and
             [?result :k1 "foo"]
             [?result :k2 ?e2]
             [?e2 :k3 "bar"]
             [?e2 :k4 "baz"]))
        (is "from-justice returns EAV triple clauses")))

  (testing "Given a nested pattern map"
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
    (testing "Given lookup syntax")
    (-> '(:entity/parent ?x)
        (t/from-justice)
        (= '[?x :entity/parent ?result])
        (is "justice produces linked EAV triples"))

    (-> (t/datascript-rule 'translation-test/rule-name '[?x] '(:entity/parent ?x))
      (= '[(translation-test/rule-name ?x ?result)
           [?x :entity/parent ?result]])
      (is "justice 'get keyword' style translates to Datascript 'relate to result' style"))

    (-> '(:k1 {:k2 "foo"})
        (t/from-justice)
        (= '(and [?e1 :k1 ?result]
                 [?e1 :k2 "foo"]))
        (is "justice handles both function application style with pattern style"))

    (-> '{:k1 (:k2 ?x)}
        (t/from-justice)
        (= '(and [?result :k1 ?v1]
                 [?x :k2 ?v1]))
        (is "justice handles both function application style with pattern style"))

    (-> '{:a (:k1 (:k2 {}))}
        (t/from-justice)
        (= '(and [?result :a ?v1]
                 [?v2 :k1 ?v1]
                 [?e5 :k2 ?v2]))
        (is "justice handles extracting nested attribute references"))

    (-> '{:a (:k {:k ?x})}
        (t/from-justice)
        (= '(and [?result :a ?x]
                 [?e2 :k ?x]))
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
        (#'t/imply-result)
        (= '{:a {:b ?result}})
        (is "When ?result is specified, nothing happens"))
    (-> '{:a {:b 1}}
        (#'t/imply-result)
        (= '{:a {:b ?result}
             :db/id ?result})
        (is "When ?result is not specified, and the base expression is an entity, result? will be the entity id"))
    (-> '{:a {:b 1}
          :db/id ?foo}
        (#'t/imply-result)
        (->> (thrown? 1))
        (is "Cannot determine what ?result should be"))
    (-> '(:a {:a {:b 1}})
        (#'t/imply-result)
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

#_
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
      (= '[[?result :parent ?e1]
           [?e1 :size 1]
           [?e1 :color "blue"]])
      (is "A nested entity is flattened to clauses"))
  (-> '{:db/id ?result
        :parent {:color "blue"
                 :parent {:color "green"}}}
      (#'t/entity-clauses)
      (= '[[?result :parent ?e1]
           [?e1 :color "blue"]
           [?e1 :parent ?e2]
           [?e2 :color "green"]])
      (is "A deeply nested entity is flattened to clauses")))

;; TODO: how to test with dynamic
(deftest lookups-as-maps-test
  (testing "Lookup :r from entity 1"
    (-> '(:r 1)
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (= '{:db/id 1
             :r ?v1})
        (is "implies entity 1 has :r")))

  (testing "Lookup :r of an entity with :k1 foo"
    (-> '(:r {:k1 "foo"})
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (= '{:k1 "foo"
             :r ?v1})
        (is "implies an entity with :k1 foo")))

  (testing "Lookup :r1 from the :r2 of an entity with :k1 foo"
    (-> '(:r1 (:r2 {:k1 "foo"}))
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (= '{:k1 "foo"
             :r2 {:r1 ?v1}})
        (is "implies 2 related entities, one with :k1 foo and :r2 which relates to an enity with a :r1 to extract")))

  (testing "Lookup :r1 from the :r2 of an entity with :k1 foo"
    (-> '(:r1 (:r2 (:r3 {:k1 "foo"})))
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (= '{:k1 "foo"
             :r3 {:r2 {:r1 ?v1}}})
        (is "implies 2 related entities, one with :k1 foo and :r2 which relates to an enity with a :r1 to extract")))

  (testing "Look for an entity with :k1 equal to the :r of entity 1"
    (-> '{:k1 (:r 1)}
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (= '(and {:k1 ?v1}
                 {:db/id 1
                  :r ?v1}))
        (is "implies 2 entities, 1 and the result, linked by :r")))

  (testing "Look for an entity with :k1 equal to the :r of an entity having :k2 foo"
    (-> '{:k1 (:r {:k2 "foo"})}
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (= '(and {:k1 ?v1}
                 {:r ?v1
                  :k2 "foo"}))
        (is "implies 2 entities, 1 and the result, linked by :r")))

  (testing "Look for an entity with :k1 equal to the :r1 of the :r2 of entity 1"
    (-> '{:k1 (:r1 (:r2 1))}
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (@#'t/rearrange-logic)
        (= '(and {:k1 ?v1}
                 {:db/id ?v2
                  :r1 ?v1}
                 {:db/id 1
                  :r2 ?v2}))
        (is "implies 3 entities, 1 the result, and a bridge linked by :r1 :r2")))

  (testing "Look for an entity with :k1 equal to the :r1 of the :r2 of entity with :k2 foo"
    (-> '{:k1 (:r1 (:r2 {:k2 "foo"}))}
        (@#'t/lookups-as-maps)
        (t/with-incremental-gensym)
        (@#'t/rearrange-logic)
        (= '(and {:k1 ?v1}
                 {:db/id ?v2
                  :r1 ?v1}
                 {:k2 "foo"
                  :r2 ?v2}))
        (is "implies two entities, 1 the result, and a bridge linked by :r1 :r2"))))

(deftest invert-test
  (testing "Justice inverts between to/from forward/backward reference style"
    (is (= :k (@#'t/invert :_k)))
    (is (= :_k (@#'t/invert :k)))
    (is (= :a/k (@#'t/invert :a/_k)))
    (is (= :a/_k (@#'t/invert :a/k)))
    (is (= 'ancestor (@#'t/invert '_ancestor)))
    (is (= '_ancestor (@#'t/invert 'ancestor)))
    (is (= 'foo.bar/ancestor (@#'t/invert 'foo.bar/_ancestor)))
    (is (= 'foo.bar/_ancestor (@#'t/invert 'foo.bar/ancestor)))))

(deftest collect-lookups-test
  (testing "Given lookup style on a target"
    (-> '(:k ?x)
        (@#'t/collect-lookups '?v1)
        (= '{:db/id ?x
             :k ?v1})
        (is "TODO: target be a value as well as entity???")))
  (testing "Given function style lookup syntax"
    (-> '(:r1 (:r2 (:r3 {:k4 "foo"})))
        (@#'t/collect-lookups '?v1)
        (= '{:k4 "foo"
             :r3 {:r2 {:r1 ?v1}}})
        (is "collected into a nested map pattern")))
  (testing "Given a lookup from a rule"
    (-> '(:r1 (ancestor 1))
        (@#'t/collect-lookups '?v1)
        (= '(:r1 (ancestor 1)))
        (is "rule invocation is preserved")))
  #_(testing "Given a logic expression containing lookup syntax children"
    (-> '(and (:r2 ?x) (:r3 ?y))
        (@#'t/collect-lookups '?result)
        (= '{:db/id ?result
             :r2 ?x
             :r3 ?y})
        (is "the map expression preserves the logic"))))

(deftest ensure-result-test
  (testing "Given a datalog expression"
    (-> '(and [?e2 :k2 "foo"]
              [?e2 :k1 ?v1])
        (@#'t/imply-result)
        (= '(and [?e2 :k2 "foo"]
                 [?e2 :k1 ?result]))
        (is "?v1 replaced by ?result")))
  (testing "Given a map with no explicit ?result"
    (-> '{:k1 (:k2 ?x)}
        (@#'t/imply-result)
        (= '{:db/id ?result
             :k1 (:k2 ?x)})
        (is "The pattern is the result"))))

(deftest expand-map-to-tripples-test
  (testing "Given a single entity with single attribute"
    (-> '{:entity/parent ?result, :db/id ?x}
        (@#'t/expand-maps-to-triples)
        (@#'t/rearrange-logic)
        (= '[?x :entity/parent ?result])
        (is "Justice produces a single EAV triple"))))

(deftest occurs-in?-test
  (testing "Given ?result in a map"
    (-> (@#'t/occurs-in? '?result '{:foo ?result})
        (is "occurs-in? found it"))))
