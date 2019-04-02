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

(deftest justice-rules-test
  (testing "syntax transformations"
    (is (= '[[(f) :a]
             [(f) :b]]
           (j/datascript-rules
             '[[(f) (or :a :b)]])))
    (is (= '(or [?x :entity/parent ?result]
                (and [?bridge :entity/parent ?result]
                     (ancestor ?x ?bridge)))
           (ungensym
             (j/datascript-rule-body
               '(or (:entity/parent ?x)
                    (:entity/parent (ancestor ?x))))))))
  ;; TODO: this doesn't work because the test gets called in the `user` namespace
  (testing "registering rules"
    (is (= '{"justice.core-test/ancestor"
             [[(justice.core-test/ancestor ?x ?result)
               [?x :entity/parent ?result]]
              [(justice.core-test/ancestor ?x ?result)
               [?bridge :entity/parent ?result]
               (justice.core-test/ancestor ?x ?bridge)]]}
           (ungensym
             (j/register-rule "justice.core-test/ancestor" '[?x]
                              '(or (:entity/parent ?x) (:entity/parent (justice.core-test/ancestor ?x)))))))))

(deftest justice-macro-expansion
  (testing "macro expands to expected definitions"
    ;; TODO: this doesn't work because the test gets called in the `user` namespace
    #_(is
      (= '(do
            (justice.core/register-rule "justice.core-test/ancestor" '[?x]
                                        '(or (:entity/parent ?x)
                                             (:entity/parent (basic.main/ancestor ?x))))
            (clojure.core/defn ancestor "Docstring"
              ([] (justice.core/apply-rule "justice.core-test/ancestor"))
              ([a] (justice.core/apply-rule "justice.core-test/ancestor" a))
              ([a b] (justice.core/apply-rule "justice.core-test/ancestor" a b))
              ([db a b]
               (justice.core/apply-rule "justice.core-test/ancestor" db a b))))

         (macroexpand-1
           `(j/defrule ancestor "Docstring" [?x]
              (or (:entity/parent ?x)
                  (:entity/parent (ancestor ?x)))))))))

(deftest justice-usage
  (testing "defrule behaves like defn"
    (is (function? @(j/defrule ancestor [?x]
                      (or (:entity/parent ?x)
                          (:entity/parent (ancestor ?x))))))
    (is (contains? j/*rule-registry* "justice.core-test/ancestor"))
    (is (empty? (ancestor (d/empty-db) '?x '?y)))))
