(ns justice.dependencies-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.dependencies :as dependencies]))

(deftest calculate-rule-dependencies-test
  (let [registry '{bar [[(bar ?result)
                         [1 :a ?result]]]
                   foo [[(foo ?x ?result)
                         [1 :a ?x]
                         (bar ?x)]]
                   baz [[(baz ?x ?result)
                         (foo ?x ?result)]]
                   booz [[(booz ?x ?result)
                          [?x :a (baz ?result)]]]}]
    (testing "Rule dependency calculation"
      (is (= '{foo #{bar}
               bar #{}
               baz #{foo bar}
               booz #{foo bar baz}}
            (dependencies/calculate-rule-dependencies registry))
        "bar depends on foo, foo has no dependencies"))

    (testing "Correct rules are returned from the registry"
      (is (= '[[(bar ?result)
                [1 :a ?result]]]
            (dependencies/relevant-rules 'bar registry))
        "when querying bar, we only need bar")
      (is (= '[[(bar ?result)
                [1 :a ?result]]
               [(foo ?x ?result)
                [1 :a ?x]
                (bar ?x)]]
            (dependencies/relevant-rules 'foo registry))
        "when querying foo, we need foo and bar"))))
