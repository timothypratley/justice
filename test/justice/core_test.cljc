(ns justice.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [datascript.core :as d]
            [justice.core :as j]))

(deftest register-rule-test
  (testing "registering rules"
    (j/register-rule 'test-registration/ancestor '[?x]
      '(or (:entity/parent ?x) (justice.core-test/ancestor (:entity/parent ?x))))
    (is (contains? (set (keys j/*rule-registry*)) 'test-registration/ancestor))
    (is (thrown? #?(:clj Error :cljs js/Error)
          (j/register-rule 'rule-name '[?x]
            '(or (:entity/parent ?x) (justice.core-test/ancestor (:entity/parent ?x))))))))

(deftest defrule-test
  (testing "defrule behaves like defn"
    (j/defq ancestor [?x]
      (or (:entity/parent ?x)
        (ancestor (:entity/parent ?x))))
    (is (fn? ancestor))
    (is (contains? (set (keys j/*rule-registry*)) 'justice.core-test/ancestor))
    (is (empty? (ancestor (d/empty-db) '?x '?y)))
    (let [conn (d/create-conn {:entity/parent {:db/valueType :db.type/ref
                                               :db/cardinality :db.cardinality/one}
                               :entity/name {:db/unique :db.unique/identity}})]
      (d/transact! conn [{:entity/name "Child"
                          :entity/parent {:entity/name "Parent"}}])
      (is (seq (ancestor @conn 1)))
      (j/with-conn conn
        (is (seq (ancestor 1)))))))

(deftest trace-test
  (testing "trace prints the underlying query"
    (is (re-find #"QUERY"
                 (with-out-str
                   (j/trace (j/rule-query (d/empty-db) '[[(foo)]] 'foo [])))))))
