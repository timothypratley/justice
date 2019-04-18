(ns justice.reactive-test
  (:require [clojure.test :refer [deftest testing is]]
            [datascript.core :as d]
            [justice.reactive :as r]))

(deftest rentity-test
  (let [state (atom nil)
        conn (d/create-conn)
        on-change #(reset! state (:foo %))
        cleanup (r/rentity conn 1 on-change)]
    (d/transact! conn [{:db/id 1
                        :foo "bar"}])
    (is (= @state "bar")
      "on-change should cause state to be set by the transaction.")
    (cleanup)
    (d/transact! conn [{:db/id 1
                        :foo "baz"}])
    (is (= @state "bar")
      "after cleanup, no further changes are reported.")))

(deftest rdbfn-test
  (let [state (atom nil)
        conn (d/create-conn)
        f #(:foo (d/entity % 1))
        on-change #(reset! state %)
        cleanup (r/rdbfn conn f identity on-change)]
    (d/transact! conn [{:db/id 1
                        :foo "bar"}])
    (is (= @state "bar")
      "on-change should cause state to be set to the result of f.")
    (cleanup)
    (d/transact! conn [{:db/id 1
                        :foo "baz"}])
    (is (= @state "bar")
      "after cleanup, no further changes are reported.")))
