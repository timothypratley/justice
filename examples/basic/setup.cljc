(ns basic.setup
  (:require [datascript.core :as d]))
(def schema
  {:entity/parent {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one}}
  (def conn
    (d/create-conn schema))
  (def seed
    [{:db/id -1
      :character/name "A"}
     {:db/id -2
      :character/name "B"
      :entity/parent -1}
     {:db/id -3
      :character/name "C"
      :entity/parent -2}
     {:db/id -4
      :character/name "D"
      :entity/parent -3}])
  (d/transact! conn seed)