(ns basic.setup
  (:require [datascript.core :as d]))

;; TODO: Clearly people have two parents :(
(def schema
  {:entity/parent {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one}
   :entity/name {:db/unique :db.unique/identity}})

(def conn
  (d/create-conn schema))

(def seed
  [{:db/id -1
    :entity/name "Justice"
    :entity/parent -2}
   {:db/id -2
    :entity/name "Mother"
    :entity/parent -3}
   {:db/id -3
    :entity/name "Grandmother"
    :entity/death "278 BC"}
   {:db/id -4
    :entity/name "Good Child"
    :entity/parent -1}
   {:db/id -5
    :entity/name "Bad Child"
    :entity/parent -1}])

(d/transact! conn seed)
