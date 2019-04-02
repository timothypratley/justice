(ns basic.setup
  (:require [datascript.core :as d]))

(def schema
  {:entity/parent {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one}
   :entity/name {:db/unique :db.unique/identity}})

(def conn
  (d/create-conn schema))

(def seed
  [{:entity/name "Justice"
    :entity/parent {:entity/name "Mother"
                    :entity/parent {:entity/name "Grandmother"
                                    :entity/death "278 BC"}}
    :entity/_parent [{:entity/name "Good Child"}
                     {:entity/name "Bad Child"}]}])

(d/transact! conn seed)
