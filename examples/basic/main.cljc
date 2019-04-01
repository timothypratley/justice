(ns basic.main
  (:require [justice.core :as j]))

;; TODO: how to run these?

(j/defrule ancestor [?x]
  (or (:entity/parent ?x)
      (:entity/parent (ancestor ?x))))

(ancestor @conn 1) ;=> [{:db/id 2} {:db/id 3}]
(j/register conn)
(ancestor 1) ;=> [{:db/id 2} {:db/id 3}]
(map :entity/name (ancestor 1))
;=> ("A", "B", "C")
(ancestor [:entity/name "Justice"])
;=> ("A", "B", "C")
(ancestor {:db/id 1})
;=> ("A", "B", "C")
(ancestor ?x [:entity/name "Justice"])
;=> [{:db/id 4}]
(defrule descendant [?x]
         (or (:entity/_parent ?x)
             (:entity/_parent (descendant ?x))))
(descedant [:entity/name "Justice"])
;=> [{:db/id 4}]
(ancestor)
;=> [[{:db/id 1} {:db/id 2}]
[{:db/id 1} {:db/id 3}]]
(defrule dead-ancestors [?x]
         (and (:entity/_death ?x)
              (ancestor ?x)))
(dead-ancestors [:entity/name "Justice"])
;=> ("A", "B")
