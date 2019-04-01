(ns basic.main
  (:require [datascript.core :as d]
            [basic.setup :as s]
            [justice.core :as j]))

(j/defrule ancestor [?x]
  (or (:entity/parent ?x)
      (:entity/parent (ancestor ?x))))

(ancestor @s/conn 1)
;=> (#:db{:id 3} #:db{:id 2})

(j/register s/conn)
(ancestor 1)
;=> (#:db{:id 3} #:db{:id 2})

(map :entity/name (ancestor 1))
;=> ("Grandmother" "Mother")

(map d/touch (ancestor 1))
;=> ({:entity/death "278 BC", :entity/name "Grandmother", :db/id 3}
;    {:entity/name "Mother", :entity/parent #:db{:id 3}, :db/id 2})

(ancestor [:entity/name "Justice"])
;=> (#:db{:id 3} #:db{:id 2})

(ancestor {:db/id 1})
;=> (#:db{:id 3} #:db{:id 2})

(ancestor #:db{:id 1})
;=> (#:db{:id 3} #:db{:id 2})

;;(ancestor ?x 1)
;=> TODO!!!

(j/defrule descendant [?x]
  (or (:entity/_parent ?x)
      (:entity/_parent (descendant ?x))))
(descendant [:entity/name "Justice"])
;=> TODO!!!

(ancestor)
;=> TODO!!!

(j/defrule dead-ancestors [?x]
         (and (:entity/_death ?x)
              (ancestor ?x)))
(dead-ancestors [:entity/name "Justice"])
;=> TODO!!!
