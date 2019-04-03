(ns basic.main
  (:require [datascript.core :as d]
            [basic.setup :as s]
            [justice.core :as j]))

(j/defrule ancestor [?x]
  (or (:entity/parent ?x)
      (ancestor (:entity/parent ?x))))

(ancestor @s/conn 1)
;=> (#:db{:id 3} #:db{:id 2})

(j/attach s/conn)
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

(map :entity/name (j/_invert [:entity/name "Justice"] ancestor))
;=> ("Good Child" "Bad Child")

(ancestor '?x 1)
;=> (#:db{:id 4} #:db{:id 5})

(map :entity/name (ancestor '?x 1))
;=> ("Good Child" "Bad Child")

(j/defrule descendant [?x]
  (or (:entity/_parent ?x)
      (descendant (:entity/_parent ?x))))

(map :entity/name (descendant 1))
;=> ("Good Child" "Bad Child")

(map :entity/name (j/_invert 1 descendant))
;=> ("Grandmother" "Mother")

(ancestor)
;=> ((#:db{:id 4} #:db{:id 3})
;    (#:db{:id 2} #:db{:id 3})
;    (#:db{:id 4} #:db{:id 2})
;    (#:db{:id 5} #:db{:id 3})
;    (#:db{:id 4} #:db{:id 1})
;    (#:db{:id 5} #:db{:id 2})
;    (#:db{:id 1} #:db{:id 3})
;    (#:db{:id 5} #:db{:id 1})
;    (#:db{:id 1} #:db{:id 2}))
(ancestor '?x '?y)

(ancestor 1 3)
;=> true

(ancestor 1 5)
;=> false


(j/defrule dead-ancestors [?x]
  (and (:entity/_death _)
       (ancestor ?x)))
(map :entity/name (dead-ancestors [:entity/name "Justice"]))
;=> ("Grandmother")

(j/defrule ancestor* [?x]
  (or [?x :entity/parent ?result]
      (and [?x :entity/parent ?z]
           (ancestor* ?z ?result))))
(map :entity/name (ancestor* 1))
;=> ("Grandmother" "Mother")
