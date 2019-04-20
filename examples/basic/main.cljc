(ns basic.main
  (:require [datascript.core :as d]
            [basic.setup :as s]
            [justice.core :as j]))

;; TODO: react components that receive entities and navigators

;; TODO: maybe DCG??
#_(j/def ancestor
    (or (--> :entity/parent)
        (--> :entity/parent ancestor)))

;; TODO: support threading (maybe all macros??), doto? probably not
#_(j/defrule ancestor [?x]
  (or
    (-> ?x :entity/parent)
    (-> ?x :entity/parent ancestor)))

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

(j/defrule descendant [?x]
  (or (:entity/_parent ?x)
      (descendant (:entity/_parent ?x))))

(map :entity/name (descendant 1))
;=> ("Good Child" "Bad Child")

(j/q '(:entity/parent (:entity/parent 1)))
;=> (#:db{:id 3})

(:entity/parent (:entity/parent (d/entity @s/conn 1)))
;=> #:db{:id 3}

(j/q '(basic.main/ancestor (basic.main/descendant 1)))
;=> (#:db{:id 3} #:db{:id 2} #:db{:id 1})

(j/q '(and
        (:entity/parent 1)
        (:entity/_parent 3)))
;=> (#:db{:id 2})

(j/q `(ancestor 1))
;=> (#:db{:id 4} #:db{:id 5})

(j/q `(ancestor [:entity/name "Justice"]))
;=> (#:db{:id 3} #:db{:id 2})

(j/q '[?result :entity/parent [:entity/name "Justice"]])
;=> (#:db{:id 4} #:db{:id 5})

(map :entity/name (j/q '(basic.main/_ancestor 1)))
;=> ("Good Child" "Bad Child")

(ancestor '?x 1)
;=> (#:db{:id 4} #:db{:id 5})

(map :entity/name (ancestor '?x 1))
;=> ("Good Child" "Bad Child")

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

(ancestor '?x '?result)
; same as before

(->>
  (ancestor)
  (map (partial map :entity/name))
  (sort-by first))
;=> (("Bad Child" "Grandmother")
;    ("Bad Child" "Mother")
;    ("Bad Child" "Justice")
;    ("Good Child" "Grandmother")
;    ("Good Child" "Mother")
;    ("Good Child" "Justice")
;    ("Justice" "Grandmother")
;    ("Justice" "Mother")
;    ("Mother" "Grandmother"))

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
