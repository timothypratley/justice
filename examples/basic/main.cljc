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
#_(j/defq ancestor [?x]
  (or
    (-> ?x :entity/parent)
    (-> ?x :entity/parent ancestor)))

(j/q @s/conn {:entity/parent 1})
;=> (#:db{:id 4} #:db{:id 5})

(j/attach s/conn)

;; TODO: this should be possible? (short-cut for d/entity db id)
#_(j/q 1)
#_(j/q [:entity/name "Justice"])
#_(j/q {:db/id 1})

(j/q {:entity/parent 1})
;=> (#:db{:id 4} #:db{:id 5})

(map :entity/name (j/q {:entity/parent 1}))
;=> ("Good Child" "Bad Child")

(j/q {:entity/parent [:entity/name "Justice"]})

(j/q {:entity/parent {:db/id 1}})

(j/q {:entity/parent 1
      :entity/name "Good Child"})
;=> (#:db{:id 4})

(j/q '{:entity/name ?result})
;=> ["Good Child" "Grandmother" "Bad Child" "Justice" "Mother"]

(j/q '{:entity/parent {:entity/parent {:entity/name "Grandmother"}}
       :entity/name ?result})

(j/q '{:entity/parent {:entity/parent [:entity/name "Grandmother"]}
       :entity/name ?result})
;=> ["Justice"]

(j/trace
  (j/q '{:entity/name "Justice"
         :entity/parent {:entity/parent {:entity/name "Grandmother"}
                         :entity/name ?result}}))


(j/q '(:entity/name {:entity/parent 1}))

(j/q '(:entity/name _))


;; TODO:
(j/defq desc [?x]
  {:entity/parent (or ?x (desc ?x))})

(j/trace
  (desc 1))

;; TODO: this should be possible:
(j/trace
  (j/q {:entity/name '?result
        :entity/parent 1}))

(j/defq ancestor [?x]
   (or (:entity/parent ?x)
       (ancestor (:entity/parent ?x))))
(ancestor @s/conn 1)
;=> (#:db{:id 3} #:db{:id 2})

(j/defq anc [?x]
  (or
    {:entity/parent ?result}
    (anc {:entity/parent ?result})))

(j/defq des [?x]
  {:entity/parent (or ?x (des ?x))})

(j/defq des [?x]
  (or
    {:entity/parent ?x}
    (des {:entity/parent ?x})))

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

(j/defq descendant [?x]
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

;; Get all entities:
(j/q '(:entity/_name _))
;=> (#:db{:id 4} #:db{:id 3} #:db{:id 5} #:db{:id 2} #:db{:id 1})

(j/q '(:entity/name _))
;=> ["Good Child" "Grandmother" "Bad Child" "Justice" "Mother"]

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

(j/defq dead-ancestors [?x]
  (and (:entity/_death _)
       (ancestor ?x)))
(map :entity/name (dead-ancestors [:entity/name "Justice"]))
;=> ("Grandmother")

(j/defq ancestor* [?x]
  (or [?x :entity/parent ?result]
      (and [?x :entity/parent ?z]
           (ancestor* ?z ?result))))
(map :entity/name (ancestor* 1))
;=> ("Grandmother" "Mother")
