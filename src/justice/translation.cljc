(ns justice.translation
  "Translates Justice map and function call expressions to Datalog triple queries"
  #?(:cljs (:require-macros [justice.translation :refer [bottom-up top-down with-incremental-gensym]]))
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [meander.strategy.gamma :as m]))

#?(:clj
   (defmacro bottom-up [& body]
     `(m/until = (m/bottom-up (m/attempt (m/rewrite ~@body))))))
#?(:clj
   (defmacro top-down [& body]
     `(m/until = (m/top-down (m/attempt (m/rewrite ~@body))))))
#?(:clj
   (defmacro with-incremental-gensym [& body]
     `(let [c# (atom 0)]
        (with-redefs [gensym (fn incremental-gensym# [prefix#]
                               (symbol (str prefix# (swap! c# inc))))]
          ~@body))))

(defn- inverse?
  "Names like :my.ns/_attribute indicates an inverse relationship."
  [r]
  (string/starts-with? (name r) "_"))

(defn- inverse
  "DataScript triple clauses cannot contain inverse relationship keywords, convert them."
  [r]
  (let [rns (namespace r)
        rname (name r)
        inverse-name (if (string/starts-with? rname "_")
                       (subs rname 1)
                       (str "_" rname))]
    (if (keyword? r)
      (keyword rns inverse-name)
      (symbol rns inverse-name))))

(def ^:private logic?
  #{'and 'or 'not})

(defn- variable? [x]
  (or
    (= x '_)
    (string/starts-with? (name x) "?")))

(defn- rule-name? [x]
  (and
    (symbol? x)
    (not (logic? x))
    (not (variable? x))))

(defn- op? [x]
  (or
    (keyword? x)
    (rule-name? x)))

(defn- ident-vector? [x]
  (and
    (vector? x)
    (= 2 (count x))
    (keyword? (first x))))

(defn- ground? [x]
  (or
    (ident-vector? x)
    (and
      (not (vector? x))
      (not (list? x))
      (not (seq? x)))))

(def ^:private rearrange-logic
  (bottom-up
   ;; nested commutative logic is raised
   ((pred #{'and 'or} ?op) . !before ... (?op . !clauses ...) . !after ...)
   (?op . !before ... !clauses ... !after ...)

   ;; moves `or` to the outside, and `and` to the inside to match Datalog rule convention
   ('and . !before ... ('or . !clauses ...) . !after ...)
   ('or . ('and . ~@!before . !clauses . ~@!after) ...)

   ;; identity logic expressions are flattened
   ((pred #{'and 'or} ?op) ?body)
   ?body

   ;; double negatives are removed
   ('not ('not ?body))
   ?body

   ;; moves `not` inside to match Datalog rule convention
   ('not ('or . !clauses ...))
   ('and . ('not !clauses) ...)

   ('not ('and . !clauses ...))
   ('or . ('not !clauses) ...)))

(defn maybe-id
  "Returns the id of an entity, or the original value."
  [x]
  (or (and (map? x)
           (get x :db/id))
      x))

;;(declare u)

#_((def e
   (m/rewrite
    {}
    (u v))))

#_((def u
   (m/rewrite
    ([!ks !vs] ...)
    [!ks ... ($ ~clojure.string/upper-case !vs) ...]))
 (seq {:a "foo" :b "bar"}))


;;('and {?k ?v & ?m} {?r ?v & ?m2})

;;(?r {:as ?m})
;;1
;;(:k ?x)
;; step 1:

(defn unravel-pattern [m]
  (let [e (get m :db/id (gensym "?e"))]
    [e (for [[k v] m
             :when (not= k :db/id)
             :let [ss (if (map? v)
                        (let [[eid triples] (unravel-pattern v)]
                          (cons [e k eid]
                                triples))

                        [[e k v]])]
             s ss]
         s)]))

(def collapse-entities
  (bottom-up
    (and
      {?k (?r ?x) & ?m1}
      (guard (keyword? ?r))
      (let ?v (gensym "?v")))
    ('and {?k ?v & ?m1} {:db/id ~(maybe-id ?x) ?r ?v})

    (and
      {?k (?r {& ?m2}) & ?m1}
      (guard (keyword? ?r))
      (let ?v (or (get ?m2 ?r) (gensym "?v"))))
    ('and {?k ?v & ?m1} {?r ?v & ?m2})

   ;; todo: {:k (:k2 (:k3 {}))}

   ;; ?v must be an entity
   #_#_(and
    (?r ?v)
    (guard (keyword? ?r)))
   {?r (gensym "?v")
    :db/id ~(maybe-id ?v)}))



(def ^:private map-terms
  (rearrange-logic
    (top-down
      (and
        (?l . !expr ...)
        (guard (logic? ?l))
        (let ?e (gensym "?e")))
      ('and ~@(map map-terms !expr))
      ;; TODO: we want to unify base for all sub expr for this and other situations

      {:as ?m}
      ('and ~@(second (unravel-pattern ?m)))

      (and
        (?r {:as ?m})
        (guard op? ?r))
      {?r ~(gensym "?e") & ?m})))

(defn- bridge-expr [r t a b c]
  (let [bridge (gensym "?e")]
    (list 'and
          (t a b bridge)
          (if (keyword? r)
            [bridge r c]
            (list r bridge c)))))

(comment
 ;; base case; link to the ?result to be found
 ;; keyword get style (:some/attribute ?x)
 (and
  (?r ?x)
  (guard (op? ?r))
  (guard (ground? ?x)))
 ~(if (keyword? ?r)
    [?x ?r (gensym "?e")]
    (list ?r ?x (gensym "?e"))))

(defn- with-id [m]
  (if (contains? m :db/id)
    m
    (assoc m :db/id (gensym "?e"))))

(defn- entity-clauses [m]
  (let [e (or (:db/id m) (gensym "?e"))
        props (for [[k v] (dissoc m :db/id)]
                [k (if (map? v)
                     (with-id v)
                     v)])]
    (into []
     (concat
      ;; bridging clauses
      (for [[k v] props]
        [e k (if (map? v)
               (:db/id v)
               v)])
      ;; matching clauses
      (for [[k v] props
            :when (map? v)
            :let [subclauses (entity-clauses v)]
            clause subclauses]
        clause)))))

(def ^:private bridge-terms
  "Converts justice rule syntax into DataScript bridged triple syntax."
  #_(top-down
    ;; Unique entity, we only need the id
    {:db/id (pred integer? ?id)}
    ?id

    ;; base case for patterns
    (and
     {?k ?v :as ?m}
     ;; TODO: is there an explict way to expect this in meander?
     (guard (= (count ?m) 1))
     (let ?e ~(gensym "?e")))
    [?v ~(inverse ?k) ?e]


    (and
     {?k {:as ?n} :as ?m}
     (let ?e ~(gensym "?e")))
    [?p ?k ?e]
    [?e !k !v] ...


    ;; an entity pattern must match the conjunction of triple clauses for each property
    (and
     ;; TODO: can this be meander syntax?
     {[!ks !vs] ... :as ?m}
     (let ?e ~(gensym "?e")))
    ('and
     [?e ~(inverse !k) !v] ...)

    ;; expand nested call syntax
    ;; to a bridged pair of DataScript triple clauses:
    ;; (:k2 (:k1 ?x))
    ;; => (and [?x :k1 ?bridge] [?bridge :k2 ?result])
    ;; or bridged rule application:
    ;; (:k2 (my-rule ?x))
    ;; => (and (my-rule ?x ?bridge) [?bridge :k2 ?result])
    (and (?r [?a ?b ?c])
      (guard (op? ?r)))
    ~(bridge-expr ?r vector ?a ?b ?c)

    (and (?r (?a ?b ?c))
      (guard (op? ?r))
      (guard (op? ?a)))
    ~(bridge-expr ?r list ?a ?b ?c)

    ;; keep adding more clauses as we move outward from the expression center
    (and (?r ('and . !clauses ... [?a ?b ?c]))
      (guard (op? ?r)))
    ('and . !clauses ... ~@(rest (bridge-expr ?r vector ?a ?b ?c)))

    (and (?r ('and . !clauses ... (?a ?b ?c)))
      (guard (op? ?r))
      (guard (op? ?a)))
    ('and . !clauses ... ~@(rest (bridge-expr ?r list ?a ?b ?c)))))

(def ^:private uninverse-terms
  (bottom-up
    (and [?x ?r ?y]
      (guard (inverse? ?r)))
    [?y ~(inverse ?r) ?x]

    (and (?r ?x ?y)
      (guard (rule-name? ?r))
      (guard (inverse? ?r)))
    (~(inverse ?r) ?y ?x)))

(defn- occurs-in?
  "Returns true when x occurs in expression."
  [x expression]
  (->> expression
       (tree-seq sequential? seq)
       (some #{x})
       (boolean)))

(defn- replace-base [expr sym1 sym2]
  ((bottom-up ~sym1 ~sym2) expr))

(defn- maybe-replace-base-clause
  "If an explicit ?result was not specified in the pattern,
  then the base of a justice expression is identified by ?result.
  If the expression is an entity, ?result is the id of the entity.
  If the expression is a function call, then ?result is the value.
  This is special behavior for the ?result symbol."
  [expression]
  #_(m/rewrite
   ('or . !x ...)
   ('or . ($ ~maybe-replace-base-clause !x) ...)

   ;;(?f ?z)

   ;; only if result not present below
   {:db/id (pred (complement #{'?result}) ?id)}
   ~(throw (ex-info "Base clause has a non-result identity" {}))

   {:db/id nil & ?m}
   {:db/id '?result & ?m}


   (if (= 'or (first expression))
     (cons 'or (map maybe-replace-base-clause (rest expression)))
     (if (-> '?result (occurs-in? expression))
       expression
       (if (map? expression)
         (assoc expression :db/id '?result)
         ())))))

;; 1. Convert function syntax to map syntax
;;    what about nested function calls?
;;    what about top level `or`?
;;    what about logic?
#_(def ^:private f2m
  (top-down
   (?f ?x)))


;; 2. Maybe assign result
(def ^:private imply-result
  ())

;; 3. Top down create triples
(def ^:private create-triples
  bridge-terms)

(defn from-justice* [x]
  (-> x
    collapse-entities
    map-terms
    ;;bridge-terms
    uninverse-terms
    rearrange-logic
    maybe-replace-base-clause))

(defn from-justice
  "Translates justice syntax to bridged triples"
  [expression]
  (with-incremental-gensym
   (from-justice* expression)))

;; TODO: provide a way to convert clauses to justice
#_(def to-justice
    "Converts DataScript rule syntax into justice syntax."
    (bottom-up ...))

(defn datascript-rule
  "A DataScript rule consists of a head describing the name/inputs, and clauses in tripple syntax."
  [relation-name args body]
  (assert (qualified-symbol? relation-name) "relation-name should be a namespace qualified symbol")
  [(list* relation-name (conj args '?result))
   (from-justice body)])

;; TODO: (and (or ...)))
;; Un-nesting completely would require creating new rules as bridges... which is totally possible.
;; Alternatively can or/and be made to work with or-join? Is that just as efficient?
(def datascript-rules
  "Converts justice and/or syntax to DataScript conjunction/disjunction syntax."
  (bottom-up
    ;; TODO: ands ands ands
    ;; `and` expressions: [rule-head (and clause1 clause2)]
    ;; translates to conjunctive form: [rule-head clause1 clause2]
    [!before ... [?head (~'and . !clauses ...)] . !after ...]
    [!before ... [?head        . !clauses ...]  . !after ...]
    ;; `or` expressions: [rule-head (or clause1 clause2)]
    ;; translates to disjunctive form: [rule-head clause1] [my-rule clause2]
    [!before ... [?head (~'or . !clauses ...)]  . !after ...]
    [!before ... [?head         !clauses] ...     !after ...]))

(defn qualify-rule-references
  "Rules are qualified with their namespace, so that you can follow function conventions."
  [current-ns-str]
  (bottom-up
    (and (?s . !args ...)
      (guard (and (symbol? ?s)
               (not (contains? logic? ?s))
               (not (qualified-symbol? ?s)))))
    (~(symbol current-ns-str (name ?s)) . !args ...)))

(defn as-rules [qualified-rule-name args body]
  (assert (qualified-symbol? qualified-rule-name) "Rule names must be namespace qualified symbols")
  (assert (some? body) "Empty body (did you forget to quote it?)")
  (let [qualify (qualify-rule-references (namespace qualified-rule-name))]
    (-> [(datascript-rule qualified-rule-name args body)]
      (datascript-rules)
      (qualify))))

(defn entity-result?
  "Walks the query to find attributes joining to result.
  If there are any non-entity joins, returns false.
  Checks the db schema when result appears in the value position,
  where it must have a :db/valueType :db.type/ref."
  [db rules rule-name args]
  (let [result-variable (last args)
        match-rule #(and
                      (= (ffirst %) rule-name)
                      (= (dec (count (first %))) (count args)))
        matching-rules (filter match-rule rules)
        other-rules (remove match-rule rules)
        entity-result-clause?
        (m/rewrite
          ;; entity position in a datom triple
          [~result-variable ?a ?v]
          true

          ;; value position, but schema says type is ref
          [?e ?a ~result-variable]
          ~(if (= :db.type/ref (get-in db [:schema ?a :db/valueType]))
             true
             false)

          ;; TODO: do other arities need to be considered?
          ;; rule application, result in the return position
          (?r ?x ~result-variable)
          ~(entity-result? db other-rules ?r [?x result-variable])

          ;; rule application, result in the argument position
          (?r ~result-variable ?x)
          ~(entity-result? db other-rules ?r [result-variable ?x])

          ;; probably fine, right?
          _ true)]
    ;; TODO: should do a bottom up search, not a rewrite.
    (every? true?
      (for [[head & body] matching-rules
            clause body]
        (entity-result-clause? clause)))))
