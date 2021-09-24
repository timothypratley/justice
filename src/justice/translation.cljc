(ns justice.translation
  "Translates Justice map and function call expressions to Datalog triple queries"
  ;; TODO: maybe have a .clj file for macros?
  #?(:cljs (:require-macros [justice.translation :refer [bottom-up top-down]]))
  (:require [clojure.string :as string]
            [meander.strategy.epsilon :as s]
            [meander.epsilon :as m]))

#?(:clj
   (defmacro bottom-up [& body]
     `(s/until = (s/bottom-up (s/attempt (s/rewrite ~@body))))))
#?(:clj
   (defmacro top-down [& body]
     `(s/until = (s/top-down (s/attempt (s/rewrite ~@body))))))

(def ^:dynamic *counter*)

(defn next-variable [label]
  (let [n (swap! *counter* inc)]
    (if (= n 0)
      '?result
      (symbol (str "?" label n)))))

(defn- reverse-lookup?
  "Names like :my.ns/_attribute indicates an inverse relationship."
  [r]
  (string/starts-with? (name r) "_"))

(defn- invert
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
  (or (= x '_)
      (string/starts-with? (name x) "?")))

(defn- rule-name? [x]
  (and (symbol? x)
       (not (logic? x))
       (not (variable? x))))

(defn- op? [x]
  (or (keyword? x)
      (rule-name? x)))

(defn- ident-vector? [x]
  (and (vector? x)
       (= 2 (count x))
       (keyword? (first x))))

(defn- ground? [x]
  (or (ident-vector? x)
      (and (not (vector? x))
           (not (list? x))
           (not (seq? x)))))
(def x (s/rewrite ?x ?x))

(def ^:private rearrange-logic
  (s/rewrite
   ;; nested commutative logic is raised
   ((m/pred #{'and 'or} ?op) . !before ... (?op . !clauses ...) . !after ...)
   (?op . !before ... !clauses ... !after ...)

   ;; broken in meander epsilon
   ;; moves `or` to the outside, and `and` to the inside to match Datalog rule convention
   #_#_
   ('and . !before ... ('or . !clauses ...) . !after ...)
   ('or . ('and . ~@!before . !clauses . ~@!after) ...)

   ;; identity logic expressions are flattened
   ((m/pred #{'and 'or} ?op) ?body)
   ?body

   ;; double negatives are removed
   ('not ('not ?body))
   ?body

   ;; moves `not` inside to match Datalog rule convention (De Morgan's law)
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
   (s/rewrite
    {}
    (u v))))

#_((def u
   (s/rewrite
    ([!ks !vs] ...)
    [!ks ... ($ ~clojure.string/upper-case !vs) ...]))
 (seq {:a "foo" :b "bar"}))


;;('and {?k ?v & ?m} {?r ?v & ?m2})

;;(?r {:as ?m})
;;1
;;(:k ?x)
;; step 1:

(defn unravel-pattern [m]
  (let [e (get m :db/id (next-variable "e"))]
    [e (for [[k v] m
             :when (not= k :db/id)
             :let [ss (if (map? v)
                        (let [[eid triples] (unravel-pattern v)]
                          (cons [e k eid]
                                triples))

                        [[e k v]])]
             s ss]
         s)]))

(defn- condense-lookups
  [ks sym]
  (reduce
   (fn [acc k]
     {k acc})
   sym
   ks))

(defn- unify-entities [m x]
  (cond
    (or (int? x) (symbol? x)) (assoc m :db/id x)
    (map? x) (merge m x)
    :else m))

(defn- collect-lookups
  "Unwraps lookup syntax into nested entity patterns."
  [expr sym]
  (loop [[r x] expr
         collected []]
    (let [ks (conj collected r)]
      (if (list? x)
        (if (keyword? (first x))
          (recur x ks)
          expr)
        (-> (condense-lookups ks sym)
            (unify-entities x))))))

(defn- occurs-in?
  "Returns true when x occurs in expression."
  [x expression]
  (->> expression
       (tree-seq seqable? seq)
       (some #{x})
       (boolean)))

(def ^:private top-level-lookup
  "Top level lookup syntax implies result"
  (s/attempt
   (s/rewrite
    (m/and
     (?r ?x :as ?expr)
     (m/guard (keyword? ?r))
     (m/guard (not (occurs-in? '?result ?expr))))
    ~(collect-lookups ?expr '?result))))

#_(def ^:private top-level-logic
  "Top level lookup syntax implies result"
  (s/attempt
   (s/rewrite
    (m/and
     (?l . !xs ...)
     (m/guard (logic? ?l))
     (m/guard (not (occurs-in? '?result !xs))))
    (?l ~(map (comp top-level-logic top-level-lookup)
              !xs))
    ?x)))

(def ^:private lookups-as-maps
  "Function style lookups imply two related entities"
  (top-down
   (m/and
    {?k (?r {& ?m2}) & ?m1}
    (m/guard (keyword? ?r))
    (let ?v (or (get ?m2 ?r) (next-variable "v"))))
   ('and
    {?k ?v & ?m1}
    {?r ?v & ?m2})

   (m/and
    {?k (?r ?x) & ?m1}
    (m/guard (keyword? ?r))
    (let ?v (next-variable "v")))
   ('and
    {?k ?v & ?m1}
    {:db/id ~(maybe-id ?x)
     ?r ?v})

   (m/and
    (?r1 ?x :as ?expr)
    (m/guard (keyword? ?r1)))
   ~(collect-lookups ?expr (next-variable "v"))))

(def ^:private expand-maps-to-triples
  "Takes map syntax and returns triple syntax"
  (top-down
    ;; broken in meander epsilon
   {:as ?m}
   ('and & (second (unravel-pattern ?m)))

   (m/and
    (?r {:as ?m})
    (m/guard (op? ?r)))
   {?r ~(next-variable "e") & ?m}))

(defn- bridge-expr [r t a b c]
  (let [bridge (next-variable "e")]
    (list 'and
          (t a b bridge)
          (if (keyword? r)
            [bridge r c]
            (list r bridge c)))))

(comment
 ;; base case; link to the ?result to be found
 ;; keyword get style (:some/attribute ?x)
 (m/and
  (?r ?x)
  (m/guard (op? ?r))
  (m/guard (ground? ?x)))
 ~(if (keyword? ?r)
    [?x ?r (next-variable "e")]
    (list ?r ?x (next-variable "e"))))

(defn- with-id [m]
  (if (contains? m :db/id)
    m
    (assoc m :db/id (next-variable "e"))))

#_
(defn- entity-clauses [m]
  (let [e (or (:db/id m) (next-variable "e"))
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
    {:db/id (m/pred integer? ?id)}
    ?id

    ;; base case for patterns
    (m/and
     {?k ?v :as ?m}
     ;; TODO: is there an explict way to expect this in meander?
     (m/guard (= (count ?m) 1))
     (let ?e ~(gensym "?e")))
    [?v ~(invert ?k) ?e]


    (m/and
     {?k {:as ?n} :as ?m}
     (let ?e ~(gensym "?e")))
    [?p ?k ?e]
    [?e !k !v] ...


    ;; an entity pattern must match the conjunction of triple clauses for each property
    (m/and
     ;; TODO: can this be meander syntax?
     {[!ks !vs] ... :as ?m}
     (let ?e ~(gensym "?e")))
    ('and
     [?e ~(invert !k) !v] ...)

    ;; expand nested call syntax
    ;; to a bridged pair of DataScript triple clauses:
    ;; (:k2 (:k1 ?x))
    ;; => (and [?x :k1 ?bridge] [?bridge :k2 ?result])
    ;; or bridged rule application:
    ;; (:k2 (my-rule ?x))
    ;; => (and (my-rule ?x ?bridge) [?bridge :k2 ?result])
    (m/and (?r [?a ?b ?c])
      (m/guard (op? ?r)))
    ~(bridge-expr ?r vector ?a ?b ?c)

    (m/and (?r (?a ?b ?c))
      (m/guard (op? ?r))
      (m/guard (op? ?a)))
    ~(bridge-expr ?r list ?a ?b ?c)

    ;; keep adding more clauses as we move outward from the expression center
    (m/and (?r ('and . !clauses ... [?a ?b ?c]))
      (m/guard (op? ?r)))
    ('and . !clauses ... ~@(rest (bridge-expr ?r vector ?a ?b ?c)))

    (m/and (?r ('and . !clauses ... (?a ?b ?c)))
      (m/guard (op? ?r))
      (m/guard (op? ?a)))
    ('and . !clauses ... ~@(rest (bridge-expr ?r list ?a ?b ?c)))))

(def ^:private invert-reverse-lookups
  (bottom-up
   (and [?x ?r ?y]
        (m/guard (reverse-lookup? ?r)))
   [?y ~(invert ?r) ?x]

   (and (?r ?x ?y)
        (m/guard (rule-name? ?r))
        (m/guard (reverse-lookup? ?r)))
   (~(invert ?r) ?y ?x)))

(defn- replace-base [expr sym1 sym2]
  ((bottom-up ~sym1 ~sym2) expr))

(defn- imply-result
  "If an explicit ?result was not specified in the pattern,
  then the base of a justice expression is identified by ?result.
  If the expression is an entity, ?result is the id of the entity.
  If the expression is a function call, then ?result is the value.
  This is special behavior for the ?result symbol."
  [expression]
  (cond
    (-> '?result (occurs-in? expression))
    expression

    (map? expression)
    (assoc expression :db/id '?result)

    (and (seq? expression) (logic? (first expression)))
    (if (= 'or (first expression))
      (cons 'or (map imply-result (rest expression)))
      (cons (first expression)
            (cons (imply-result (second expression))
                  (drop 2 expression))))

    :else
    expression))

;; 1. Convert function syntax to map syntax
;;    what about nested function calls?
;;    what about top level `or`?
;;    what about logic?
;; 2. Maybe assign result
;; 3. Top down create triples

(defn spy [x]
  (prn 'SPY x)
  x)

(defn from-justice* [x]
  (-> x
      ;;      top-level-lookup
      ;;top-level-logic
      ;;spy
      imply-result
      lookups-as-maps
      expand-maps-to-triples
      invert-reverse-lookups
      ;;bridge-terms
      rearrange-logic
      ))

(defn from-justice
  "Translates justice syntax to bridged triples"
  [expression]
  (let [x (imply-result expression)]
    (binding [*counter* (atom (if (occurs-in? '?result x)
                                0
                                -1))]
      (from-justice* x))))

;; TODO: provide a way to convert clauses to justice
#_(def to-justice
    "Converts DataScript rule syntax into justice syntax."
    (bottom-up ...))

(defn datascript-rule
  "A DataScript rule consists of a head describing the name/inputs, and clauses in tripple syntax."
  [relation-name args body]
  (assert (qualified-symbol? relation-name)
          "relation-name should be a namespace qualified symbol")
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
    (m/and (?s . !args ...)
      (m/guard (and (symbol? ?s)
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
        (s/rewrite
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
