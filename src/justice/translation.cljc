(ns justice.translation
  #?(:cljs (:require-macros [justice.translation :refer [rewrite-all]]))
  (:require [clojure.string :as string]
            [meander.strategy.gamma :as m]))

#?(:clj
   (defmacro rewrite-all [& body]
     `(m/until = (m/bottom-up (m/attempt (m/rewrite ~@body))))))

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

(defn- bridge-expr [r t a b c]
  (let [bridge (gensym "?bridge_")]
    (list 'and
      (t a b bridge)
      (if (keyword? r)
        [bridge r c]
        (list r bridge c)))))

(def ^:private bridge-terms
  "Converts justice rule syntax into DataScript bridged triple syntax."
  (rewrite-all
    ;; This is an entity, we only want the id
    {:db/id (pred integer? ?id)}
    ?id

    ;; base case for patterns
    ;; don't bother with and for single map
    {?k ?v :as ?m}
    ~(if (> (count ?m) 1)
       (cons 'and (for [[k v] ?m]
                    (list (inverse k) v)))
       (list (inverse ?k) ?v))

    #_(and {!r !x}
        (guard (op? !r))
        (guard (ground? !x)))
    #_('and . ($ ~f [!x !r]) ...)

    ;;; TODO: everything in an and should be linked

    ;; base case; link to the ?result to be found
    ;; keyword get style (:some/attribute ?x)
    ;; translates to a DataScript triple clause [?x :some/attribute ?result]
    ;; TODO: base may join
    (and (?r ?x)
      (guard (op? ?r))
      (guard (ground? ?x)))
    ~(if (keyword? ?r)
       [?x ?r '?justice-pattern-base]
       (list ?r ?x '?justice-pattern-base))

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
  (rewrite-all
    (and [?x ?r ?y]
      (guard (inverse? ?r)))
    [?y ~(inverse ?r) ?x]

    (and (?r ?x ?y)
      (guard (rule-name? ?r))
      (guard (inverse? ?r)))
    (~(inverse ?r) ?y ?x)))

(def ^:private rearrange-logic
  (rewrite-all
    ;; nested commutative logic is raised
    ((pred #{'and 'or} ?op) . !before ... (?op . !clauses ...) . !after ...)
    (                  ?op  . !before ...        !clauses ...    !after ...)

    ;; moves `or` to the outside, and `and` to the inside to match Datalog rule convention
    ('and . !before ... ('or  . !clauses ...) . !after ...)
    ('or  . ('and . ~@!before . !clauses . ~@!after)   ...)

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

(defn- count-occurrences
  "Returns how many times sym appears in expression"
  [expression]
  (->> expression
    (tree-seq sequential? seq)
    (frequencies)))

(defn- replace-base [expr sym]
  ((rewrite-all '?justice-pattern-base ~sym) expr))

(defn- maybe-replace-base-clause
  "The base of a justice expression may be replaced by ?result for querying,
  but only if an explicit ?result was not specified in the pattern.
  If no ?result was specified, then we replace base with ?result,
  otherwise base can be removed.
  This is special behavior for the ?result symbol."
  [expression]
  (let [{result-count '?result
         base-count '?justice-pattern-base
         :or {result-count 0 base-count 0}}
        (count-occurrences expression)]
    (if (> result-count 0)
      (if (> base-count 1)
        expression
        (replace-base expression '_))
      (replace-base expression '?result))))

(def from-justice
  "Translates justice syntax to bridged triples"
  (comp maybe-replace-base-clause rearrange-logic uninverse-terms bridge-terms))

;; TODO: provide a way to convert clauses to justice
#_(def to-justice
    "Converts DataScript rule syntax into justice syntax."
    (rewrite-all ...))

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
  (rewrite-all
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
  (rewrite-all
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
