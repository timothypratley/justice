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

(defn- uninverse
  "DataScript triple clauses cannot contain inverse relationship keywords, convert them."
  [r]
  (cond (simple-keyword? r)
        (keyword (subs (name r) 1))

        (qualified-keyword? r)
        (keyword (namespace r) (subs (name r) 1))

        (simple-symbol? r)
        (symbol (subs (name r) 1))

        (qualified-symbol? r)
        (symbol (namespace r) (subs (name r) 1))))

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

(defn bridge-expr [r t a b c]
  (let [bridge (gensym "?bridge_")]
    (list 'and
      (t a b bridge)
      (if (keyword? r)
        [bridge r c]
        (list r bridge c)))))

(def bridge-terms
  "Converts justice rule syntax into DataScript bridged triple syntax."
  (rewrite-all
    ;; base case; link to the ?result to be found
    ;; keyword get style (:some/attribute ?x)
    ;; translates to a DataScript triple clause [?x :some/attribute ?result]
    (and (?r ?x)
      (guard (op? ?r))
      (guard (ground? ?x)))
    ~(if (keyword? ?r)
       [?x ?r '?result]
       (list ?r ?x '?result))

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

(def uninverse-terms
  (comp
    (rewrite-all
      (and [?x ?r ?y]
        (guard (inverse? ?r)))
      [?y ~(uninverse ?r) ?x])
    (rewrite-all
      (and (?r ?x ?y)
        (guard (rule-name? ?r))
        (guard (inverse? ?r)))
      (~(uninverse ?r) ?y ?x))))

(def datascript-rule-body
  (comp uninverse-terms bridge-terms))

;; TODO: provide a way to convert clauses to justice
#_(def to-justice
    "Converts DataScript rule syntax into justice syntax."
    (rewrite-all ...))

(defn datascript-rule
  "A DataScript rule consists of a head describing the name/inputs, and clauses in tripple syntax."
  [relation-name args body]
  (assert (qualified-symbol? relation-name) "relation-name should be a namespace qualified symbol")
  [(list* relation-name (conj args '?result))
   (datascript-rule-body body)])

;; TODO: (and (or ...)))
;; Un-nesting completely would require creating new rules as bridges... which is totally possible.
;; Alternatively can or/and be made to work with or-join? Is that just as efficient?
(def datascript-rules
  "Converts justice and/or syntax to DataScript conjunction/disjunction syntax."
  (rewrite-all
    ;; `and` expressions: [rule-head (and clause1 clause2)]
    ;; translates to conjunctive form: [rule-head clause1 clause2]
    [!before ... [?head (~'and . !clauses ...)] . !after ...]
    [!before ... [?head . !clauses ...] ...       !after ...]
    ;; `or` expressions: [rule-head (or clause1 clause2)]
    ;; translates to disjunctive form: [rule-head clause1] [my-rule clause2]
    [!before ... [?head (~'or . !clauses ...)]  . !after ...]
    [!before ... [?head !clauses] ...             !after ...]))

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
  (let [qualify (qualify-rule-references (namespace qualified-rule-name))]
    (-> [(datascript-rule qualified-rule-name args body)]
      (datascript-rules)
      (qualify))))
