(ns justice.translation
  #?(:cljs (:require-macros [justice.translation :refer [rewrite-all]]))
  (:require [clojure.string :as string]
            [meander.strategy.gamma :as m]))

#?(:clj
   (defmacro rewrite-all [& body]
     `(m/until = (m/bottom-up (m/attempt (m/rewrite ~@body))))))

(defn- inverse?
  "Names like :my.ns/_attribute indicates an inverse relationship."
  [k]
  (string/starts-with? (name k) "_"))

(defn- forward-keyword? [k]
  (and (keyword? k)
    (not (inverse? k))))

(defn- inverse-keyword? [k]
  (and (keyword? k)
    (inverse? k)))

(defn- forward
  "DataScript triple clauses cannot contain inverse relationship keywords, convert them."
  [k]
  ;; TODO: make work with rule names and unqualified keywords
  (keyword (subs (string/replace (str k) #"/_" "/") 1)))

(defn- inverse [k]
  ;; TODO: make work with rule names and unqualified keywords
  (keyword (subs (string/replace (str k) #"/" "/_") 1)))

(def ^:private logic-terms
  #{'and 'or 'not})

(def ^:private rule-term?
  (complement logic-terms))

(defn- term? [x]
  (or (keyword? x) (rule-term? x)))

(defn- forward-rule? [term]
  (and (rule-term? term)
    (not (inverse? term))))

(defn- inverse-rule? [term]
  (and (rule-term? term)
    (inverse? term)))

(defn- xor [a b]
  (and (or a b)
    (not (and a b))))

(def datascript-rule-body
  "Converts justice rule syntax into DataScript bridged triple syntax."
  (rewrite-all
    ;; base case; link to the ?result to be found
    ;; keyword get style (:some/attribute ?x)
    ;; translates to a DataScript triple clause [?x :some/attribute ?result]
    (and (?a ?b)
      (guard (term? ?a))
      (guard (not (seqable? ?b))))
    ~(if (keyword? ?a)
       (if (inverse? ?a)
         ['?result (forward ?a) ?b]
         [?b ?a '?result])
       (if (inverse? ?a)
         (list ?a ?b '?result)
         (list ?a '?result ?b)))

    ;; expand nested call syntax
    ;; to a bridged pair of DataScript triple clauses:
    ;; (:k2 (:k1 ?x))
    ;; => (and [?x :k1 ?bridge] [?bridge :k2 ?result])
    ;; or bridged rule application:
    ;; (:k2 (my-rule ?x))
    ;; => (and (my-rule ?x ?bridge) [?bridge :k2 ?result])
    (and (?new [?a ?b ?c])
      (guard (term? ?new)))
    ~(let [bridge (gensym "?bridge_")]
       (list 'and
         [?a ?b bridge]
         (if (keyword? ?new)
           (if (inverse? ?new)
             [?c (forward ?new) bridge]
             [bridge ?new ?c])
           (if (xor (= '?result ?c) (inverse? ?new))
             (list ?new bridge ?c)
             (list ?new ?c bridge)))))))

;; TODO:
#_(def to-justice
    "Converts DataScript rule syntax into justice syntax."
    (rewrite-all
      ;; base case; link to the ?result to be found
      ;; keyword get style (:some/attribute ?x)
      ;; translates to a DataScript triple clause [?x :some/attribute ?result]
      [?e ?a '?result]
      (?a ?e)

      ['?result ?a ?v]
      (~(inverse ?a) ?v)

      ;; expand nested call syntax
      ;; to a bridged pair of DataScript triple clauses:
      ;; (:k2 (:k1 ?x))
      ;; => (and [?x :k1 ?bridge] [?bridge :k2 ?result])
      ;; or bridged rule application:
      ;; (:k2 (my-rule ?x))
      ;; => (and (my-rule ?x ?bridge) [?bridge :k2 ?result])
      (let [bridge (gensym "?bridge_")]
        (list 'and
          [?a ?b bridge]
          (if (keyword? ?new)
            (if (inverse-keyword? ?new)
              [?c (forward-keyword? ?new) bridge]
              [bridge ?new ?c])
            (if (= '?result ?c)
              (list ?new bridge ?c)
              (list ?new ?c bridge)))))
      (and (?new [?a ?b ?c])
        (guard (not (contains? logic-terms ?new))))))

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
               (not (contains? logic-terms ?s))
               (not (qualified-symbol? ?s)))))
    (~(symbol current-ns-str (name ?s)) . !args ...)))

(defn as-rules [qualified-rule-name args body]
  (assert (qualified-symbol? qualified-rule-name) "Rule names must be namespace qualified symbols")
  (let [qualify (qualify-rule-references (namespace qualified-rule-name))]
    (-> [(datascript-rule qualified-rule-name args body)]
      (datascript-rules)
      (qualify))))
