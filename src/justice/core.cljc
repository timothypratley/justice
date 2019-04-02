(ns justice.core
  #?(:cljs (:require-macros [justice.core]))
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [datascript.core :as d]
            [meander.strategy.gamma :as s]))

(def ^:dynamic *trace*
  "Can be bound to enable trace println messages"
  false)

(def ^:dynamic *conn*
  "Can be bound or attached to provide a db connection."
  nil)

(def ^:dynamic *rule-registry*
  "Stores all rules created with defrule.
  Can be bound for testing purposes."
  {})

(defmacro trace [& body]
  `(binding [*trace* true]
     ~@body))

(defn attach
  "Remembers a db connection and uses it in subsequent rule applications."
  [conn]
  (alter-var-root #'*conn* (constantly conn)))

(defn all-rules
  "Retrieves the all registered rules from the registry."
  []
  (vec (mapcat val *rule-registry*)))

(defn inverse? [k]
  (re-find #"/_" (str k)))

(defn forward [k]
  (keyword (subs (string/replace (str k) #"/_" "/") 1)))

(def datascript-rule-body
  "Converts justice rule syntax into DataScript bridged triple syntax."
  (comp
    (s/bottom-up
      (s/attempt
        (s/match
          (and (?k ?target)
               (guard (keyword? ?k)))
          (if (inverse? ?k)
            ['?result (forward ?k) ?target]
            [?target ?k '?result]))))
    (s/bottom-up
      (s/attempt
        (s/match
          (and (?r (?a ?b))
               (guard (not (#{'and 'or} ?r))))
          (let [v (gensym "?bridge_")]
            (list 'and (list ?r v)
                  (list ?a ?b v))))))))

(defn datascript-rule
  "A DataScript rule consists of a head describing the name/inputs, and clauses in tripple syntax."
  [relation-name args body]
  [(list* (symbol relation-name) (conj args '?result))
   (datascript-rule-body body)])

;; TODO: Right now you can't nest more than 2 layers: (or (and (or ...)))
;; Unnesting completely would require creating new rules as bridges... which is totally possible.
;; Alternatively can or/and be made to work with or-join? Is that just as efficient?
(def datascript-rules
  "Converts justice and/or syntax to DataScript conjunction/disjunction syntax."
  (comp
    ;; Replace any top level and with implied
    (s/bottom-up
      (s/attempt
        (s/match
          [?rule-head (~'and . !clauses ...)]
          (into [?rule-head] !clauses))))
    ;; Replace any top level or with multiple rules
    (s/bottom-up
      (s/attempt
        (s/match
          [[?rule-head (~'or . !clauses ...)]]
          (vec (for [clause !clauses]
                 [?rule-head clause])))))))

(defn qualify [s]
  (symbol (name (ns-name *ns*)) (name s)))

(def qualify-rule-references
  "Rules are qualified with their namespace, so that you can follow function conventions."
  (s/bottom-up
    (s/attempt
      (s/match
        (and (?s . !args ...)
             (guard (and (symbol? ?s)
                         (not (#{'and 'or} ?s))
                         (not (qualified-symbol? ?s)))))
        (list* (qualify ?s) !args)))))

(defn register-rule
  "Adds a rule to the rule-registry.
  Converts body from justice syntax to DataScript syntax."
  ([rule-name args body] (register-rule #'*rule-registry* rule-name args body))
  ([registry rule-name args body]
   (->> (datascript-rules [(datascript-rule rule-name args body)])
        (qualify-rule-references)
        (alter-var-root registry assoc (name rule-name)))))

(defn variable? [x]
  (and (symbol? x)
       (-> x name first #{\? \_})))

;; TODO: pattern match!
(defn query-pattern
  "Returns a query-pattern suitable for matching variables or values as arguments."
  [rule-symbol a b]
  (cond
    ;; TODO: these are pairs, not entities! :(
    (and (variable? a) (variable? b))
    {:find '[?a ?b]
     :in '[$ % _ _]
     :where [(list rule-symbol '?a '?b)]}

    (variable? b)
    {:find '[[?result ...]]
     :in '[$ % ?a _]
     :where [(list rule-symbol '?a '?result)]}

    (variable? a)
    {:find '[[?result ...]]
     :in '[$ % _ ?b]
     :where [(list rule-symbol '?result '?b)]}

    ;; TODO: is there a special form for boolean tests?
    :else
    {:find '[[?a ...]]
     :in '[$ % ?a ?b]
     :where [(list rule-symbol '?a '?b)]}))

(defn maybe-id
  "Returns the id of an entity, or the original value."
  [x]
  (if (map? x)
    (:db/id x)
    x))

;; TODO: what if @*conn* is nil???
;; TODO: pattern match
(defn apply-rule
  "Queries a DataScript db using a pre-defined rule identified by rule-name."
  ([rule-name]
    (apply-rule rule-name @*conn* '?x))
  ([rule-name a]
    (if (d/db? a)
      (apply-rule rule-name a '?x '?y)
      (apply-rule rule-name @*conn* a)))
  ([rule-name a b]
    (if (d/db? a)
      (apply-rule rule-name a b '?y)
      (apply-rule rule-name @*conn* a b)))
  ([rule-name db a b]
   {:pre [(d/db? db)]}
   (when *trace*
     (println "QUERY:")
     (pprint/pprint
       `(d/q ~(query-pattern (symbol rule-name) a b)
             ~'datascript/DB
             ~(all-rules)
             ~(maybe-id a)
             ~(maybe-id b))))
   (let [result (d/q (query-pattern (symbol rule-name) a b)
                     db
                     (all-rules)
                     (maybe-id a)
                     (maybe-id b))]
     (cond (and (variable? a) (variable? b))
           ;; Cartesian product
           (reduce
             (fn [acc [k v]]
               (update acc (d/entity db k) (fnil conj []) (d/entity db v)))
             {}
             result)

           (not (or (variable? a) (variable? b)))
           ;; Truth check
           (boolean (seq result))

           :else
           (for [id result]
             (d/entity db id))))))

(defmacro defrule
  "Registers a rule and sets up a convenience function to invoke that rule."
  ([rule-symbol-short args body] `(defrule ~rule-symbol-short nil ~args ~body))
  ([rule-symbol-short docstring args body]
   (let [rule-symbol (symbol (name (ns-name *ns*)) (name rule-symbol-short))
         rule-name (str rule-symbol)]
     `(do
       (register-rule ~rule-name '~args '~body)
       (defn ~rule-symbol-short ~(or docstring (str "Applies the " rule-name " rule."))
         ([] (apply-rule ~rule-name))
         (~'[a] (apply-rule ~rule-name ~'a))
         (~'[a b] (apply-rule ~rule-name ~'a ~'b))
         (~'[db a b] (apply-rule ~rule-name ~'db ~'a ~'b)))))))
;; TODO: Could also handle metadata https://blog.klipse.tech/clojure/2016/10/10/defn-args.html

(defn transacte
  "Like transact, but returns an entity.
  Aides in composition."
  ([tx] (transacte *conn* tx))
  ([conn tx] (some->> tx (d/transact conn) :tx-data ffirst (d/entity @conn))))
