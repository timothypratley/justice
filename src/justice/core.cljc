(ns justice.core
  "Concise query and rule syntax for Datalog."
  #?(:cljs (:require-macros [justice.core]))
  (:require #_[justice.defn :as defn]
            [justice.dependencies :as dependencies]
            [justice.translation :as t]
            [clojure.pprint :as pprint]
            [datascript.core :as d]))

(def ^:dynamic *trace*
  "Can be bound to enable trace println messages"
  false)

(defonce
  ^{:dynamic true
    :doc "Can be bound or attached to provide a db connection."}
  *conn* nil)

(defonce
  ^{:dynamic true
    :doc "Stores all rules created with defrule. Can be bound for testing purposes."}
  *rule-registry* {})

(defn attach
  "Remembers a db connection and uses it in subsequent rule applications."
  [conn]
  (assert (d/conn? conn) "Can only attach to a DataScript conn")
  #?(:cljs
     (set! *conn* conn)
     :clj
     (alter-var-root #'*conn* (constantly conn)))
  nil)

(defmacro with-conn
  "Provide a db connection to use for rule applications."
  [conn & body]
  `(binding [*conn* ~conn]
     (assert (d/conn? ~conn) "Expected a connection as first argument")
     ~@body))

(defmacro trace
  "Prints the DataScript query produced to stdout."
  [& body]
  `(binding [*trace* true]
     ~@body))

(defn q
  "Performs a query using justice syntax.
  May call registered rules.
  When calling registered rules, use the fully qualified name (`rule-name will work from within the same namespace).
  May be recursive with the special name justice.core/q.
  Use attach prior to calling, or provide a db as the first argument."
  ([expr] (q @*conn* expr))
  ([db expr]
   (assert (d/db? db) "Expected a db as first argument.")
   (let [q-rules (t/as-rules `q [] expr)
         registry (assoc *rule-registry* `q q-rules)
         rules (dependencies/relevant-rules `q registry)]
     (when *trace*
       (println "QUERY:")
       (pprint/pprint
         `(d/q
            ~{:find '[[?result ...]]
              :in '[$ %]
              :where [(list `q '?result)]}
            ~'db
            ~rules)))
     (let [result (d/q
                    {:find '[[?result ...]]
                     :in '[$ %]
                     :where [(list `q '?result)]}
                    db
                    rules)]
       (if (t/entity-result? db rules `q ['?result])
         (for [id result]
           (d/entity db id))
         result)))))

(defn all-rules
  "Retrieves the all registered rules from the registry."
  []
  (vec (mapcat val *rule-registry*)))

;; TODO: there should be a way to add a disjuctive clause to an existing rule.
(defn register-rule
  "Adds a rule to the rule-registry.
  Converts body from justice syntax to DataScript syntax."
  ([qualified-rule-name args body]
   (assert (some-> qualified-rule-name qualified-symbol?)
     "Rules must be registered in a namespace as a symbol.
      You can use register-rule directly with a hand supplied current-ns if you need to.")
   (let [rules (t/as-rules qualified-rule-name args body)
         r (assoc *rule-registry* qualified-rule-name rules)]
     #?(:cljs
        (set! *rule-registry* r)
        :clj
        (alter-var-root #'*rule-registry* (constantly r)))
     nil)))

(defn variable? [x]
  (and
    (symbol? x)
    (or
      (= x \_)
      (-> x name first #{\?}))))

;; TODO: pattern match!
(defn query-pattern
  "Returns a query-pattern suitable for matching variables or values as arguments."
  [rule-symbol [a b]]
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
  (or (and (map? x)
           (get x :db/id))
      x))

(defn rule-query
  "Runs a rule query against a db using rules."
  [db rules rule-name args]
  (when *trace*
    (println "QUERY:")
    (pprint/pprint
      `(d/q ~(query-pattern rule-name args)
            ~'db
            ~rules
            ~@(map maybe-id args))))
  (apply d/q
         (query-pattern rule-name args)
         db
         rules
         (map maybe-id args)))

(defn apply-rule
  "Queries a DataScript db using a pre-defined rule identified by rule-name."
  ([rule-name db & args]
   (assert (qualified-symbol? rule-name) "rule-name should be a namespace qualified symbol")
   (assert (d/db? db) "Must provide a DataScript db")
   (let [rules (dependencies/relevant-rules rule-name *rule-registry*)
         result (rule-query db rules rule-name args)
         want-entities (t/entity-result? db rules rule-name args)
         ;; TODO: support ternary
         [a b] args]
     (cond
       ;; Cartesian product
       (and (variable? a) (variable? b))
       ;; TODO: need to check both variables and maybe entity on either
       (if want-entities
         (map (partial map #(d/entity db %)) result)
         result)

       ;; Truth check
       (not (or (variable? a) (variable? b)))
       (boolean (seq result))

       :else
       (if want-entities
         (for [id result]
           (d/entity db id))
         result)))))

(defn db
  "Returns the currently attached connection db if present,
  else an assertion is raised."
  []
  (assert (and *conn* (d/conn? *conn*)) "DataScript db required. Did you forget to attach?")
  @*conn*)

(defn ar
  ([rule-name]
   (apply-rule rule-name (db) '?x '?result))
  ([rule-name a]
   (if (d/db? a)
     (apply-rule rule-name a '?x '?result)
     (apply-rule rule-name (db) a '?result)))
  ([rule-name a b]
   (if (d/db? a)
     (apply-rule rule-name a b '?result)
     (apply-rule rule-name (db) a b)))
  ([rule-name db a b]
   (apply-rule rule-name db a b)))

#_(defn- register-rule* [current-ns {:keys [name specs]}]
  (for [{:keys [params body]} specs]
    `(register-rule current-ns name params body)))

#_(defmacro defrule
  {:arglists '([name docstring? attr-map? & fn-spec-or-specs])}
  [& args]
  (let [parsed (defn/parse-define-args args)
        current-ns (or
                     (some-> *ns* ns-name)
                     (some-> &env :ns :name))]
    `(do
       ~@(register-rule* current-ns-str parsed)
       ~(defn/define-defn parsed))))

(defmacro defq
  "Registers a rule and sets up a convenience function to invoke that rule as a query."
  ([rule-name-short args body]
   `(defq ~rule-name-short nil ~args ~body))
  ([rule-name-short docstring args body]
   (let [current-ns (or
                      (some-> *ns* ns-name)
                      (some-> &env :ns :name))
         rule-name (symbol (name current-ns) (name rule-name-short))]
     `(do
        (register-rule '~rule-name '~args '~body)
        (defn ~rule-name-short ~(or docstring (str "Applies the " rule-name " rule."))
          ([] (ar '~rule-name))
          (~'[a] (ar '~rule-name ~'a))
          (~'[a b] (ar '~rule-name ~'a ~'b))
          (~'[db a b] (ar '~rule-name ~'db ~'a ~'b)))))))

(defn transacte
  "Like transact, but returns an entity.
  Aides in composition."
  ([tx] (transacte *conn* tx))
  ([conn tx] (some->> tx (d/transact conn) :tx-data ffirst (d/entity @conn))))

(defn _
  "Finds the input to a rule that satisfies result."
  [result rule]
  ;; TODO: ternary rules
  (rule '?x result))

(defn navigator [q relevant?]
  (let [k (keyword (gensym "navl"))
        a (atom nil)]
    (d/listen! *conn* k
               (fn [tx-report]
                 (when (relevant? tx-report)
                   (let [new (q)]
                     (when (not= new @a)
                       (reset! a new))))))
    (fn []
      ;; getter, setter, destructor
      [a
       #()
       (d/unlisten! *conn* k)])))
