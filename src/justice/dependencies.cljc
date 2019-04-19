(ns justice.dependencies
  (:require [clojure.set :as set]))

(defn- follow-dependencies [direct-dependencies unvisited visited]
  (if (empty? unvisited)
    visited
    (let [visit (first unvisited)
          new (set/difference (get direct-dependencies visit) visited)]
      (recur
        direct-dependencies
        (disj (set/union unvisited new) visit)
        (conj visited visit)))))

(defn- dependencies [rules all-rule-names]
  (apply set/union (for [[head & clauses] rules]
                     (->> (tree-seq seqable? seq clauses)
                       (filter all-rule-names)
                       (set)))))

(defn calculate-rule-dependencies
  "Builds a map of rule name to dependencies.
  Dependencies of a rule may include itself if it is recursive.
  Indirect dependencies are included."
  [registry]
  (let [all-rule-names (set (keys registry))
        direct-dependencies
        (into {}
          (for [[rule-name datascript-rules] registry]
            [rule-name (dependencies datascript-rules all-rule-names)]))]
    (into {}
      (for [[rule-name] registry]
        [rule-name (follow-dependencies direct-dependencies (get direct-dependencies rule-name) #{})]))))

(defn relevant-rules
  "Retrieves only rules required to execute the given rule-name."
  [rule-name registry]
  (vec
    (mapcat
      registry
      (-> (calculate-rule-dependencies registry)
        (get rule-name)
        (conj rule-name)))))
