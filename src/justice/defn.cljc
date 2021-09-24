(ns justice.defn
  #?(:cljs (:require-macros [justice.defn]))
  (:require [clojure.spec.alpha :as spec]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as s]))

;; Alternative to https://blog.klipse.tech/clojure/2016/10/10/defn-args.html

(defn wrap-defn
  "Returns a function that will parse a form according to `defn` semantics.
  Takes a function which will convert fn-spec forms."
  [rewrite-fn-spec]
  (s/rewrite
    (m/and ((m/pred simple-symbol? ?name) .
            (m/pred string? !?docstring) ..?n
            (m/pred map? !?attr-map) ..?m
            !tail ...)
           (m/guard (<= ?n 1))
           (m/guard (<= ?m 1))
           (let
             (or (([(m/pred simple-symbol? !params) ... :as !param-list] . !forms ... :as !fn-specs) ..1)
                 ([(m/pred simple-symbol? !params) ... :as !param-list] . !forms ... :as !fn-specs))
             (list* !tail))
           (m/guard (apply distinct? (map count !param-list))))
    (defn ?name . !?docstring ... !?attr-map ...
      (m/app rewrite-fn-spec !fn-specs) ...)))

(def defrule*
  (wrap-defn (s/rewrite ([!params ...] !forms ...)
                        ([] "hi!"))))

(defmacro defrule [& forms]
  (assert (spec/valid? :clojure.core.specs.alpha/defn-args forms)
          (spec/explain :clojure.core.specs.alpha/defn-args forms))
  (defrule* forms))
