(ns justice.defn
  #?(:cljs (:require-macros [justice.defn]))
  (:require
   ;;[clojure.core.specs.alpha :as specs]
            [clojure.spec.alpha :as s]
   ;;[clojure.spec.gen.alpha :as gen]
            [meander.strategy.gamma :as m]))

;; Alternative to https://blog.klipse.tech/clojure/2016/10/10/defn-args.html

(defn wrap-defn
  "Returns a function that will parse a form according to `defn` semantics.
  Takes a function which will convert fn-spec forms."
  [rewrite-fn-spec]
  (m/rewrite (and ((pred simple-symbol? ?name) .
                    (pred string? !?docstring) ...
                    (pred map? !?attr-map) ...
                    !tail ...)
                  (guard (<= (count !?docstring) 1))
                  (guard (<= (count !?attr-map) 1))
                  (let
                    (or (([(pred simple-symbol? !params) ... :as !param-list] . !forms ... :as !fn-specs) ..1)
                        ([(pred simple-symbol? !params) ... :as !param-list] . !forms ... :as !fn-specs))
                    (list* !tail))
                  (guard (apply distinct? (map count !param-list))))
             (defn ?name . !?docstring ... !?attr-map ...
               ~@(map rewrite-fn-spec !fn-specs))))

(def defrule*
  (wrap-defn (m/rewrite ([!params ...] !forms ...)
                        ([] "hi!"))))

(defmacro defrule [& forms]
  (assert (s/valid? :clojure.core.specs.alpha/defn-args forms)
          (s/explain :clojure.core.specs.alpha/defn-args forms))
  (defrule* forms))
