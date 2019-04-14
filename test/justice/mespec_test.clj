(ns justice.mespec-test
  (:require [clojure.test :refer [deftest is testing]]
            [justice.mespec :as ms]
            [meander.strategy.gamma :as m]))
#_#_
(deftest rewrite-optionals-test
  (is (= '(?name . (and (pred string !?docstring)
                        (guard (<= (count !?docstring) 1))) ...
                 . (and (pred map? !?attr-map)
                        (guard (<= (count !?attr-map) 1))) ...)
         (ms/replace-from-pattern-optionals '(?name (pred string? ??docstring) (pred map? ??attr-map)))))
  (is (= '(?name . !?docstring ... . !?attr-map ...)
         (ms/replace-to-pattern-optionals '(?name (pred string? ??docstring) (pred map? ??attr-map)))))

  (is (= '((. (and (pred identity !?name)
                   (guard (<= (count !?name) 1))) ...)
            (. !?name ...))
         (ms/replace-clause-optionals '(??name) '(??name)))))

(deftest rewrite-test
  (let [t (ms/rewrite ((?name ??docstring ??attr-map)
                          (simple-symbol? string? map?))
                        (:> ?name :> ??docstring :> ??attr-map))]
    (is (= '(:> foo :> "bar" :> {})
           (t '(foo "bar" {}))))
    (is (= '(:> foo :> "bar" :>)
           (t '(foo "bar"))))
    (is (= '(:> foo :> :> {})
           (t '(foo {}))))
    (is (= '(:> foo :> :>)
           (t '(foo))))
    (is (= m/*fail*
           (t '(foo {} "bar"))))))
