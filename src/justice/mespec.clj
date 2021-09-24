(ns justice.mespec
  ;;#?(:cljs (:require-macros [justice.mespec]))
  (:require [clojure.string :as string]
            [meander.strategy.epsilon :as s]
            [meander.epsilon :as m]))

(defmacro rewrite-all [& body]
  `(s/until = (s/bottom-up (s/attempt (s/rewrite ~@body)))))

(defn optional? [x]
  (and
    (simple-symbol? x)
    (or
      (string/starts-with? (name x) "??"))))

(defn ?! [s]
  (if (and (symbol? s) (string/starts-with? (name s) "?"))
    (symbol (str "!" (subs (name s) 1)))
    s))

(def with-specs
  (s/rewrite ((!syms ...) (!specs ...))
             (('pred !specs !syms) ...)))

;; emulates at most 1 semantics
(def replace-from-pattern-optionals
  "Translates ??x to . !?x ... with a guard on count being 0 or 1 to emulate regex ? feature of at most 1.
  Works through (m/pred string? ?x) so that you can write spec like patterns.
  This function is not for direct use, see super-syntax."
  #_(rewrite-all (!before ... (m/or (m/and (m/pred optional? ?v) (m/let [?spec 'identity]))
                                  (m/pred ?spec (m/pred optional? ?v)))
                        . !after ...)
               (!before ... ~@(list '. (list m/and (list m/pred ?spec (?! ?v))
                                             (list m/guard (list '<= (list 'count (?! ?v)) 1))) '...)
                        . !after ...)))

(def replace-to-pattern-optionals
  (rewrite-all (!before ... (m/pred optional? ?v) . !after ...)
               (!before ... ~@(list '. (?! ?v) '...) !after ...)))

(def replace-clause-optionals
  #_(s/rewrite (!from !to ...)
             (~(replace-from-pattern-optionals !from) ~(replace-to-pattern-optionals !to) ...)))

(defmacro rewrite [& clauses]
  `(s/rewrite ~@(eval (replace-clause-optionals clauses))))

;; TODO: nested params
;; TODO: name the groups by wrapping the first element in (and !group1 !1), then can count same named as different groups
(def ||-!
  #_(s/rewrite
    ;; group delimiters on either side
    (!before ... ~'. . !group1 ..1 | . !group2 ..1 ~'. . !after ...)
    ('and (!before ... ~'. ~@(map ?! !group1) ~'... ~'. ~@(map ?! !group2) ~'... !after ...)
      ('guard (= 1 (+ (count ~(?! (first !group1))) (count ~(?! (first !group2)))))))

    ;; ternary group:   A | B | C  ;; not yet supported
    (!before ..1 | . !middle ..1 | . !after ..1)
    {:before [!before]
     :after [!after]
     :middle [!middle]}

    ;; last thing in expression
    (!before ... ~'. . !group1 ..1 | . !group2 ..1)
    ('and (!before ... ~'. ~@(map ?! !group1) ~'... ~'. ~@(map ?! !group2) ~'...)
      ('guard (= 1 (+ (count ~(?! (first !group1))) (count ~(?! (first !group2)))))))

    ;; first thing in expression
    (!group1 ..1 | . !group2 ..1 ~'. . !after ...)
    ('and (~@(map ?! !group1) ~'... ~'. ~@(map ?! !group2) ~'... !after ...)
      ('guard (= 1 (+ (count ~(?! (first !group1))) (count ~(?! (first !group2)))))))

    ;; only thing in expression
    (!group1 ... | . !group2 ...)
    ('and (~@(map ?! !group1) ~'... ~'. ~@(map ?! !group2) ~'...)
      ('guard (= 1 (+ (count ~(?! (first !group1))) (count ~(?! (first !group2)))))))))

#_#_#_#_#_#_
(||-! '(?x ?y | ?x))
(||-! '(?a . ?x ?y | ?x . ?z))
(||-! '(?x ?y | ?x . ?z))
(||-! '(?a . ?x ?y | ?x))
(||-! '(?x | ?y | ?z))
(||-! '(?a . ?x ?y | ?x))

#_(def qqq (s/rewrite (m/and (?a . !tail ...)
                           (m/let
                             (m/or (([!params ...] . !forms ... :as !fn-specs) ...)
                                   ([!params ...] . !forms ... :as !fn-specs))
                             (list* !tail)))
                    {:specs (!fn-specs ...)}))
#_#_#_#_#_#_
(qqq '(foo [a b] baz))
(qqq '(foo ([a b] baz)))
(qqq '(foo ([a] baz) ([a b] baz)))
(qqq '(foo ([]) ([a b] baz)))
(qqq '(foo []))
(qqq '(foo f [a b] baz))

(def sig '(?x ?y | ?x))
(def sig '(([!params ...] ??body) ... | [!params ...] ??body))
(def sig '(?name ??docstring ??attr-map . (m/and !1) ... | (m/and !2) ...))
#_
((s/rewrite (m/and [(m/and [!xs ...] (m/let !nxs (count !xs))) ...])
            {:xs [!xs ...]
             :nxs [!nxs ...]})
  [[1 2 3] [4 5]])

(def ttt (s/rewrite (m/and (?a . !b !c ... . [!b !c] ...)
                           (m/guard (= (count !b) 1)))
                    (?a :> . !b ... . :> . !c ...)))
#_#_#_#_#_#_
(ttt '(1 2 3))
(ttt '(1 [2 3]))
(ttt '(1 2 3 4))
(ttt '(1 2 3 2 3))
(ttt '(1 [2 3 4] [2 3 4]))
(ttt '(1 [2 3 4] [2 3 4] [2 3 4] [2 3 4]))


#_(or ([_ ...] . _ ...)
      (([_ ...] . _ ...) ...))
