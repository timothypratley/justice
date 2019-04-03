# Justice

A [Clojure/Script](https://clojure.org/) library providing a concise rule definition and query syntax for [Datalog](https://en.wikipedia.org/wiki/Datalog).

![Lady Justice](https://cdn.dribbble.com/users/101244/screenshots/2921435/lady_justice.jpg)


## Rationale

In his keynote,
[Zeno and the tar pit](https://skillsmatter.com/skillscasts/12820-keynote-zeno-and-the-tar-pit),
Christope Grand argues that Datalog is better for modeling data than maps,
but that the syntax is cumbersome.

Christope presents a syntactic fix for rule application;

    ancestor(Me,P) :- parent(Me,P).
    ancestor(Me,G) :- ancestor(Me,E), parent(E,G).

Can be more concisely written as:

    ancestor(Me) := parent(Me) | parent(ancestor(Me)).

Where bridge variables are implied.

Justice is a Lisp flavoured implementation of this syntax on top of DataScript,
providing a concise way to create and query with rules.


## Usage

Justice is in alpha; the API may change in response to feedback.
I'd love to hear your suggestions.

Add justice to your dependencies:

    [justice "0.0.1-alpha"]

See [examples](examples) for executable code described below.


### Setup (DataScript)

Say we want to define an `ancestor` rule.
We might start with some facts indicating who is a `parent` of who.
And we want to say that an ancestor is a parent, or ancestor of a parent.

Before we can define this rule we need to create a db schema and some test data to operate on.
The schema defines an `:entity/parent` attribute.
For this example an entity can have only one parent.

    (ns basic.setup
      (:require [datascript.core :as d]))

    (def schema
      {:entity/parent {:db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one}})

    (def conn
      (d/create-conn schema))

    (def seed
      [{:entity/name "Justice"
        :entity/parent {:entity/name "Mother"
                        :entity/parent {:entity/name "Grandmother"
                                        :entity/death "278 BC"}}
        :entity/_parent [{:entity/name "Good Child"}
                         {:entity/name "Bad Child"}]}])

    (d/transact! conn seed)

Now we are ready to define our rule.


### Creating rules

Require justice from your code:

    (ns basic.main
      (:require [justice.core :as j]
                [basic.setup :as s]))

Rules are defined in a similar way to `defn`:

    (j/defrule ancestor [?x]
      (or (:entity/parent ?x)
          (ancestor (:entity/parent ?x))))

The clause `(:entity/parent ?x)` implies that
the result is the value of the attribute `:entity/parent` of input `?x`.
This notation is consistent with a get value by keyword from a map.
You can read this as "`?x` has an `:entity/parent` value `?result`",
which is consistent with the DataScript where clause `[?x :entity/parent ?result]`.

The clause `(ancestor (:entity/parent ?x))` implies a recursive application of the rule.
The input `?x` has an `:entity/parent`.
That parent will be used in the application of `ancestor`.
The result will be the result of ancestor applied to the parent of `?x`.
This is consistent with function application.

The term `or` implies that either sub-clause will match with existing facts.


### Applying rules

Rules are functions.
Rules need a database in context to resolve against.
There are two ways to pass in the database:

Passing a database as the first argument:

    (ancestor @s/conn 1)
    ;=> (#:db{:id 3} #:db{:id 2})

Attaching to a database connection:

    (j/attach s/conn)

After attaching, rule applications can omit the database argument:

    (ancestor 1)
    ;=> (#:db{:id 3} #:db{:id 2})

The result of applying a rule is a sequence of [entities](https://docs.datomic.com/on-prem/entities.html).
Entities provide a lazy, associative view of all the information that can be reached from an id.

    (map :entity/name (ancestor 1))
    ;=> ("Grandmother" "Mother")
    
Entities only pull attributes when you access them.
To realize all attributes, use `d/touch`.
When following relations, the result is another Entity.

A [lookup ref](https://docs.datomic.com/on-prem/identity.html) can be supplied instead of an entity id:

    (ancestor [:entity/name "Justice"])
    ;=> (#:db{:id 3} #:db{:id 2})

An Entity can be supplied instead of an entity id or lookup ref:

    (ancestor {:db/id 1})
    ;=> (#:db{:id 3} #:db{:id 2})


### Query direction

Rules can query what input produces a result value:

    (map :entity/name (j/_invert [:entity/name "Justice"] ancestor))
    ;=> ("Good Child" "Bad Child")

Here we use `_invert` to ask for whome does `ancestor` result in "Justice"?
It is clear that "Justice" is not an input because she appears before the rule in the expression.
You can read this as "Unify Justice with (ancestor ?x)", thus ?x will be an input that results in "Justice".
And indeed `_invert` is syntactic sugar for supplying an unbound variable:

    (map :entity/name (ancestor '?x 1))
    ;=> ("Good Child" "Bad Child")

The name `?x` chosen here does not matter, it can be any variable starting with `?`.


### Attribute direction

Fact clauses can be inverted with the `/_` reverse lookup convention:

    (defrule descendant [?x]
      (or (:entity/_parent ?x)
          (descendant (:entity/_parent ?x))))
    (map :entity/name (descendant 1))
    ;=> ("Good Child" "Bad Child")

This notation is consistent with Entity navigation.
Given an Entity `e`, you can reverse lookup children of `e` with `(:entity/_parent e)`.


### Cartesian Product

Rules can be called with no arguments to get all possible answers based on existing facts:

    (ancestor)
    ;=> ((#:db{:id 4} #:db{:id 3})
    ;    (#:db{:id 2} #:db{:id 3})
    ;    (#:db{:id 4} #:db{:id 2})
    ;    (#:db{:id 5} #:db{:id 3})
    ;    (#:db{:id 4} #:db{:id 1})
    ;    (#:db{:id 5} #:db{:id 2})
    ;    (#:db{:id 1} #:db{:id 3})
    ;    (#:db{:id 5} #:db{:id 1})
    ;    (#:db{:id 1} #:db{:id 2}))

Supplying no arguments is syntactic sugar for `(ancestor '?x '?y)`.


### Truth checking

Rules can be called with 2 Entity arguments to test if the rule holds:

    (ancestor 1 3)
    ;=> true

    (ancestor 1 5)
    ;=> false


### Composing rules

Rules can make use of other rules:

    (j/defrule dead-ancestors [?x]
      (and (:entity/_death _)
           (ancestor ?x)))
    (map :entity/name (dead-ancestors [:entity/name "Justice"]))
    ;=> ("Grandmother")

While it appears that `dead-ancestors` applies `ancestor` directly,
this is not the case.
What really happens here is that justice produces a set of dependent rules,
which are used in a query.
This is important to understand, as it explains why regular functions cannot appear in rules.


### Where clauses

Notice that in the `dead-ancestors` rule, the first clause `(:entity/_death _)` does not relate to the input `?x`.
It does however implicitly relate to the `?result`.
Thus a restriction has been made that the result entity has an `:entity/death`.
This is analogous to a "where" clause in SQL.


### Debugging rules

Having rules be self contained functions makes it easier to invoke them in isolation while debugging.

Sometimes it is helpful to see the DataScript that a justice rule will produce.
Calls to rules wrapped with `trace` will print out the underlying DataScript query being made and return the result.

    (j/trace
      (ancestor 1))
    ;;; QUERY:
    ;   (datascript.core/q
    ;    {:find [[?result ...]],
    ;     :in [$ % ?a _],
    ;     :where [(basic.main/ancestor ?a ?result)]}
    ;    [[(basic.main/ancestor ?x ?result)
    ;      [?x :entity/parent ?result]]
    ;     [(basic.main/ancestor ?x ?result)
    ;      [?bridge_23980 :entity/parent ?result]
    ;      (basic.main/ancestor ?x ?bridge_23980)]]
    ;    1
    ;    ?y)
    => (#:db{:id 3} #:db{:id 2})

You can also look in the rule registry at `*rule-registry*`.
Rules are stored in a map of `rule-name` -> `[[(rule-name ?a ?b) [clause]+]]`.


### Aggregates

A syntax for aggregation is not (yet) provided.
Entities are amenable to aggregate operations.
You can use Clojure's built in aggregates to operate over the sequence of entities produced.

    (count (ancestor 1))
    ;=> 2

Entity navigation can be utilized for more complex aggregations.

DataScript supports aggregations, but it's not clear to me how they would work with this syntax.


### Warning: justice without mercy

It is possible to express non-terminating recursive rules in justice,
just as it is in Datalog.

    
### How it works

Justice rewrites the rule syntax into Datalog queries with rule clauses.
The pattern based rewriting is made possible by [Meander](https://github.com/noprompt/meander).

The `ancestor` rule produces code that constructs a query:

    (datascript.core/q
       '{:find [[?result ...]]
         :where [(ancestor ?x ?result)]
         :in [$ % ?x]}
       @conn
       '[[(ancestor ?x ?result)
          [?x :entity/parent ?result]]
         [(ancestor ?x ?result)
          [?bridge :entity/parent ?result]
          (ancestor ?x ?bridge)]]
       [:entity/name "Justice"])

The justice syntax is more concise than DataScript queries and handles several shorthand conventions.
However, justice syntax is restricted in what can be expressed.
There is no way (yet) to produce "row" results (non-Entities) or aggregates.

Justice maintains a rule registry of all rules created with `defrule`.


### Escaping the justice syntax

The `?result` symbol is special, and that you can escape the justice syntax if you need to:

    (j/defrule ancestor* [?x]
      (or [?x :entity/parent ?result]
          (and [?x :entity/parent ?z]
               (ancestor* ?z ?result))))
    (map :entity/name (ancestor* 1))
    ;=> ("Grandmother" "Mother")

This new version `ancestor*` is equivalent to the original `ancestor`,
but has been expressed in triples instead.
You can use this to opt out of the full transformation but still express concise rules.


### Transacting

Justice provides `transacte` which behaves very similar to `d/transact!` but returns the first entity.


## Developing

[Issues](https://www.github.com/timothypratley/justice/issues) and pull requests welcome.


## Todo

- [ ] Support Datomic as well as DataScript.
- [ ] Should rules allow multiple input variables? `(defrule r [?x ?y] ...)`
- [ ] Should there be a way for rules to produce rows? (non-entity results)
      -- I don't think so, the whole point is to stick with entities?
- [ ] Is there be a concise syntax for updates?
      -- `d/transact!` is already pretty great?
      -- Entity versions of assoc/dissoc/update?
- [ ] Provide syntax checking and nice error messages!!!
- [ ] Check for left recursive forms.
- [ ] Check for unused variables.
- [ ] Find a way to make testable examples


## License

Copyright © 2019 Timothy Pratley

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
