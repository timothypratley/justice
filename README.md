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


### Setup (DataScript)

Consider defining an `ancestor` rule.
Given an entity, the `ancestor` rule will find all ancestors of that entity.
Before we can define this rule we need to create a db to operate on.

We create a schema for this data which defines an `:entity/parent` attribute.
An entity can have only one parent.

    (ns basic.setup
      (:require [datascript.core :as d]))
    (def schema
      {:entity/parent {:db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one}}
    (def conn
      (d/create-conn schema))
    (def seed
      [{:db/id -1
        :character/name "A"}
       {:db/id -2
        :character/name "B"
        :entity/parent -1}
       {:db/id -3
        :character/name "C"
        :entity/parent -2}
       {:db/id -4
        :character/name "D"
        :entity/parent -3}])
    (d/transact! conn seed)

Now we are ready to define our rule.


### Creating rules

Require justice from your code:

    (ns basic.main
      (:require [justice.core :as j]))

Rules are defined in a similar way to `defn`:

    (j/defrule ancestor [?x]
      (or (:entity/parent ?x)
          (:entity/parent (ancestor ?x))))

The clause `(:entity/parent ?x)` implies that
the result is the value of the attribute `:entity/parent` of the input entity.

The clause `(ancestor ?x)` implies a recursive application of the rule.

The clause `(:entity/parent (ancestor ?x))` implies that the inner result has attribute `:entity/parent`,
and that the value of that attribute is the outer result.

The term `or` implies that either sub-clause will match with existing facts.


### Applying rules

Rules are functions.
But rules need a database in context to resolve against.
There are two ways to pass in the database:

1. Calling a rule with a database as the first argument:
   `(ancestor @conn 1) ;=> [{:db/id 2} {:db/id 3}]`
2. Alternatively, you can register a database connection:
   `(j/register conn)`    
   Subsequently all rule applications can omit the database argument:
   `(ancestor 1) ;=> [{:db/id 2} {:db/id 3}]`

The result of applying a rule is a sequence of [entities](https://docs.datomic.com/on-prem/entities.html).
Entities provide a lazy, associative view of all the information that can be reached from an id.

    (map :entity/name (ancestor 1))
    ;=> ("A", "B", "C")

A [lookup ref](https://docs.datomic.com/on-prem/identity.html) can be supplied instead of an entity id:
    
    (ancestor [:entity/name "Justice"])
    ;=> ("A", "B", "C")
    
An entity can be supplied instead of an entity id:

    (ancestor {:db/id 1})
    ;=> ("A", "B", "C")


### Directionality

Rules can be inverted by supplying an unbound variable:

    (ancestor ?x [:entity/name "Justice"])
    ;=> [{:db/id 4}]

Fact clause direction can be inverted with the `_` convention:

    (defrule descendant [?x]
      (or (:entity/_parent ?x)
          (:entity/_parent (descendant ?x))))
    (descedant [:entity/name "Justice"])
    ;=> [{:db/id 4}]


### Cartesian Product

Rules can be called with no arguments:

    (ancestor)
    ;=> [[{:db/id 1} {:db/id 2}]
         [{:db/id 1} {:db/id 3}]]

Resulting in all possible answers based on existing facts.


### Composing rules

Rules can call other rules:

    (defrule dead-ancestors [?x]
      (and (:entity/_death ?x)
           (ancestor ?x)))

    (dead-ancestors [:entity/name "Justice"])
    ;=> ("A", "B")


### Warning: justice without mercy

It is possible to express non-terminating recursive rules in justice,
just as it is in Datalog.

    
## How it works

Justice rewrites the rule syntax into Datalog queries with rule clauses.

The `ancestor` rule presented previously produces code very similar to this:

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

Justice syntax is more concise than DataScript queries and handles several shorthand conventions.
However, justice syntax is restricted in what can be expressed.
There is no way (yet) to produce "row" results (non-entities).


## Developing

[Issues](https://www.github.com/timothypratley/justice/issues) and pull requests welcome.


## Todo

- [ ] Support Datomic as well as DataScript.
- [ ] Should rules allow multiple input variables? `(defrule r [?x ?y] ...)`
- [ ] Should there be a way for rules to produce rows? (non-entity results)
      -- I don't think so, the whole point is to not produce maps.
- [ ] Is there be a concise syntax for updates?
- [ ] Provide syntax checking and nice error messages.
- [ ] Check for left recursive forms.
- [ ] Should there be a way to invert the direction of rule recursion?
      -- possibly just by providing another unbound variable?


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
