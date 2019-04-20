# Justice

A [Clojure/Script](https://clojure.org/) library providing a concise rule query syntax for [Datalog](https://en.wikipedia.org/wiki/Datalog).

![Lady Justice](https://cdn.dribbble.com/users/101244/screenshots/2921435/lady_justice.jpg)


## Rationale

In his keynote,
[Zeno and the tar pit](https://skillsmatter.com/skillscasts/12820-keynote-zeno-and-the-tar-pit),
Christope Grand argues that Datalog is better for modeling data than maps,
but also more cumbersome.

Christope presents a syntactic fix for rule application;

    ancestor(Me,P) :- parent(Me,P).
    ancestor(Me,G) :- parent(Me,E), ancestor(E,G).

Can be more concisely written as:

    ancestor(Me) := parent(Me) | ancestor(parent(Me)).

Where the bridging variables P, G, and E don't need to be written as they are implied.

Justice is an implementation of this syntax fix for DataScript.

DataScript and Datomic provide a powerful entity abstraction.
Justice offers a concise mechanism for constructing queries that return entities,
and helpers to observe change that affects those entities.
Abstracting query as navigation removes a tedious layer of incidental complexity;
the maintenance of queries and transformations.


## Usage

Justice is in alpha; the API may change in response to feedback.
I'd love to hear your suggestions.

Add justice to your dependencies:

    [justice "0.0.3-alpha"]

Add [DataScript](https://github.com/tonsky/datascript) to your dependencies.

See [basic examples](examples/basic) for executable code described below.


### Setup (DataScript)

First we need a schema and some data defined in order to create queries and rules.
Let's start with some facts about people indicating who their parents are.
To keep things simple for this example I will restrict people to only have one parent.

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

The schema defines an `:entity/parent` attribute.
In the example data "Justice" has a parent named "Mother",
who in turn has a parent named "Grandmother".
"Good Child" and "Bad Child" both have "Justice" as their parent.


### Creating rules

An ancestor of a person can be a parent, or the parent's ancestor.
Let's create a rule that captures this relationship.

Require justice from your code:

    (ns basic.main
      (:require [justice.core :as j]
                [basic.setup :as s]))

Rules are defined in a similar way to `defn`:

    (j/defrule ancestor [?x]
      (or (:entity/parent ?x)
          (ancestor (:entity/parent ?x))))

The notation `(:entity/parent ?x)` is consistent with using keywords as get functions.
You can read this expression informally as "get the parent of input x."

The clause `(ancestor (:entity/parent ?x))` implies a recursive application of the rule.
If the input x has a parent, then the final result can be the ancestor of the parent of x.
This notation is consistent with function application.
You can read it informally as "the ancestor of the parent of input x."

The term `or` implies that either clause will match with existing facts.

Defining a rule adds it to a global, namespaced rules registry,
and creates a convenience function of the same name to query it with.


### Applying rules

`defrule` created a function that we can invoke.
But we still need a database to resolve the rule against.

Passing a database as the first argument to a rule:

    (ancestor @s/conn 1)
    ;=> (#:db{:id 3} #:db{:id 2})

Attaching to a database connection:

    (j/attach s/conn)

After attaching, rule applications can omit the database argument:

    (ancestor 1)
    ;=> (#:db{:id 3} #:db{:id 2})

The result of applying a rule is often a sequence of [entities](https://docs.datomic.com/on-prem/entities.html).
Entities provide a lazy, associative view of all the information that can be reached from an id.

    (map :entity/name (ancestor 1))
    ;=> ("Grandmother" "Mother")
    
The input `1` is the entity id of "Justice".
The results is 2 entities.
Their names are "Grandmother" and "Mother".
They are the ancestors of "Justice."

Entities only pull attributes when you access them.
To realize all attributes, use `d/touch`.
When following relations, the result is another Entity.

A [lookup ref](https://docs.datomic.com/on-prem/identity.html) can be supplied instead of an entity id:

    (ancestor [:entity/name "Justice"])
    ;=> (#:db{:id 3} #:db{:id 2})

An Entity can be supplied instead of an entity id or lookup ref:

    (ancestor {:db/id 1})
    ;=> (#:db{:id 3} #:db{:id 2})

The inputs and results of a rule application may also be a scalar values.
The value of the attribute might be a string or a number.
Justice uses the database schema to determine whether a relation is a ref or not.
Prefer the creation of rules that take and return entities,
as you can navigate to scalars conveniently using the entity pattern.


### Attribute direction

Clauses can be inverted with the `_relation` reverse lookup convention:

    (defrule descendant [?x]
      (or (:entity/_parent ?x)
          (descendant (:entity/_parent ?x))))
    (map :entity/name (descendant 1))
    ;=> ("Good Child" "Bad Child")

This notation is consistent with Entity navigation.
Given an Entity `e`, you can find the children of `e` with `(:entity/_parent e)`.
This can be read informally as "Who has parent e?"


### Plain old queries

Rules are cool when you want recursion and composition,
but rules are often not necessary for queries.

    (j/q (:entity/parent (:entity/parent 1)))
    ;=> (#:db{:id 3})

`q` is a shorthand way to construct simple queries to find entities.
Here we were able to concisely ask "Who is the grandparent of 1?".
This is the same as:

    (:entity/parent (:entity/parent (d/entity @s/conn 1)))
    ;=> #:db{:id 3}

However we can use logic and rules inside `q` expressions:

    (j/q '(basic.main/ancestor (basic.main/descendant 1)))
    ;=> (#:db{:id 3} #:db{:id 2} #:db{:id 1})

    (j/q '(and
            (:entity/parent 1)
            (:entity/_parent 3)))
    ;=> (#:db{:id 2})

We could save a find query into a function:

    (defn grand-parent [x]
      (j/q (:entity/parent (:entity/parent x)))

But consider using `defrule` instead:

    (defrule parent [?x]
      (:entity/parent ?x))

Justice rules provide the same behavior as queries, but rules are more flexible.


### Query direction

Previously we asked "Who are the ancestors of Justice?".
Writing `(ancestor 1)` was actually shorthand for providing both sides of the relation:

    (ancestor 1 '?result)

We were able to leave `'?result` off because it is implied,
and the advantage is that we can think of rules as behaving like a function.

Now we shall ask "Who has Justice as an ancestor?"

    (map :entity/name (ancestor '?x 1))
    ;=> ("Good Child" "Bad Child")

Here we are asking for the `?result` to be 1, for some `?x`.

We can avoid providing both sides of the relation by reversing it:

    (map :entity/name (j/q (_ancestor [:entity/name "Justice"])))
    ;=> ("Good Child" "Bad Child")

Rules can have their direction reversed, using the `_` reverse lookup convention,
analogous to how attribute lookup can be reversed.
This expression can be read informally as "Who has an ancestor named Justice?"


### Cartesian product

Rules can be called with no arguments at all to query all possible answers based on existing facts:

    (->>
      (ancestor)
      (map (partial map :entity/name))
      (sort-by first))
    ;=> (("Bad Child" "Grandmother")
    ;    ("Bad Child" "Mother")
    ;    ("Bad Child" "Justice")
    ;    ("Good Child" "Grandmother")
    ;    ("Good Child" "Mother")
    ;    ("Good Child" "Justice")
    ;    ("Justice" "Grandmother")
    ;    ("Justice" "Mother")
    ;    ("Mother" "Grandmother"))

Supplying no arguments is syntactic sugar for unbound input and result `(ancestor '?x '?result)`.


### Truth checking

Rules can be called with 2 arguments to test if the rule holds:

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

You can use the same approach to introduce new logic variables.
We could have introduced `?death` to be bound to "date of death" instead of the ignoring the value with `_`.
Thus we can chain together conditions with other clauses.


### Select clauses

Justice does not produce "row" results (non-Entities).
Results are intended to be either scalars or navigated using the Entity interface.
Rows can be approximated with `(juxt :field1 :field2)`to produce a function that will call `:field1` and `:field2` on an Entity,
but the Entity abstraction is preferable for both clarity and performance.

Note that where one might otherwise want two separate Entities returned side by side,
it is possible to instead split the query into two parts...
Finding one of the Entities, and finding the related Entity.
Keep in mind that this all reduces down to in memory map lookups,
so splitting queries up does not necessarily imply extra overhead.


### Rule arity

You can define rules that take many arguments instead of just 1.

    (defrule example [?x ?y ?z])

You can define rules that accept multiple arities similar to `defn`:

    (defrule example ([?x] ...) ([?x y?] ...))

But you cannot define rules with variadic arguments:

    (defrule example [?x & ?more])
    ;=> ERROR

See the "Relations" section for more information on this limitation.


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


### Namespaces

When registering a rule, rule names are prefixed by the current namespace.
The full name of our `ancestor` rule is `example.basic/ancestor`.

When applying a rule from within another rule, justice fully qualifies the short name with the current namespace.
We call `ancestor` from `dead-ancestor` by it's short name because the code for both is in the same namespace.

To call a rule from a different namespace from within a rule you must use the fully qualified name.
For example for `dead-ancestor` in `basic.example` to apply `my-rule` in `other.ns`,
it must do so using the full name `other.ns/my-rule`.
No namespace aliasing is provided (yet).

We can define another rule with the same short name in another namespace without collision.
Two versions of `ancestor` can exist as `example.basic/ancestor` and `other.ns/ancestor`, as different rules.

The fully qualified names produced by for the generated query aid in debugging;
the name indicates where the rule is defined in the source code.


### Transacting

Justice provides `transacte` which behaves very similar to `d/transact!` but returns the first entity.

`assox` operates on an entity to produce a transaction, transacts it, and returns a new entity.

`updatex` operates on an entity to produce a transaction, transacts it, and returns a new entity.

`dissox` operates on an entity to produce a transaction, transacts it, and returns a new entity.


### Reacting to change

Justice provides a `reactive` namespace with several helpers to assist listening to change.
These are intended for use in React UIs, and similar scenarios.


### Warning: justice without mercy

It is possible to express non-terminating recursive rules in justice,
just as it is in Datalog.


### Escaping the justice system

The justice syntax is more concise than DataScript queries and handles several shorthand conventions.
These conventions restrict what can be expressed in a query.

The `?result` symbol is special, it is always bound to the final result.

    (j/defrule ancestor* [?x]
      (or [?x :entity/parent ?result]
          (and [?x :entity/parent ?z]
               (ancestor* ?z ?result))))
    (map :entity/name (ancestor* 1))
    ;=> ("Grandmother" "Mother")

This new rule `ancestor*` is equivalent to the original `ancestor`,
but has been expressed in relation triple clauses.

Triple relations will have their direction swapped where the `/_` reverse lookup convention is used,
in the same way that justice syntax emulates entity navigation.

    [?result :entity/_parent ?x]
    
Will be rewritten because DataScript queries require forward ordering only.

    [?x :entity/parent ?result]

You can use relation triple style to opt out of transformation.

You can escape the result abstraction by calling the rule with it's full arity.

    (map :entity/name (ancestor '?x 1))
    ;=> ("Good Child" "Bad Child")

And indeed you can call rules directly with whatever find clauses you want (see `apply-rule`).

That said, I claim there is good reason to model data using binary relations and chaining results.
This is explained further in the Relations section.


## How it works

Justice rewrites the rule syntax into Datalog queries with rule clauses.
The pattern based rewriting is made possible by the excellent [Meander](https://github.com/noprompt/meander) library.

This section discusses how justice abstractions translate to the DataScript layer.


### The rule registry

The `(defrule ancestor ...)` form produces code that constructs a query similar to:

    (d/q
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

The justice clause `(:entity/parent ?x)` translates to a DataScript relation clause `[?x :entity/parent ?result]`.
The order of the relation is the reverse of the justice abstraction.
Bridge variables are created to join the clauses together.

Justice maintains a global rule registry of all rules created with `defrule`.


### Relations

Datalog rules have heads that define bindings and bodies that define relationships between those bindings.
Strictly speaking, rules are not functions and do not produce results.

The DataScript signature of the `ancestor` rule is `(ancestor ?descendant ?ancestor)`.
The rule has 2 bindings.
Rules define complex relationships, not queries.
The `ancestor` rule defines a relationship between entities; between `descendants` and `ancestors`.
A query is formed by providing values that must be matched, or leaving variables unbound.
We can choose to supply one input, all inputs, or none of the inputs.
The find clause of the query contains all unbound variables.
The find clause is implied by the inputs we chose to supply.

Results for a rule is an abstraction introduced to make them look like functions.
Quite often this is a useful abstraction.

You can escape the result abstraction by calling the rule with it's full arity.

    (map :entity/name (ancestor '?x 1))
    ;=> ("Good Child" "Bad Child")

Justice embraces a convention that rules are most often used by binding all but the last input to values,
and that the find clause of a query will pull out any matches for that last input.
By convention `defrule` produces a convenience function that implies a hidden `?result` argument.
The convenience function accepts multiple arities, where leaving off an argument creates an unbound input,
which in turn is used in the find clause of a query.
The form `(defrule my-rule [?x])` registers a rule with signature `(my-rule ?x ?result)`.
Applying `(my-rule 1)` executes a query with input `?x` bound to `1`, with the find clause containing `?result`.
Applying `(my-rule '?x 1)` executes a query with the implicit input `?result` bound to `1`, with a find clause containing `?x`.
The name `?x` chosen here does not matter, it can be any symbol starting with `?` except `?result`, which is special.

The convention of leaving off `?result` is followed to make rule application feel like function application.
`_` is syntactic sugar to maintain the abstraction while using rule relations in reverse.


### Rule arity

Rules can have multiple arities, and `defrules` allows you to create these using `defn` syntax.
When calling multi-arity rules, the convenience function for applying the rule resolves to the rule arity first.
Let's look at an example:

    (defrule example
      ([?x] ...)
      ([?x ?y] ...)

This will create relation rules for both binary and ternary bindings.

There are a multitude of ways we can call the convenience function for this rule:

    (example 1)
    ;; the result of the single arity version (binary rule signature)

    (example 1 2)
    ;; true/false of the single arity version (binary rule signature)

    (example 1 2 3)
    ;; true/false of the dual arity holds (ternary rule signature)

    (j/q (_example 3)
    ;; the single arity version (binary rule signature)

    (j/q (_example 4 1)
    ;; true/false of the single arity version (binary rule signature)

    (j/q (_example 1 2 5)
    ;; true/false of the dual arity holds (ternary rule signature)

    (example)
    ;; the cartesian product of the single arity version (binary rule signature)

    (example '?x)
    ;; the cartesian product of the single arity version (binary rule signature)

    (example '?x 'y)
    ;; the cartesian product of the single arity version (binary rule signature)

    (example '?x '?y '?z)
    ;; the cartesian product of the dual arity version (ternary rule signature)

    (example 1 '?x)
    ;; the single arity version is called

Datalog rules do not *have* to follow the hidden `?result` convention.
Even justice rules don't have to follow it.
But the meaning of relations is quickly eroded in the absence of the "result" convention.
For example a rule `(my-rule ?a ?b ?c ?d)` that you call as `(my-rule 1 ?b ?c 2)` is very confusing to think about.
We are probably better off just writing the relation we want instead of creating a rule here.
On the other hand `(my-rule ?x ?y ?z ?result)` makes a lot of sense if `?x ?y ?z` are filter values where I'm trying to find things that pass those filters.
So I think it is a good convention to favor that the last binding of a rule is often to be left unbound.

Rules cannot be variadic in Datalog, so `defrules` disallows that.

Higher order rules are not supported as it is unclear what semantics should be attached to them.


### Open closed

Datalog rules are conceptually "open" in the sense that a rule can be extended externally.
Consider the expanded DataScript rules for `ancestor` stored in the global rule registry:

    [[(basic.main/ancestor ?x ?result)
      [?x :entity/parent ?result]]
     [(basic.main/ancestor ?x ?result)
      [?bridge_23980 :entity/parent ?result]
      (basic.main/ancestor ?x ?bridge_23980)]]

We can update the global rule registry by adding a new clause on the end to extend the definition.
Rules can have multiple bodies.
New signatures and bodies can be added to existing rules.
In this sense they are more like multimethods than functions.

However, justice encourages using a more restrictive "closed" approach to defining rules.

Restrictive "closed" style:

    (ns my.ns)
      (defrule my-rule
        ([?x]
         (or body1 body2 body3))
        ([?x ?y]
         (or body4 body5)))

Instead of "open" style:

    (ns this.ns)
      (register-rule 'my-rule '[?x] body1)
      (register-rule 'my-rule '[?x] body2)
    (ns that.ns)
      (register-rule 'my-rule '[?x] body3)
      (register-rule 'my-rule '[?x ?y] body4)
      (register-rule 'my-rule '[?x ?y] body5)

The problem with "open" rule definitions is uncertainty of the full definition.
If part of the rule is defined in a different namespace that has not been loaded,
the dispatch options are not connected, and we would be missing a clause in the rule registry.
When developing interactively the registry might longer reflect the code.

When you control the source code, "closed" style is an advantage.
Fully defining rules as a single body per arity in a single namespace is avoids any uncertainty.
The rule registry will always reflect the code.

The main justice convention of `defrule` is intentionally "closed".
However, the justice system is "open" to extension via `register-rule`.


## Developing

[Issues](https://www.github.com/timothypratley/justice/issues) and pull requests welcome.

Running the tests:

    clojure -Atest
    clojure -Acljs-test


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
- [ ] Should defrule create a macro that can take ?x without quoting?


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
