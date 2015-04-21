# Rw-Prolog

Chris Barrick


## Overview

An equational logic programming language is one in which computation is described as the process of deriving a normal form of a query formula. Such is similar to the way a classical logic programming language, like Prolog, describes computation as the process of deriving logical consequence of a query formula. A functional programming language and an equational logic programming language are essentially the same from the perspective of the user, in that the user describes an equivalence between formulas and computes by providing formulas to be transformed. The difference between these types of language is that of semantics (O'Donnell, 2005). In a functional language, the equivalences of are expected to hold in a unique model of the equations in the program, while equational logic semantics expect the equivalences to hold in all possible models. This extra generality imbues the program itself with logical significance, much like classical logic programming.

Rw-Prolog is an attempt to combine classical and equational logic programming in a flexible way, allowing the user to choose amongst logical, functional, and object oriented programming styles. More specifically, Rw-Prolog is an extension of the Prolog resolution procedure that considers user defined equivalences amongst terms. This is accomplished by applying special semantics to the user defined predicate `:=/2`.the set of rules for `:=/2` are considered to define a conditional term rewriting system describing how terms may be reduced into equivalent form. If a particular query would normally fail in traditional Prolog, the query is transformed according to the user defined rewrite system, and resolution is attempted on the reduced query. This shifts the goal of reduction away from finding normal forms towards finding resolvable queries.

Rw-Prolog paces no restrictions on the rewrite rules. Should the programmer want, she could define ambiguous or inconsistent systems, requiring the rewrite procedure be non-deterministic to be complete.


## Definitions

Some vocabulary must be presented to discuss the semantics of Rw-Prolog. The definitions given here are only to be taken relative to Rw-Prolog, yet they are based on the general theory of term rewriting systems described by Baader & Nipkow (1998).

A **rewrite rule** is a regular Prolog rule of the form `Pattern := Template :- Conditions.` The existence of a rewrite rule means that terms matching the *pattern* may be replaced according to the *template* if the *conditions* are true. A set of rewrite rules is called a **rewriting system**.

A reducible expression, or **redex**, is a term that unifies with the pattern of some rewrite rule. We say a term `t` "has a redex" if `t` is a redex or if any subterm of `t` has a redex. We also say a redex "matches" a rewrite rule if it unifies with the pattern of the rewrite rule. The process of replacing a redex in a term is called **contracting** the redex.

A **reduction** from term `t1` to `t2` is the result of replacing a redex in `t1` to produce `t2`. The redex must be contracted according to the template of the rewrite rule to which the redex matches, and only if the conditions of the rule are true. We say `t1` "reduces" to `t2`.

Reductions may be further limited by a **reduction strategy**. For example, in the outermost-leftmost strategy, if multiple redexes exist in a term, then only the first redex found in a leftmost-depth-first search may be replaced. Formally, a reduction strategy is a function `φ` that assigns to every term `t` a set of positions in `t` that may be reduced.

We say a term is a **normal form** when it cannot be reduced. And a term is a **terminal form** when it denotes a satisfiable formula of first-order logic.

Rewrite systems in which every term reduces to a single terminal form are called **consistent**. Consistent systems in which every term has a single path to its terminal form are said to be **unambiguous**.

The **rewrite graph** is the directed graph over terms whose edges are defined when a reduction exists between terms. The leaves of the graph are normal forms.


## Semantics

Rw-Prolog extends the semantics of Prolog to embrace term rewriting. Specifically, a query in Rw-Prolog is satisfiable iff it is itself satisfiable in first-order logic (as in traditional Prolog) or it can be reduced to term which is satisfiable. The term to which the satisfiable query reduces is called the terminal form, and the rewrite rules are stored as Horn clauses in the knowledge-base as regular Prolog rules of the `:=/2` predicate.

To determine if a formula is satisfiable in first-order-logic, Rw-Prolog implements SLD-resolution by delegating to the underlying SWI-Prolog system. When a formula is found to be unsatisfiable, Rw-Prolog engages in a search for a terminal form in the formula's rewrite graph. When found, the formula is replaced by the terminal form, and the execution procedure continues to the next goal. Rw-Prolog imposes as few restrictions as possible upon the rewrite rules. In particular, the rewrite system need not be consistent. Thus upon backtracking, the graph search resumes to find more terminal forms.

### The Reduction and Search Strategies of Rw-Prolog

Searching for terminal forms in possibly inconsistent systems poses additional problems not found in traditional consistent systems. Namely, inconsistency means that we must leave choice points whenever a decision is made as to which redex to contract, because contracting redexes in different orders may lead to different terminal forms. Fortunately, many computable functions can be easily expressed with minimal amounts of ambiguity and inconsistency.

The reduction strategy of Rw-Prolog is known as the **parallel-outermost** strategy. Formally, `φ(t)` is the set of all positions `p` of redexes in `t` such that the redex at `p` is not contained within another redex (Nedjah & Mourelle, 2003). In other words, when searching `t` for redexes, Rw-Prolog does not consider sub-terms of know redexes as candidates for contraction.

The parallel-outermost strategy may return more than one position that can be reduced. However, we cannot contract all possible redexes simultaneously because we may reach a terminal from by contracting only one redex but skip the terminal from by contracting two redexes. Instead, Rw-Prolog implements a breadth-first search of the rewrite graph. Here is the algorithm:

- Let `Q` be a FIFO queue of terms to reduce, initially containing only the subject term.
- Until `Q` is empty:
	- Pop a term `t` from `Q`.
	- If `t` is a terminal form,
		- Yield `t`.
	- Otherwise,
		- For all possible reductions in `t` identified by the reduction strategy, create a new term `t'` by contracting the redex.
		- Append all `t'` to `Q`. (The order of the `t'` is undefined.)

The search strategy will yield all possible terminal forms of `t`. However, if the rewrite graph contains cycles and no terminal forms, then the search may not terminate.


## Applications

A proof of concept application was developed along with Rw-Prolog for compiling and running regular expressions/NFAs. The regular expression module defines two reduction sequences. The first reduces terms of the form `regexp(Expression)` into the normal form `regexp(Expression, NFA)` where `NFA` is a representation of the finite automaton compiled from the `Expression`. The second reduction sequence allows terms of the form `regexp(_, NFA)::match(String)` to reduce to the terminal form `true`. Together, these reductions allow the end-user to resolve queries like `regexp(Expression)::match(String)` without worrying about the internal representation of the automaton.

This application showcases an object-oriented design pattern facilitated by equational logic. Object state can be maintained in a reusable term with the `::/2` operator acting syntactically as a method resolution operator.

This toy application also provides opportunity to discuss one of Rw-Prolog's greatest weeknesses. Unfortunately, resolving a compound query like `regexp(Exp)::match(A), regexp(Exp)::match(B)` requires that the NFA be compiled, for both goals. Future work on the project may be towards identifying and remembering important results, allowing reduction steps to be skipped in the best case.


## References

Baader, F., & Nipkow, T. (1998). *Term Rewriting and All That.* Cambridge: Cambridge University Press.

Nedjah, N., & Mourelle, L. (2003). *Implementation of Term Rewriting-Based Programming Languages.* New York: Nova Science Publishers, Inc.

O'Donnell, M. (2005). Equational Logic Programming. In D. Gabbay, C. Hogger, & J. Robinson (Eds.), *Handbook of Logic in Artificial Intelligence and Logic Programming* (Vol. 5). Oxford: Oxford University Press.
