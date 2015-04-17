# Rw-Prolog

## Overview

Rw-Prolog is an extension of Prolog, embracing equational logic programming through term rewriting systems. Rw-Prolog is implemented in Prolog on top of a library for reasoning about term reductions. At a high level, Rw-Prolog behaves like a traditional Prolog except goal terms may be reduced to different terms during the resolution procedure. The rules governing such reductions are defined by the user.

Extending Prolog in this fashion allows for greater flexibility for the programmer. On one extreme, the user may choose not to incorporate rewrite rules and write code as pure logic programs. On another extreme, the programmer may use only deterministic rewrite rules, resulting in very functional code. In yet another approach, the user may use rewrite rules to build a method-call syntax for state terms, resulting in more object-oriented code.


## Definitions

A **rewrite rule** is a regular Prolog rule of the form `Pattern := Template :- Conditions.`[^1] The existence of a rewrite rule means that terms matching the *pattern* may be replaced according to the *template* if the *conditions* are true. A set of rewrite rules is called a **rewriting system**.

A reducible expression, or **redex**, is a term that unifies with the pattern of some rewrite rule. We say a term `t` "has a redex" if `t` is a redex or if any subterm of `t` has a redex. We also say a redex "matches" a rewrite rule if it unifies with the pattern of the rewrite rule. The process of replacing a redex in a term is called **contracting** the redex.

A **reduction** from term `t1` to `t2` is the result of replacing a redex in `t1` to produce `t2`. The redex must be contracted according to the template of the rewrite rule to which the redex matches, and only if the conditions of the rule are true. We say `t1` "reduces" to `t2`.

Reductions may be further limited by a **reduction strategy**. For example, in the outermost-leftmost strategy, if multiple redexes exist in a term, then only the first redex found in a leftmost-depth-first search may be replaced. Formally, a reduction strategy is a function `φ` that assigns to every term `t` a set of positions in `t` that may be reduced.

We say a term is a **normal form** when it cannot be reduced. And a term is a **terminal form** when it denotes a satisfiable formula of first-order logic.

Rewrite systems in which every term reduces to a single terminal form are called **consistent**. Consistent systems in which every term has a single path to its terminal form are said to be **unambiguous**.

The **rewrite graph** is the directed graph over terms whose edges are defined when a reduction exists between terms. The leaves of the graph are normal forms.

[^1]: `:=/2` is an operator with precedence 990 and no associativity.


## Semantics

Rw-Prolog extends the semantics of Prolog to embrace term rewriting. Specifically, a query in Rw-Prolog is satisfiable iff it is itself satisfiable in first-order logic (as in traditional Prolog) or it can be reduced to term which is satisfiable. The term to which the satisfiable query reduces is called the terminal form, and the rewrite rules are themselves represented as Horn clauses in the system's knowledge-base.

To determine if a formula is satisfiable in first-order-logic, Rw-Prolog implements SLD-resolution by delegating to the underlying Prolog system. When a formula is found to be unsatisfiable, Rw-Prolog engages in a search for a terminal form in the formula's rewrite graph. When found, the formula is replaced by the terminal form, and the execution procedure continues to the next goal. Rw-Prolog imposes as few restrictions as possible upon the rewrite rules. In particular, the rewrite system need not be consistent. Thus upon backtracking, the graph search resumes to find more terminal forms.

### The Reduction and Search Strategies of Rw-Prolog

Searching for terminal forms in possibly inconsistent systems poses additional problems not found in traditional consistent systems. Namely, inconsistency means that we must leave choice points whenever a decision is made as to which redex to contract, because contracting redexes in different orders may lead to different terminal forms. Fortunately, many computable functions can be easily expressed with minimal amounts of ambiguity and inconsistency.

The reduction strategy of Rw-Prolog is known as the **parallel-outermost** strategy. Formally, `φ(t)` is the set of all positions `p` of redexes in `t` such that the redex at `p` is not contained within another redex. In other words, when searching `t` for redexes, Rw-Prolog does not consider sub-terms of know redexes as candidates for contraction.

The parallel-outermost strategy may return more than one position that can be reduced. However, we cannot contract all possible redexes simultaneously because we may reach a terminal from by contracting only one redex but skip the terminal from by contracting two redexes. Instead, Rw-Prolog implements a breadth-first search of the rewrite graph. Here is the algorithm:

- Let `Q` be a FIFO queue of terms to reduce, initially containing only the subject term.
- Until `Q` is empty:
	- Pop a term `t` from `Q`.
	- If `t` is a terminal form,
		- Yield `t`.
	- Otherwise,
		- For all possible reductions in `t` identified by the reduction strategy, create a new term `t'` by contracting the redex.
		- Append all `t'` to `Q`. (The order of the `t'` is undefined.)

The search strategy will yield all possible terminal forms of `t`. However, if the rewrite graph contains cycles and no terminal froms, then the search may not terminate.
