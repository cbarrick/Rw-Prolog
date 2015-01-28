Term-RW
=========================

Term-RW will be an extension of the Prolog programming supporting term
rewriting. Term-RW will be implemented as a meta-interpreter for SWI-Prolog.


Syntax
-------------------------

Term-RW operates upon "rewrite graphs". The edges of the graphs are expressed
as regular Prolog rules using the `:=/2` operator as the principal functor of
the head. Rules are written in the form `A := B :- Guard.` meaning a term `A`
may be rewritten to term `B` if the condition `Guard` is true. Computation
can be expressed through these rewrite graphs where the steps of the algorithm
are encoded as edges in these graphs.


Semantics
-------------------------

**Comming Soon**

The semantics of this system are not yet finalized.


Examples
-------------------------

The predicates responsible for evaluating rewrite rules are located in
`src/rewrite.pl`. The project is currently unstable; for the most up-to-date
information, see the inline documentation in the source.

The most important predicates are `rewrite/2` and `rewrite/3`. The query
`rewrite(A, B)` means that there is some path of any length along the rewrite
graph from term `A` to term `B`. The query `rewrite(N, A, B)` means that there
is a path of length `N` from `A` to `B`. A third predicate, `simplify/2`, exists
that rewrites a term to it's notmal form; however the semantics of normal forms
for nondeterministic graphs is currently unstable.

Example graphs are included in the `lib` directory. Each one tackles different
use-cases. Currently, only one graph may be loaded at any time. To make sure
the system know how to parse the rules, consult `src/rewrite.pl` before
consulting the graph.


### lib/algebra.pl

This graph is the first graph I wrote. It attempts to model algebraic
simplification, much like Mathematica. Algebraic expressions _should_ tend to
a normal, simplified form. Currently, there are bugs in the graph where
equivalent terms may not simplify to the same normal form; however, many
expressions, including most polynomials, work properly.

#### Example:
```prolog
?- ['src/rewrite'].
true.

?- ['lib/algebra'].
true.

?- simplify(X+5+4*X^3+3*X^2, Normal).
Normal = X^3*4+X+5+X^2*3.
```


### lib/fib.pl

This graph models the computation of the fibonacci sequence. Succesive terms in
the graph encode the sequence as a "lazy list"/generator, a popular technique
in functional programming.

#### Example
```prolog
?- ['src/rewrite'].
true.

?- ['lib/fib'].
true.

?- rewrite(fib(0,1,1), X).
X = [0|fib(1, 1, 2)] ;
X = [0, 1|fib(1, 2, 3)] ;
X = [0, 1, 1|fib(2, 3, 5)] ;
X = [0, 1, 1, 2|fib(3, 5, 8)] ;
X = [0, 1, 1, 2, 3|fib(5, 8, 13)] ;
X = [0, 1, 1, 2, 3, 5|fib(8, 13, 21)] ;
X = [0, 1, 1, 2, 3, 5, 8|fib(13, 21, 34)] ;
X = [0, 1, 1, 2, 3, 5, 8, 13|fib(..., ..., ...)] .
```


### lib/infinite.pl

This module provides examples of graphs where terms do not tend to a normal
form but instead always provide a transformation of the terms. It is mostly
useful for internal testing purposes; I use it to to help reason about, debug,
and examine the computational properties rewrite semantics.

#### Example
```prolog
?- ['src/rewrite'].
true.

?- ['lib/infinite'].
true.

?- rewrite(a(x)=b(x), Unifier=Unifier).
Unifier = z(x).
```


### lib/regexp.pl

This module uses term rewriting to model DFA execution and explores how
object-oriented syntax might be incorporated.

The module describes how simple regular expression terms of the form
`regexp(Expression)` can be compiled into NFA terms `regexp(Expression, NFA)`.
It then defines a "match" method using a method-call operator, `::/2`, for NFA
terms which executes the NFA against an input string, simplifying to either
`true` or `fail`.

#### Example
```prolog
?- ['src/rewrite'].
true.

?- ['lib/regexp'].
true.

?- simplify(regexp("aa(a|b)bb"), X).
X = regexp([97, 97, 40, 97, 124, 98, 41, 98|...], [edge(start, state1, 97), edge(state1, state2, 97), edge(state2, state3, 97), edge(state2, state3, 98), edge(state3, state4, 98), edge(state4, accept, 98)]).

?- simplify(regexp("aa(a|b)bb")::match("aabbb"), X).
X = true.

% We can use rewrite/2 to examine the operation of the match method.
% It implements Thomson's algorithm to evaluate the NFA as a DFA.
% (basically, an implicit powerset construction).
?- rewrite(regexp("aa(a|b)bb")::match("aabbb"), X).
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state33, 97), edge(state33, state34, 97), edge(state34, state35, 97), edge(state34, state35, 98), edge(state35, state36, 98), edge(state36, accept, 98)])::match([97, 97, 98, 98, 98]) ;
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state37, 97), edge(state37, state38, 97), edge(state38, state39, 97), edge(state38, state39, 98), edge(state39, state40, 98), edge(state40, accept, 98)])::match([97, 97, 98, 98, 98], [start]) ;
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state41, 97), edge(state41, state42, 97), edge(state42, state43, 97), edge(state42, state43, 98), edge(state43, state44, 98), edge(state44, accept, 98)])::match([97, 98, 98, 98], [state41]) ;
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state45, 97), edge(state45, state46, 97), edge(state46, state47, 97), edge(state46, state47, 98), edge(state47, state48, 98), edge(state48, accept, 98)])::match([98, 98, 98], [state46]) ;
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state49, 97), edge(state49, state50, 97), edge(state50, state51, 97), edge(state50, state51, 98), edge(state51, state52, 98), edge(state52, accept, 98)])::match([98, 98], [state51]) ;
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state53, 97), edge(state53, state54, 97), edge(state54, state55, 97), edge(state54, state55, 98), edge(state55, state56, 98), edge(state56, accept, 98)])::match([98], [state56]) ;
X = regexp([97, 97, 40, 97, 124, 98, 41|...], [edge(start, state57, 97), edge(state57, state58, 97), edge(state58, state59, 97), edge(state58, state59, 98), edge(state59, state60, 98), edge(state60, accept, 98)])::match([], [accept]) ;
X = true.
```
