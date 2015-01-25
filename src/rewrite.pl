%! <module> Predicates for searching rewrite graphs
%
% This module provides basic utilities for examining conditional rewrite graphs.
% Rewrite graphs are encoded as Prolog rules of the form
%	`OldTerm := NewTerm :- Conditions`.
%
% @author Chris Barrick
% @license MIT

:- module(rewrite, [
	rewrite/2,
	rewrite/3,
	simplify/2,
	op(990, xfx, (:=))
]).


%! rewrite(?A, ?B) is nondet
% There exists a single rule allowing term A to be rewritten as term B.
%
% @arg A is the source term.
% @arg B is the destination term.

% Variables and numbers cannot be rewritten
rewrite(A, _) :- var(A), !, fail.
rewrite(A, _) :- number(A), !, fail.

rewrite(A, B) :- rewrite_top(A, B).
rewrite(A, B) :- rewrite_args(A, B).


%! rewrite_top(?A, ?B) is nondet
% Rewrites the top-level functor in term A into term B.

% Apply rewriting to the principal functor
rewrite_top(A, B) :-
	% We cannot use rules that require a term to be narrower (i.e. more
	% instantiated). To prevent narrowing, we search for rules using a copy of
	% the term. Unusable rules will narrow the copy. However, if the copy
	% subsumes the original, then the rule does not require a more narrow term.
	copy_term(A, Copy),
	clause(Copy := B, Body),
	subsumes_term(Copy, A),
	A = Copy,

	% If the rule has conditions, we must check that they are met. The body of
	% the rule is scoped to the module in which it was defined. Thus we must
	% tell `call/1` to evaluate its argument relative to the proper module.
	once((
		predicate_property(A := B, imported_from(Module)),
		call(Module:Body)
	;
		call(Body)
	)).


%! rewrite_args(?A, ?B) is nondet
% Rewrite an argument in term A. B is the rewritten form of A.

% Apply rewriting to arguments
rewrite_args(A, B) :-
	A =.. [Functor|OriginalArgs],
	select(Arg, OriginalArgs, NewArg, NewArgs),
	rewrite(Arg, NewArg),
	B =.. [Functor|NewArgs],
	A \== B.


%! rewrite(+N:integer, ?A, ?B) is nondet
% Rewrite term A, N times. B is the rewritten term.

rewrite(0, A, A) :- !.
rewrite(1, A, B) :- !, rewrite(A, B).
rewrite(N, A, B) :-
	N0 is N - 1,
	rewrite(A, X),
	rewrite(N0, X, B).


%! simplify(?Term, ?Normal) is det
% Finds the normal form of Term. The normal form is defined as the first form
% found in a depth-first search of the rewrite graph such that it cannot be
% rewritten into a previously unseen term.
%
% @arg Term is the initial term.
% @arg Normal is the normal form of Term.

simplify(Term, Term) :- var(Term), !.
simplify(Term, Term) :- number(Term), !.
simplify(Term, Normal) :- simplify_(Term, Normal, [Term]).

simplify_(Term, Normal, Seen) :-
	rewrite(Term, Next),
	\+ (
		member(Previous, Seen),
		Next == Previous
	),
	!,
	simplify_(Next, Normal, [Next|Seen]).
simplify_(Normal, Normal, _).



%! unify_rw(?A, ?B, ?UnifyingTerm)
% TODO: Document

unify_rw(A, A, A) :- !.
unify_rw(A, B, UnifyingTerm) :-
	MaxDepth = 256,
	between(1, MaxDepth, N),
	rewrite(N, A=B, UnifyingTerm=UnifyingTerm),
	!.
