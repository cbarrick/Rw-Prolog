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
	rewrite_n/3,
	simplify/2,
	simplify_compound/2,
	simplify_args/2,
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


%! rewrite_n(+N:integer, ?A, ?B) is nondet
% Rewrite term A, N times. B is the rewritten term.

rewrite_n(0, A, A) :- !.
rewrite_n(1, A, B) :- !, rewrite(A, B).
rewrite_n(N, A, B) :-
	N0 is N - 1,
	rewrite(A, X),
	rewrite_n(N0, X, B).


%! simplify(?Term, ?Normal) is det
% Searches the graph of rewrite rules for the normal form of a term. If the
% graph contains multiple normal forms, only the first is found. For compound
% terms, the arguments are normalized before the term itself.
%
% @arg Term is the initial term.
% @arg Normal is the normal form of Term.

simplify(Term, Term) :- var(Term), !.
simplify(Term, Term) :- number(Term), !.
simplify(Term, Normal) :- simplify_compound(Term, Normal).


%! simplify_compound(?Term:compound, ?Normal) is det
% Like `simplify/2` with the constraint that Term must be a compound.
%
% @arg Term is the initial term.
% @arg Normal is the normal form of Term.

simplify_compound(Term, Normal) :-
	simplify_compound(Term, Normal, [Term]).

simplify_compound(Normal, Normal, _) :-
	\+ rewrite(Normal, _),
	!.

simplify_compound(Term, Normal, Seen) :-
	rewrite(Term, Next),
	\+ (
		member(Previous, Seen),
		Next == Previous
	),
	!,
	simplify_compound(Next, Normal, [Next|Seen]).


%! simplify_args(?Term:compound, ?NormalArgs:compound) is det
% Like `simplify_compound/2` but only simplifies the arguments of the term.

simplify_args(Term, NormalArgs) :-
	compound(Term),
	Term =.. [Functor|Args1],
	same_length(Args1, Args2),
	maplist(simplify, Args1, Args2),
	NormalArgs =.. [Functor|Args2],
	!.


%! unify_rw(?A, ?B, ?UnifyingTerm)
% TODO: Document

unify_rw(A, A, A) :- !.
unify_rw(A, B, Target) :- unify_rw_(A, B, Target, [A], [B]), !.

% We introduce two new state parameters, `SeenA` and `SeenB`, that maintain
% a cache of the forms of `A` and `B` already seen in the search. The caches
% are updated with `nb_setarg/2`, allowing the entries to persist across
% backtracking.
% TODO: Optimize caches. Currently we are using lists, but (lack of) membership
% is the only property we care about. I bet there is some kind of set data
% structure that would allow us to check for non-membership efficiently.

unify_rw_(A, _, A, _, SeenB) :- member(A, SeenB).

unify_rw_(A, B, Target, SeenA, SeenB) :-
	rewrite(A, Next),
	\+ (
		member(Previous, SeenA),
		Next == Previous
	),

	% Update the caches. This operation is extra-logical; the caches are
	% updated in place and are persistent across backtracking.
	duplicate_term(SeenA, SeenAOriginal),
	nb_setarg(1, SeenA, Next),
	nb_setarg(2, SeenA, SeenAOriginal),

	unify_rw_(B, Next, Target, SeenB, SeenA).

unify_rw_(A, B, Target, SeenA, SeenB) :- unify_rw_(B, A, Target, SeenB, SeenA).
