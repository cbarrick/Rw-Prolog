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
	simplify/2,
	simplify_compound/2,
	simplify_args/2
]).


%! rewrite(?A, ?B) is nondet
% There exists a rule allowing term A to be rewritten as term B.
%
% @arg A is the source term.
% @arg B is the destination term.

% Variables and numbers cannot be rewritten
rewrite(A, _) :- var(A), !, fail.
rewrite(A, _) :- number(A), !, fail.

% Apply rewriting to the principal functor
rewrite(A, B) :-
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

% Apply rewriting to arguments
rewrite(A, B) :-
	A =.. [Functor|OriginalArgs],
	rewrite_args_(OriginalArgs, NewArgs),
	B =.. [Functor|NewArgs],
	A \== B.


% Given a list of terms, possibly rewrite any number of them.
rewrite_args_([], []).
rewrite_args_([X|Xs], [X|Ys]) :- rewrite_args_(Xs, Ys).
rewrite_args_([X|Xs], [Y|Ys]) :- rewrite(X, Y), rewrite_args_(Xs, Ys).


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


unify_rw_(A, _, A, _, SeenB) :- member(A, SeenB).

unify_rw_(A, B, Target, SeenA, SeenB) :-
	rewrite(A, Next),
	\+ (
		member(Previous, SeenA),
		Next == Previous
	),
	duplicate_term(SeenA, SeenAOriginal),
	nb_setarg(1, SeenA, Next),
	nb_setarg(2, SeenA, SeenAOriginal),
	!,
	unify_rw_(B, Next, Target, SeenB, SeenA).

unify_rw_(A, B, Target, SeenA, SeenB) :- unify_rw_(B, A, Target, SeenB, SeenA).
