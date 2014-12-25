%! <module> Predicates for searching rewrite graphs
%
% This module provides basic utilities for examining conditional rewrite graphs.
% Rewrite graphs are encoded as Prolog rules of the form
%	`OldTerm := NewTerm :- Conditions`.
%
% @author Chris Barrick
% @license MIT

:- module(simplify, [
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
	predicate_property(A := B, imported_from(Module)),
	call(Module:Body).


%! simplify(?Term, ?Normal) is det
% Searches the graph of rewrite rules for the normal form of a term. If the
% graph contains multiple normal forms, only the first is found. For compound
% terms, the arguments are normalized before the term itself.
%
% @arg Term is the initial term.
% @arg Normal is the normal form of Term.

simplify(Term, Term) :- var(Term), !.
simplify(Term, Term) :- number(Term), !.
simplify(Term, Term) :- atom(Term), !.
simplify(Term, Term) :- string(Term), !.
simplify(Term, Normal) :- simplify_compound(Term, Normal).


%! simplify_compound(?Term:compound, ?Normal:compound) is det
% Like `simplify/2` with the constraint that Term must be a compound.
%
% @arg Term is the initial term.
% @arg Normal is the normal form of Term.

simplify_compound(Term, Normal) :-
	simplify_args(Term, NormalArgs),
	simplify_compound(NormalArgs, Normal, [Term]).


%! simplify_compound(?Term:compound, ?Normal:compound, +Seen:list) is det
% A helper predicate for `simplify_compound/2`. Here we assume that the
% arguments of Term are already normal.
%
% @arg Term is the initial term.
% @arg Normal is the normal form of Term.
% @arg Seen is the list of terms already encountered in the graph.

simplify_compound(Term, Normal, Seen) :-
	rewrite(Term, Next),
	\+ (
		member(Previous, Seen),
		Next == Previous
	),
	!,
	simplify_args(Next, NextNormalArgs),
	simplify_compound(NextNormalArgs, Normal, [Next|Seen]).

simplify_compound(Normal, Normal, _).


%! simplify_args(?Term:compound, ?NormalArgs:compound) is det
% Like `simplify_compound/2` but only simplifies the arguments of the term.

simplify_args(Term, NormalArgs) :-
	compound(Term),
	Term =.. [Functor|Args1],
	same_length(Args1, Args2),
	maplist(simplify, Args1, Args2),
	NormalArgs =.. [Functor|Args2],
	!.
