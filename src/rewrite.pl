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
	unify_rw/3,
	call_rw/1,
	op(990, xfx, (:=))
]).


%! rewrite(?A, ?B) is nondet
% There exists a rewrite path from A to B.
%
% @arg A is the start term.
% @arg B is the rewritten term.

% Variables and numbers cannot be rewritten
rewrite(A, _) :- var(A), !, fail.
rewrite(A, _) :- number(A), !, fail.

% Search for rewrite path of length at most 1024
rewrite(A, B) :-
	rewrite(_, A, B).


%! rewrite(+N:integer, ?A, ?B) is nondet
% There exists a rewrite path of length N from A to B.
%
% @arg N is the number of times the term is rewritten.
% @arg A is the start term.
% @arg B is the rewritten term.

rewrite(1, A, B) :- rewrite_top(A, B).
rewrite(1, A, B) :- rewrite_args(A, B).
rewrite(N, A, B) :-
	between(2, 1024, N),
	flag(rewrite_count(A), _, 0),
	N0 is N - 1,
	(
		rewrite(1, A, X),
		rewrite(N0, X, B),
		flag(rewrite_count(A), Count, Count+1)
	;
		flag(rewrite_count(A), Count, Count),
		Count = 0,
		!, fail
	).


%! rewrite_top(?A, ?B) is nondet
% Rewrites the top-level functor in term A into term B.
%
% @arg A is the start term.
% @arg B is the rewritten term.

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
%
% @arg A is the start term.
% @arg B is the rewritten term.

rewrite_args(A, B) :-
	compound(A),
	A =.. [Functor|OriginalArgs],
	select(Arg, OriginalArgs, NewArg, NewArgs),
	rewrite(1, Arg, NewArg),
	B =.. [Functor|NewArgs],
	A \== B.


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
	rewrite(1, Term, Next),
	\+ (
		member(Previous, Seen),
		Next == Previous
	),
	!,
	simplify_(Next, Normal, [Next|Seen]).
simplify_(Normal, Normal, _).



%! unify_rw(?A, ?B, ?UnifyingTerm) is det
% Find a term from each of the rewrite graphs of A and B that unify.
%
% @arg A is the term to unify with B.
% @arg B is the term to unify with A.
% @arg UnifyingTerm is the form to which A and B are rewritten and unifed.

unify_rw(A, A, A) :- !.
unify_rw(A, B, UnifyingTerm) :-
	rewrite(A=B, UnifyingTerm=UnifyingTerm),
	!.


%! call_rw(:Goal) is nondet
% Call Goal. If the query does not succeed, rewrite Goal and try again.
%
% @arg Goal is the query to be called.

:- meta_predicate call_rw(:).

call_rw(Goal) :-
	rewrite(Goal, RWGoal),
	catch((
		call(RWGoal)
	), error(existence_error(procedure,_),_), (
		fail
	)).
