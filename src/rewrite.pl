%! <module> Predicates for searching rewrite graphs
%
% This module provides basic utilities for examining conditional rewrite graphs.
% Rewrite graphs are encoded as Prolog rules of the form
%	`OldTerm := NewTerm :- Conditions`.
%
% @author Chris Barrick
% @license GPLv3

:- expects_dialect(swi).

:- use_module(library(nb_set)).

:- use_module('util').

:- dynamic (:=)/2.
:- multifile (:=)/2.
:- public (:=)/2.


%! redex(@Redex, ?Replacement, ?Rule)
%
% TODO: Document

redex(Redex, Replacement, (Pattern:=Template:-Condition)) :-
	copy_term_nat(Redex, Redex_nat),
	copy_term_nat(Replacement, Replacement_nat),
	copy_term(Redex_nat, Pattern),
	copy_term(Replacement_nat, Template),
	clause(Pattern:=Template, Condition),
	subsumes_term(Pattern, Redex_nat),
	call_rw(Condition, _),
	subsumes_term(Pattern, Redex_nat),
	Pattern = Redex_nat,
	Redex = Redex_nat,
	Template = Replacement_nat,
	Replacement = Replacement_nat.


%! reduce(@Source, ?Dest)
%! reduce(@Source, ?Dest, ?Rule, ?Position)
%
% TODO: Document

reduce(Source, Dest) :- reduce(Source, Dest, _, _).

reduce(return(X), X, return(X):=X, []) :- !.

reduce(Source, Dest, Rule, Position) :-
	nonvar(Source),
	(
		distinct(redex(Source, Dest, Rule)),
		Position = []
	*->true;
		Position = [H|T],
		Source =.. [Functor|Args],
		length(Args, L),
		length(DestArgs, L),
		Dest =.. [Functor|DestArgs],
		between(1,L,H),
		H0 is H-1,
		nth0(H0, Args, NextSource, Same),
		nth0(H0, DestArgs, NextDest, Same),
		reduce(NextSource, NextDest, Rule, T)
	).


%! simplify(@Term, ?Simple)
%
% TODO: Document

simplify(Term, Simple) :- simplify_terminal(Term, Simple).
simplify(Term, Simple) :- simplify(Term, Simple, [Term]).

simplify(Term, Simple, [H|T]) :-
	empty_nb_set(NewForms),
	(
		reduce(H, Next),
		add_nb_set((Term:=Next), NewForms, true),
		simplify_terminal(Next, Simple)
	;
		simplify_set_to_list(NewForms, NewFormsList, Term),
		append(T, NewFormsList, NewQueue),
		simplify(Term, Simple, NewQueue)
	).

simplify_terminal(return(X), X) :- !.
simplify_terminal(Simple, Simple) :-
	predicate_property(Simple, visible),
	catch((
		clause(Simple, Body)
	*->
		call_rw(Body, _)
	;
		call(Simple)
	), error(permission_error(access,_,_),_), (
		call(Simple)
	)).

simplify_set_to_list(nb_set(S), List, Witness) :-
	nb_set_to_list(nb_set(S), L),
	simplify_set_to_list(L, List, Witness).

simplify_set_to_list([Witness:=Term|L], [Term|List], Witness) :-
	simplify_set_to_list(L, List, Witness).

simplify_set_to_list([], [], _Witness).



%! call_rw(:Goal) is nondet
%
% Call Goal. If the query does not succeed, rewrite Goal and try again. If Goal
% is a compound query (i.e. contains control predicates), the rewrite engine
% is only applied to the failing component.
%
% @arg Goal is the query to be called.

:- meta_predicate call_rw(:).
:- meta_predicate call_rw(:,-).
:- meta_predicate call_rw(?,:,-).
:- meta_predicate call_rw_(?,:,-).
:- meta_predicate call_rw_expand(?,:,-).

call_rw(Goal) :- call_rw(Goal, _).

call_rw(Goal, Result) :- call_rw(Goal, Goal, Result).

call_rw(Witness, Goal, Result) :-
	catch((
		call_rw_(Witness, Goal, Result)
	), cut(Witness, Resume, Result), (
		call_rw(Witness, Resume, Result)
	)).


% Cuts
% -----
% Cuts are handled by throwing a term of the form `cut(Continuation, Query)`
% where Query is the body of the query in which the cut occurs and Continuation
% is a query describing to the top-level how to recover, i.e. what remains of
% the original goal after the cut is applied.

call_rw_(Witness, _M:(!), (!)) :- !,
	throw( cut(Witness, true, (!)) ).


% Control Predicates
% -----
% Control predicates must consider how cuts affect their behavior. In most
% cases, the implementation of the control predicate should catch cuts thrown
% by the individual sub-queries and throw a new cut describing how the overall
% query is affected.
%
% TODO: soft-cuts (`*->`)

call_rw_(Witness, M:(A->B), Result) :- !,
	call_rw_(Witness, M:(A->B;fail), Result).

call_rw_(Witness, M:(A->B;C), (X->Y;Z)) :- !,
	(
		call_rw(M:A, X),
		!,
		Z=C,
		call_rw_(Witness, M:B, Y)
	;
		A=X,
		B=Y,
		call_rw_(Witness, M:C, Z)
	).

call_rw_(Witness, M:(A,B), (X,Y)) :- !,
	catch((
		call_rw_(Witness, M:A, X)
	), cut(Witness, Resume, X), (
		throw( cut(Witness, (Resume,call_rw(M:B,Y)), (X,Y)) )
	)),
	call_rw_(Witness, M:B, Y).

call_rw_(Witness, M:(A;B), (X;Y)) :- !,
	(
		Y = B,
		call_rw_(Witness, M:A, X)
	;
		X = A,
		call_rw_(Witness, M:B)
	).

call_rw_(Witness, M:catch(Goal,Ball,Recover), catch(X,Ball,Y)) :- !,
	catch((
		Y = Recover,
		call_rw(Witness, M:Goal, X)
	), Ball, (
		X = Goal,
		call_rw(Witness, M:Recover, Y)
	)).


% Type-checking predicates
% -----
% In this system, it is most useful for certain type-checking predicates to
% be delayed until the term they are applied to is bound.

call_rw_(_, M:number(X), number(X)) :- !, freeze(X, M:number(X)).
call_rw_(_, M:list(X),   list(X))   :- !, freeze(X, M:member(X,[[],[_|_]])).


% Regular queries
% -----
% Regular queries are the non-control, non-builtin predicates. These are the
% kinds of queries that get rewritten if they fail.

call_rw_(_, _:Goal, Result) :- simplify(Goal, Result).
