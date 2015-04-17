%! <module> Predicates for searching rewrite graphs
%
% This module provides basic utilities for examining conditional rewrite graphs.
% Rewrite graphs are encoded as Prolog rules of the form
%	`Pattern := Template :- Conditions`.
%
% @author Chris Barrick
% @license GPLv3

% :- module(rewrite, [
% 	redex/3,    % redex(@Redex, ?Replacement, ?Rule)
% 	reduce/2,   % reduce(@Source, ?Dest)
% 	reduce/4,   % reduce(@Source, ?Dest, ?Rule, ?Position)
% 	simplify/2, % simplify(@Term, ?Simple)
% 	call_rw/1,  % call_rw(:Goal)
% 	call_rw/2,  % call_rw(:Goal, :Result)
% 	op(990, xfx, :=)
% ]).

:- expects_dialect(swi).

:- use_module(library(nb_set)).

:- use_module('./util.pl').

:- dynamic( (:=)/2 ).
:- multifile( (:=)/2 ).
:- public( (:=)/2 ).


%! redex(@Redex, ?Replacement, ?Rule) is nondet
%
% Redex matches the pattern of the rewrite Rule, and Replacement is the
% result of contracting Redex.

:- public redex/3.
:- meta_predicate redex(:,:,?).

redex(M:Redex, M:Replacement, (Pattern:=Template:-Condition)) :-
	% Use compy_term_nat to remove attributes of attributed variables.
	% This prevents attribute duplication, a form of memory leak.
	copy_term_nat(Redex, Redex_nat),
	copy_term_nat(Replacement, Replacement_nat),
	copy_term(Redex_nat, Pattern),
	copy_term(Replacement_nat, Template),
	M:clause(Pattern:=Template, Condition),
	subsumes_term(Pattern, Redex_nat),
	call_rw(M:Condition, _),
	subsumes_term(Pattern, Redex_nat),
	Pattern = Redex_nat,
	Redex = Redex_nat,
	Template = Replacement_nat,
	Replacement = Replacement_nat.


%! reduce(@Source, ?Dest) is nondet
%! reduce(@Source, ?Dest, ?Rule, ?Position) is nondet
%
% The term Source can be reduced to the term Dest by applying a rewrite Rule
% at a specific Position.
%
% Redexes in Source are candidates for contraction iff they are not proper
% subterms of another redex in Source.

:- public reduce/2, reduce/4.
:- meta_predicate reduce(:,:).
:- meta_predicate reduce(:,:,?,?).

reduce(M:Source, M:Dest) :- reduce(M:Source, M:Dest, _, _).

reduce(M:Source, M:Dest, return(X):=X:-true, []) :- Source == return(Dest), !.

reduce(M:Source, M:Dest, Rule, Position) :-
	nonvar(Source),
	(
		distinct(redex(M:Source, M:Dest, Rule)),
		Position = []
	*->true;
		Position = [H|T],
		Source =.. [Functor|Args],
		length(Args, L),
		length(DestArgs, L),
		Dest =.. [Functor|DestArgs],
		between(1,L,H),
		nth1(H, Args, NextSource, Same),
		nth1(H, DestArgs, NextDest, Same),
		reduce(M:NextSource, M:NextDest, Rule, T)
	).


%! simplify(@Term, ?Simple) is nondet
%
% Simple is a terminal form reduced from Term. Determining whether a term is a
% terminal form is undecidable in general. Thus the Simple is called before
% returning from this predicate. Term may have more than one terminal form.

:- public simplify/2.
:- meta_predicate simplify(:,:).

simplify(M:call(X), M:call(X)) :- !, M:call(X).

simplify(M:Term, M:Simple) :- simplify_terminal(M:Term, M:Simple).
simplify(M:Term, M:Simple) :- simplify(M:Term, M:Simple, [Term]).

simplify(M:Term, M:Simple, [H|T]) :-
	empty_nb_set(NewForms),
	(
		reduce(M:H, M:Next),
		add_nb_set((Term:=Next), NewForms, true),
		simplify_terminal(M:Next, M:Simple)
	;
		simplify_set_to_list(NewForms, NewFormsList, Term),
		append(T, NewFormsList, NewQueue),
		simplify(M:Term, M:Simple, NewQueue)
	).

simplify_terminal(M:return(X), M:X) :- !.
simplify_terminal(M:Simple, M:Simple) :-
	functor(Simple, Functor, Arity),
	M:current_predicate(Functor/Arity),
	catch((
		M:clause(Simple, Body)
	*->
		(
			M:predicate_property(Simple, imported_from(BodyModule))
		*->true;
			BodyModule = M
		),
		call_rw(BodyModule:Body)
	;
		M:call(Simple)
	), error(permission_error(access,_,_),_), (
		M:call(Simple)
	)).

simplify_set_to_list(nb_set(S), List, Witness) :-
	nb_set_to_list(nb_set(S), L),
	simplify_set_to_list(L, List, Witness).

simplify_set_to_list([Witness:=Term|L], [Term|List], Witness) :-
	simplify_set_to_list(L, List, Witness).

simplify_set_to_list([], [], _Witness).



%! call_rw(:Goal) is nondet
%! call_rw(:Goal, :Result) is nondet
%
% call_rw/2 is like simplify/2 except control predicates are special cased so
% that they cannot be overridden by rewrite rules and are evaluated as in
% normal Prolog.
%
% @arg Goal is the query to be called.
% @arg Result is the terminal from of the goal.

:- public call_rw/1, call_rw/2.
:- meta_predicate call_rw(:).
:- meta_predicate call_rw(:,:).

call_rw(M:Goal) :- call_rw(M:Goal, M:_).

call_rw(M:Goal, M:Result) :- call_rw(Goal, M:Goal, M:Result).

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

call_rw_(Witness, M:(!), M:(!)) :- !,
	throw( cut(Witness, true, (!)) ).


% Short-circuit built-ins
% -----
% We can handle certain built-in predicates automatically without delegating
% to the rewrite system.

call_rw_(_, M:true, M:true)   :- !, true.
call_rw_(_, M:false, M:false) :- !, false.
call_rw_(_, M:fail, M:fail)   :- !, fail.


% Control Predicates
% -----
% Control predicates must consider how cuts affect their behavior. In most
% cases, the implementation of the control predicate should catch cuts thrown
% by the individual sub-queries and throw a new cut describing how the overall
% query is affected.
%
% TODO: soft-cuts (`*->`)

call_rw_(Witness, M:(A->B), M:Result) :- !,
	call_rw_(Witness, M:(A->B;fail), M:Result).

call_rw_(Witness, M:(A->B;C), M:(X->Y;Z)) :- !,
	(
		call_rw(M:A, M:X),
		!,
		Z=C,
		call_rw_(Witness, M:B, M:Y)
	;
		A=X,
		B=Y,
		call_rw_(Witness, M:C, M:Z)
	).

call_rw_(Witness, M:(A,B), M:(X,Y)) :- !,
	catch((
		call_rw_(Witness, M:A, M:X)
	), cut(Witness, Resume, X), (
		throw( cut(Witness, M:(Resume,call_rw(B,Y)), M:(X,Y)) )
	)),
	call_rw_(Witness, M:B, M:Y).

call_rw_(Witness, M:(A;B), M:(X;Y)) :- !,
	(
		Y = B,
		call_rw_(Witness, M:A, M:X)
	;
		X = A,
		call_rw_(Witness, M:B)
	).

call_rw_(Witness, M:catch(Goal,Ball,Recover), M:catch(X,Ball,Y)) :- !,
	catch((
		Y = Recover,
		call_rw(Witness, M:Goal, M:X)
	), Ball, (
		X = Goal,
		call_rw(Witness, M:Recover, M:Y)
	)).


% % Type-checking predicates
% % -----
% % In this system, it is most useful for certain type-checking predicates to
% % be delayed until the term they are applied to is bound.
%
% call_rw_(_, M:number(X), M:number(X)) :- !, M:freeze(X, number(X)).
% call_rw_(_, M:list(X),   M:list(X))   :- !, M:freeze(X, member(X,[[],[_|_]])).


% Regular queries
% -----
% Regular queries are the non-control, non-builtin predicates. These are the
% kinds of queries that get rewritten if they fail.

call_rw_(_, M:Goal, M:Result) :- simplify(M:Goal, M:Result).
