%! <module> Predicates for searching rewrite graphs
%
% This module provides basic utilities for examining conditional rewrite graphs.
% Rewrite graphs are encoded as Prolog rules of the form
%	`OldTerm := NewTerm :- Conditions`.
%
% @author Chris Barrick
% @license GPLv3

:- expects_dialect(swi).

:- use_module('util').

:- op(990, xfx, (:=)).
:- multifile (:=)/2.


%! rewrite(?A, ?B) is nondet
% There exists a rewrite path from A to B.
%
% @arg A is the start term.
% @arg B is the rewritten term.

% :- table rewrite/2.

rewrite(X, X).
rewrite(A, B) :- rewrite_top(A, X),  rewrite(X, B).
rewrite(A, B) :- rewrite_args(A, X), rewrite(X, B).


rewrite_top(A, B) :-
	copy_term(A, A_Varient),
	clause(A_Varient := B, Body),
	A =@= A_Varient,
	A = A_Varient,
	call_rw(Body).


rewrite_args(A, B) :-
	compound(A),
	A =.. [Functor|OriginalArgs],
	maplist(rewrite, OriginalArgs, NewArgs),
	OriginalArgs \= NewArgs,
	B =.. [Functor|NewArgs].


%! call_rw(:Goal) is nondet
% Call Goal. If the query does not succeed, rewrite Goal and try again.
%
% @arg Goal is the query to be called.

:- meta_predicate call_rw(:).

call_rw(M:(A,B)) :- !, call_rw(M:A), call_rw(M:B).
call_rw(M:(A;B)) :- !, call_rw(M:A); call_rw(M:B).

call_rw(M:Goal) :-
	rewrite(Goal, RwGoal),
	call(M:RwGoal).
