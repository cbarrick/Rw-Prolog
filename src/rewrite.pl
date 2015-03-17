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

:- op(990, xfx, (:=)).
:- multifile (:=)/2.


%! rewrite(?A, ?B) is nondet
% There exists a rewrite path from A to B.
%
% @arg A is the start term.
% @arg B is the rewritten term.

% :- table rewrite/2.

rewrite(Source, Dest) :- distinct(rewrite_(Source, Dest)).


rewrite_(Source, Dest) :- rewrite_(Source, Dest, [Source]).

rewrite_(Source, Dest, [H|T]) :-
	empty_nb_set(NewTermsSet),
	(
		rewrite_shallowest(H, Dest, _),
		add_nb_set(Dest, NewTermsSet, true)
	;
		nb_set_to_list(NewTermsSet, NewTerms),
		append(T, NewTerms, NextQueue),
		!,
		rewrite_(Source, Dest, NextQueue)
	).


rewrite_shallowest(Source, Dest, Depth) :-
	term_depth(Source, MaxDepth),
	rewrite_shallowest(0, MaxDepth, Source, Dest, Depth).

rewrite_shallowest(CurrentDepth, MaxDepth, Source, Dest, Depth) :-
	(
		Depth = CurrentDepth,
		rewrite_depth(CurrentDepth, Source, Dest)
	*->
		true
	;
		CurrentDepth < MaxDepth,
		NextDepth is CurrentDepth + 1,
		rewrite_shallowest(NextDepth, MaxDepth, Source, Dest, Depth)
	).


rewrite_depth(0, Source, Dest) :-
	nonvar(Source),
	once(clause((Source := Dest), _)),
	call_rw((Source := Dest)).

rewrite_depth(N, Source, Dest) :-
	compound(Source),
	N > 0,
	N0 is N - 1,
	Source =.. [Functor|SourceArgs],
	same_length(SourceArgs, DestArgs),
	Dest =.. [Functor|DestArgs],
	select(Arg, SourceArgs, RwArg, DestArgs),
	rewrite_depth(N0, Arg, RwArg).



%! call_rw(:Goal) is nondet
% Call Goal. If the query does not succeed, rewrite Goal and try again.
%
% @arg Goal is the query to be called.

:- meta_predicate call_rw(:).

call_rw(Goal) :-
	catch((
		call_rw_(Goal)
	), cut, (
		fail
	)).

call_rw_(M:(A,B)) :- !,
	call_rw_(M:A),
	call_rw_(M:B).

call_rw_(M:(A;B)) :- !,
	(
		call_rw_(M:A)
	;
		call_rw_(M:B)
	).

call_rw_(_M:(!)) :- !,
	(
		true
	;
		throw(cut)
	).

call_rw_(Goal) :-
	(
		call(Goal)
	*->
		true
	;
		rewrite(Goal, RwGoal),
		call_rw_(RwGoal)
	).
