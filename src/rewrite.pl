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

rewrite(Source, Dest) :- distinct(rewrite_(Source, Dest)).


rewrite_(Source, Dest) :- rewrite_(Source, Dest, [Source]).

rewrite_(Source, Dest, [H|T]) :-
	empty_nb_set(SuccessfulRules),
	(
		rewrite_shallowest(H, Dest, _),
		add_nb_set((H:=Dest), SuccessfulRules, true)
	;
		nb_set_to_list(SuccessfulRules, EdgeList),
		rewrite_collectdests(H, EdgeList, DestList),
		append(T, DestList, NextQueue),
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
	clause((Source := Dest), Body),
	call_rw(Body).

rewrite_depth(N, Source, Dest) :-
	compound(Source),
	N > 0,
	N0 is N - 1,
	Source =.. [Functor|SourceArgs],
	same_length(SourceArgs, DestArgs),
	Dest =.. [Functor|DestArgs],
	select(Arg, SourceArgs, RwArg, DestArgs),
	rewrite_depth(N0, Arg, RwArg).


rewrite_collectdests(_, [], []) :- !.
rewrite_collectdests(Source, [(Source:=Dest)|EdgeList], [Dest|DestList]) :-
	rewrite_collectdests(Source, EdgeList, DestList).



%! call_rw(:Goal) is nondet
% Call Goal. If the query does not succeed, rewrite Goal and try again.
%
% @arg Goal is the query to be called.

:- meta_predicate call_rw(:).

call_rw(Goal) :-
	catch((
		call_rw_(Goal)
	), cut(Resume, Goal), (
		call_rw(Resume)
	)).

call_rw_(M:(A,B)) :- !,
	catch((
		call_rw_(M:A)
	), cut(Resume, (M:A)), (
		throw( cut((Resume,(M:B)), M:(A,B)) )
	)),
	catch((
		call_rw_(M:B)
	), cut(Resume, (M:B)), (
		throw( cut(Resume, M:(A,B)) )
	)).

call_rw_(M:(A;B)) :- !,
	(
		call_rw_(M:A)
	;
		call_rw_(M:B)
	).

call_rw_(M:(!)) :- !,
	throw( cut(true, M:(!)) ).

call_rw_(Goal) :-
	(
		call(Goal)
	*->
		true
	;
		rewrite(Goal, RwGoal),
		call_rw_(RwGoal)
	).
