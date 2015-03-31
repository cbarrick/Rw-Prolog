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
:- op(990, xfx, (:=>)).
:- multifile (:=)/2.


%! rewrite(Source, ?Destination) is nondet
%
% There exists a rewrite path from Source to Destination.
%
% @arg Source is the term from which to start the search.
% @arg Destination is the result of the search.

rewrite(X,X).
rewrite(Source, Dest) :- distinct(rewrite_(Source, Dest)).

rewrite_(Source, Dest) :-
	copy_term_nat(Source, S),
	copy_term_nat(Dest, D),
	rewrite_(S, D, [S]),
	Source=S,
	Dest=D.

rewrite_(Source, Dest, [H|T]) :-
	empty_nb_set(SuccessfulRules),
	(
		rewrite_once(H, Dest, _),
		add_nb_set((H:=Dest), SuccessfulRules)
	;
		nb_set_to_list(SuccessfulRules, EdgeList),
		rewrite_collectdests(H, EdgeList, DestList),
		append(T, DestList, NextQueue),
		!,
		rewrite_(Source, Dest, NextQueue)
	).

rewrite_collectdests(_, [], []) :- !.
rewrite_collectdests(Source, [(Source:=Dest)|EdgeList], [Dest|DestList]) :-
	rewrite_collectdests(Source, EdgeList, DestList).


%! rewrite_once(?Source, ?Destination, -Depth) is nondet
%
% The Source term can be rewritten into the Destination term by applying a
% rewrite rule exactly once at the shallowest possible depth of the Source
% term's syntax tree. If multiple rewrites are possible at the same depth,
% each rewrite is chosen nondeterministicly.
%
% @arg Source is the term from which to start the search.
% @arg Destination is the result of the search.
% @arg Depth is the depth of the Source term at which the rewrite was applied.

rewrite_once(Source, Dest, Depth) :-
	term_depth(Source, MaxDepth),
	rewrite_once(0, MaxDepth, Source, Dest, Depth).

rewrite_once(CurrentDepth, MaxDepth, Source, Dest, Depth) :-
	(
		Depth = CurrentDepth,
		rewrite_depth(Source, Dest, Depth)
	*->true;
		CurrentDepth < MaxDepth,
		NextDepth is CurrentDepth + 1,
		rewrite_once(NextDepth, MaxDepth, Source, Dest, Depth)
	).


%! rewrite_depth(?Source, ?Destination, ?Depth) is nondet
%
% The Source term can be rewritten into the Destination term by applying
% exactly one rewrite rule at the particular Depth of the Source term's syntax
% tree.
%
% @arg Source is the term from which to start the search.
% @arg Destination is the result of the search.
% @arg Depth is the depth of the Source term at which the rewrite was applied.

rewrite_depth(Source, Dest, 0) :- !,
	nonvar(Source),
	copy_term(Source,S),
	catch((
		clause(S:=Dest, Body),
		call_rw_(S:=Dest, Body)
	), cut(S:=Dest, Resume), (
		call_rw(S:=Dest, Resume)
	)),
	subsumes_term(S,Source),
	S=Source.

rewrite_depth(Source, Dest, Depth) :-
	compound(Source),
	between(1, 256, Depth),
	Depth0 is Depth - 1,
	Source =.. [Functor|SourceArgs],
	same_length(SourceArgs, DestArgs),
	Dest =.. [Functor|DestArgs],
	select(Arg, SourceArgs, RwArg, DestArgs),
	rewrite_depth(Arg, RwArg, Depth0).


%! simplify(?Term, ?Normal) is det
%
% TODO: Document

simplify(Term, Norm) :-
	rewrite_once(Term, Next, _),
	!,
	simplify(Next, Norm).

simplify(Norm, Norm).


%! call_rw(:Goal) is nondet
%
% Call Goal. If the query does not succeed, rewrite Goal and try again. If Goal
% is a compound query (i.e. contains control predicates), the rewrite engine
% is only applied to the failing component.
%
% @arg Goal is the query to be called.

:- meta_predicate call_rw(:).
:- meta_predicate call_rw(?,:).
:- meta_predicate call_rw_(?,:).
:- meta_predicate call_rw_expand(?,:).

call_rw(Goal) :- call_rw(Goal, Goal).

call_rw(Witness, Goal) :-
	catch((
		call_rw_(Witness, Goal)
	), cut(Witness, Resume), (
		call_rw(Witness, Resume)
	)).


% Cuts
% -----
% Cuts are handled by throwing a term of the form `cut(Continuation, Query)`
% where Query is the body of the query in which the cut occurs and Continuation
% is a query describing to the top-level how to recover, i.e. what remains of
% the original goal after the cut is applied.

call_rw_(Witness, _M:(!)) :- !,
	throw( cut(Witness, true) ).


% Control Predicates
% -----
% Control predicates must consider how cuts affect their behavior. In most
% cases, the implementation of the control predicate should catch cuts thrown
% by the individual sub-queries and throw a new cut describing how the overall
% query is affected.
%
% TODO: Implication (`->`) and the soft-cut (`*->`)

call_rw_(Witness, M:(A->B)) :- !,
	call_rw_(Witness, M:(A->B;fail)).

call_rw_(Witness, M:(A->B;C)) :- !,
	(
		call_rw(M:A),
		!,
		call_rw_(Witness, M:B)
	;
		call_rw_(Witness, M:C)
	).

call_rw_(Witness, M:(A,B)) :- !,
	catch((
		call_rw_(Witness, M:A)
	), cut(Witness, Resume), (
		throw( cut(Witness, (Resume,(M:B))) )
	)),
	call_rw_(Witness, M:B).

call_rw_(Witness, M:(A;B)) :- !,
	(
		call_rw_(Witness, M:A)
	;
		call_rw_(Witness, M:B)
	).

call_rw_(Witness, M:catch(Goal,Ball,Recover)) :- !,
	catch((
		call_rw(Witness, M:Goal)
	), Ball, (
		call_rw(Witness, M:Recover)
	)).


% Type-checking predicates
% -----
% In this system, it is most useful for certain type-checking predicates to
% be delayed until the term they are applied to is bound.

call_rw_(_, M:number(X)) :- !, freeze(X, M:number(X)).
call_rw_(_, M:list(X))   :- !, freeze(X, M:member(X,[[],[_|_]])).


% Regular queries
% -----
% Regular queries are the non-control, non-builtin predicates. These are the
% kinds of queries that get rewritten if they fail.

call_rw_(Witness, M:Goal) :-
	(
		call_rw_regular(Witness, M:Goal)
	*->true;
		Success = bool(false),
		rewrite(Goal, RwGoal),
		(
			call_rw_regular(Witness, M:RwGoal),
			nb_setarg(1, Success, true)
		;
			Success == bool(true) -> !
		)
	).

call_rw_regular(Witness, M:Goal) :-
	catch((catch((
		% Expand the goal and evaluate the body in the meta-language
		catch((
			clause(M:Goal, Body),
			once((
				predicate_property(M:Goal, imported_from(BodyModule))
			;
				BodyModule = M
			;
				BodyModule = user
			)),
			call_rw_(Witness, BodyModule:Body)
		), cut(Witness, Resume), (
			call_rw(Goal, Resume)
		))

	), error(permission_error(access,private_procedure,_),_), (
		% If there is a permission error,
		% we must let the underlying system handle the query.
		call(M:Goal)

	))), error(type_error(_,_),_), (
		% Ignore type errors.
		% Assume the query can be rewritten into valid types.
		fail
	)).
