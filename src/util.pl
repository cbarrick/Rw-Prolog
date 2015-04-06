:- module(util, [
	multi_subset/2,
	replacen/5,
	list/1,
	distinct/1,
	distinct/2,
	limit/2,
	term_depth/2
]).

:- use_module(library(nb_set)).


%! multi_subset(?Sub:list, +List:list) is nondet
% True when Sub is a multi-subset of List. That is, all elements of Sub are
% also elements of List; elements of Sub occur no more often than they occur
% in List; and elemnts of Sub occur in the same order that they occur in List.
%
% The benifit of this predicate over the SWI builtin subset/2 is that the
% sub-list can be generated non-deterministicly.
%
% @see http://stackoverflow.com/a/4917016
%
% @arg Sub is the subset
% @arg List is the original list

multi_subset([], []).
multi_subset([E|NTail], [E|Tail]):-
	multi_subset(NTail, Tail).
multi_subset(NTail, [_|Tail]):-
	multi_subset(NTail, Tail).


%! replacen(?N:integer, ?Subset:list, ?Original:list, ?NewSubset:list, ?New:list)
% Like SWI's select/4. The New list is like the Original list with an ordered
% Subset of N elements replaced by the elements of NewSubset.

replacen(N, Subset, Original, NewSubset, New) :-
	length(Original, L),
	length(New, L),
	between(0, L, N),
	replacen_(N, Subset, Original, NewSubset, New).

replacen_(0, [], X, [], X) :- !.

replacen_(N, [H0|Subset], [H0|Original], [H1|NewSubset], [H1|New]) :-
	N > 0,
	N0 is N - 1,
	replacen_(N0, Subset, Original, NewSubset, New).

replacen_(N, Subset, [H0|Original], NewSubset, [H0|New]) :-
	N > 0,
	replacen_(N, Subset, Original, NewSubset, New).


%! list(?L) is nondet
% True when L is a list.

list([]).
list([_|T]) :- list(T).


%! distinct(:Goal).
%! distinct(?Witness, :Goal).
%
% True if Goal is true and  no   previous  solution  of Goal bound
% Witness to the same value. The  variant distinct/1 is equivalent
% to distinct(Goal,Goal). Semantically, distinct/1 is  the same as
% the code below, but answers are returned  as soon as they become
% available rather than first computing the complete answer set.
%
% Taken directly from SWI-Prolog v7.
% https://github.com/SWI-Prolog/swipl-devel/blob/63c06e36dccaf1b8a9b158043726de5fa00d4e53/library/solution_sequences.pl#L119
%
% ```
% distinct(Goal) :-
%     findall(Goal, Goal, List),
%     list_to_set(List, Set),
%     member(Goal, Set).
% ```

:- meta_predicate distinct(0).
:- meta_predicate distinct(:,0).

distinct(Goal) :-
	distinct(Goal, Goal).
distinct(Witness, Goal) :-
	term_variables(Witness, Vars),
	Witness1 =.. [v|Vars],
	empty_nb_set(Set),
	call(Goal),
	add_nb_set(Witness1, Set, true).


%! limit(+Count, :Goal)
%
% Limit the number of solutions. True   if Goal is true, returning
% at most Count solutions. Solutions are  returned as soon as they
% become  available.
%
% Taken directly from SWI-Prolog v7
%
% https://github.com/SWI-Prolog/swipl-devel/blob/63c06e36dccaf1b8a9b158043726de5fa00d4e53/library/solution_sequences.pl#L128

:- meta_predicate limit(+,0).

limit(Count, Goal) :-
	Count > 0,
	State = count(0),
	call(Goal),
	arg(1, State, N0),
	N is N0+1,
	(   N =:= Count
	->  !
	;   nb_setarg(1, State, N)
	).


%! term_depth(+Term, -Depth)
%
% Depth is the maximum height of the Term's syntax tree.

term_depth(Term, 1) :- atomic(Term), !.
term_depth(Term, 1) :- var(Term), !.
term_depth(Term, Depth) :-
	Term =.. [_|Args],
	maplist(term_depth, Args, Depths),
	max_list(Depths, D0),
	Depth is D0 + 1.
