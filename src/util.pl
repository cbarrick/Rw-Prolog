:- module(util, [
	multi_subset/2,
	replacen/5,
	list/1
]).


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
