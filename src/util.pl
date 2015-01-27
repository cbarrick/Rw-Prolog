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
