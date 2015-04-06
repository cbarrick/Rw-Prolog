:- use_module('../src/rewrite').


nth(0, [H|_]) := H.
nth(N, [_|T]) := nth(N0, T) :-
	between(1,inf,N),
	N0 is N - 1.


length(List)       := rw_length(0, List).
rw_length(N,[])    := N :- !.
rw_length(N,[_|T]) := rw_length(N+1,T).


reverse(List)     := reverse(List, []).
reverse([], X)    := X.
reverse([H|T], X) := reverse(T, [H|X]).
