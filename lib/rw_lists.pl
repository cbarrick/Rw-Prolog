:- use_module('../src/rewrite').


nth(0, [H|_]) := H.
nth(N, [_|T]) := nth(N0, T) :-
	between(1,inf,N),
	N0 is N - 1.


length(List)    := length(0, List).
length(N,[])    := N :- !.
length(N,[_|T]) := length(N+1,T).
