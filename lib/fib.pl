:- use_module('../src/rewrite').


fibonacci_seq := fib(0,1).

fib(A,B) := [A|fib(B,C)] :- C is A+B.


lazy_nth1(1, [H|_]) := H :- !.
lazy_nth1(N, [_|T]) := lazy_nth1(N0, T) :- N > 1, N0 is N - 1.
