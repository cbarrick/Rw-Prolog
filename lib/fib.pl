:- use_module('../src/rewrite').


fib(A,B,C) := [A|fib(B,C,D)] :- D is B+C.
