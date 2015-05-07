fibonacci_seq := fib(0,1).

fib(A,B) := [A|fib(B,C)] :- C is A+B.
