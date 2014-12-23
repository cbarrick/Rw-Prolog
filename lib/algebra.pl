:- module(algebra, [
	(:=)/2
]).

:- use_module('../src/simplify').


% Normal ordering of terms
% -------------------------
% Commutative operators need a normal ordering of arguments for a proper
% normal form. To ease the definition of mathematical rewrite rules, we define
% a normal order that differs from Prolog's standard ordering of terms. Our
% order is `Compound Terms < Variables < Atoms < Strings < Numbers` where
% numbers are sorted by value, strings and atoms are sorted lexicographicaly,
% variables are sorted by address, and compound terms are sorted by functor,
% then by arity, then by arguments in the opposite order of the standard order.

math_order(Ord, A, B) :- compound(A), compound(B), reverse_order(Ord, A, B), !.
math_order((<), A, B) :- compound(A), var(B), !.
math_order((<), A, B) :- compound(A), atom(B), !.
math_order((<), A, B) :- compound(A), string(B), !.
math_order((<), A, B) :- compound(A), number(B), !.

math_order((>), A, B) :- var(A), compound(B), !.
math_order(Ord, A, B) :- var(A), var(B), compare(Ord, A, B), !.
math_order((<), A, B) :- var(A), atom(B), !.
math_order((<), A, B) :- var(A), string(B), !.
math_order((<), A, B) :- var(A), number(B), !.

math_order((>), A, B) :- atom(A), compound(B), !.
math_order((>), A, B) :- atom(A), var(B), !.
math_order(Ord, A, B) :- atom(A), atom(B), compare(Ord, A, B), !.
math_order((<), A, B) :- atom(A), string(B), !.
math_order((<), A, B) :- atom(A), number(B), !.

math_order((>), A, B) :- string(A), compound(B), !.
math_order((>), A, B) :- string(A), var(B), !.
math_order((>), A, B) :- string(A), atom(B), !.
math_order(Ord, A, B) :- string(A), string(B), compare(Ord, A, B), !.
math_order((<), A, B) :- string(A), number(B), !.

math_order((>), A, B) :- number(A), compound(B), !.
math_order((>), A, B) :- number(A), var(B), !.
math_order((>), A, B) :- number(A), atom(B), !.
math_order((>), A, B) :- number(A), string(B), !.
math_order(Ord, A, B) :- number(A), number(B), compare(Ord, A, B), !.

%% reverse_order
%

reverse_order((=), A, B) :- A =@= B, !.
reverse_order((<), A, B) :- A @> B, !.
reverse_order((>), A, B) :- A @< B, !.


% Unary Negation
% -------------------------

% Ground evaluation
-B := C :-
	ground(B),
	C is -B.

% Distribute over addition
-(A + B) := (-A) + (-B).


% Addition
% -------------------------

% Ground evaluation
A + B := C :-
	ground(A),
	ground(B),
	C is A + B,
	!.

% Normal associativity
A + (B + C) := (A + B) + C.

% Associative
A + B + C := A + D :- simplify(B+C, D).

% Normal order
% This rule must come after the associative rule
% so that the principal functor of B is not (+).
A + B := B + A :- math_order(<, B, A).

% Identity element
A + 0 := A.

% Zero element
A + (-A) := 0.
(-A) + A := 0.

% Iterated addition
A     + A     := A * 2.
A     + (A*N) := A * X :- number(N), X is N+1.
(A*N) + (A*M) := A * X :- number(N), number(M), X is N+M.



% Subtraction
% -------------------------

% Normal associativity
A - (B - C) := (A - B) - C.

% Zero element
A - A := 0.

% Defined in terms of addition
A - B := A + (B*(-1)).


% Multiplication
% -------------------------

% Ground evaluation
A * B := C :-
	ground(A),
	ground(B),
	C is A * B,
	!.

% Normal associativity
A * (B * C) := (A * B) * C.

% Associative
(A * B) * C := A * D :- simplify(B*C, D).

% Normal order
% This rule must come after the associative rule
% so that the principal functor of B is not (*).
A * B := B * A :- math_order(<, B, A).

% Distribute over addition
A * (B + C) := (A*B) + (A*C).

% Identity element
1 * A := A.

% Zero element
_ * 0 := 0.

% Inverse element
(A/B) * (B/A) := 1.
A     * (B/A) := B.

% Multiplication of rationals
(A/B) * (C/D) := (A*C) / (B*D).
(A/B) * C     := (A*C) / B.
A     * (C/D) := (A*C) / D.

% Multiplication of exponents
A     * A     := A ^ 2.
A     * (A^B) := A ^ (B+1).
(A^B) * A     := A ^ (B+1).
(A^B) * (A^C) := A ^ (B+C).


% Division
% -------------------------

% Ground evaluation
A / B := C :-
	ground(A),
	ground(B),
	C is A / B,
	!.

% Reduce
(N0 * X) / (D0 * Y) := (N1 * X) / (D1 * Y) :- reduce_fraction(N0/D0, N1/D1).
(N0 * X) / D0       := (N1 * X) / D1       :- reduce_fraction(N0/D0, N1/D1).
N0       / (D0 * Y) := N1       / (D1 * Y) :- reduce_fraction(N0/D0, N1/D1).

% Identity element
A / 1 := A.

% Zero element
0 / _ := 0.

%
A / A := 1.

% Distribute over addition
(A + B) / C := (A/C) + (B/C).

% Division of rationals
(A/B) / (C/D) := (A/B) * (D/C).
(A/B) / C     := A / (B*C).
A     / (C/D) := (A*D) / C.

% Division of exponents
X^A     / X^B     := X ^ (A-B).
(C*X^A) / X^B     := X ^ (A-B) * C.
(X^A*C) / X^B     := X ^ (A-B) * C.
X^A     / (C*X^B) := X ^ (A-B) / C.
X^A     / (X^B*C) := X ^ (A-B) / C.
(C*X^A) / (D*X^B) := X ^ (A-B) * C/D.
(X^A*C) / (D*X^B) := X ^ (A-B) * C/D.
(C*X^A) / (X^B*D) := X ^ (A-B) * C/D.
(X^A*C) / (X^B*D) := X ^ (A-B) * C/D.


% Exponentiation
% -------------------------

% Ground evaluation
A ^ B := C :-
	ground(A),
	ground(B),
	C is A ^ B,
	!.

% Identity element
A ^ 1 := A.

% 0th power is always 1
_ ^ 0 := 1.

% Multiplicative inverse
A ^ -B := 1 / (A ^ B).
A ^ B  := 1 / (A ^ C) :- number(B), B < 0, C is -B.

% Combine exponents
(A ^ B) ^ C := A ^ (B*C).


% Roots
% -------------------------

sqrt(A)    := root(2, A).
root(N, A) := A ^ (1/N).


% Helpers
% -------------------------

reduce_fraction(N0/D0, N1/D1) :-
	integer(N0),
	integer(D0),
	Gcd is gcd(N0, D0),
	Gcd \= 1,
	N1 is N0 / Gcd,
	D1 is D0 / Gcd.
