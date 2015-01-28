% Simple example of infinite rewrite paths that converge
z(X) := a(X+1).
a(X) := z(X).
b(X) := z(X).


% An infinite counting example
s(X) := s(Y) :- Y is X + 1.
