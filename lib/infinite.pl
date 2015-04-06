% Simple example of infinite rewrite paths that converge
z(X) := a(X+1).
a(X) := z(X).
b(X) := z(X).


% An infinite counting example
s(X) := s(Y) :- Y is X + 1.


% A different counting example
c(_+1+1+1) := _ :- !, fail.  % Limit rewrite to max of X+1+1+1
c(X)       := c(X+1).
