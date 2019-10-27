nd(X) := nd(Y) :- !, X < 10, Y is X + 1.
nd(X) := nd(Y) :- X > -10, Y is X - 1.

nd_nocut(X) := nd_nocut(Y) :- X < 10, Y is X + 1.
nd_nocut(X) := nd_nocut(Y) :- X > -10, Y is X - 1.
