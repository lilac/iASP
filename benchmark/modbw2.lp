#base.
location(X) :- block(X).
location(table).

#cumulative t.
covered(X, t) :- on(A, X, t), block(X).

nmove(X, Y, t) :- move(X1, Y, t), block(X), location(Y), X1 != X, X != Y.
nmove(X, Y, t) :- move(X, Y1, t), block(X), location(Y), Y != Y1, X != Y.
nmove(X, Y, t) :- move(X1, Y1, t), block(X), location(Y), X1 != X, Y1 != Y.
% nmove(X, Y, t) :- move(Y, X, t), block(X).

move(X, Y, t) :- block(X), location(Y), not covered(X, t-1), not covered(Y, t-1), not nmove(X, Y, t), X != Y.

moved(X, t) :- move(X, Y, t).

on(X, Y, 0) :- init(X, Y).
on(X, Y, t) :- move(X, Y, t).
on(X, Y, t) :- on(X, Y, t-1), not moved(X, t).

#volatile t.
:- goal(X, Y), not on(X, Y, t).
