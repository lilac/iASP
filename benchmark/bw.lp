#const t = 1.
time(0..t).

location(X) :- block(X).
location(table).

covered(X, T) :- on(A, X, T), block(X).

nmove(X, Y, T) :- move(X1, Y, T), block(X), location(Y), time(T), X1 != X, X != Y.
nmove(X, Y, T) :- move(X, Y1, T), block(X), location(Y), time(T), Y != Y1, X != Y.
nmove(X, Y, T) :- move(X1, Y1, T), block(X), location(Y), X1 != X, Y1 != Y.
% nmove(X, Y, T) :- move(Y, X, T), block(X).

move(X, Y, T) :- block(X), location(Y), time(T), not covered(X, T), not covered(Y, T), not nmove(X, Y, T), X != Y, T < t.

moved(X, T) :- move(X, Y, T).

on(X, Y, 0) :- init(X, Y).
on(X, Y, T+1) :- move(X, Y, T), time(T), T < t.
on(X, Y, T+1) :- on(X, Y, T), time(T), not moved(X, T), T < t.

:- goal(X, Y), not on(X, Y, t).
