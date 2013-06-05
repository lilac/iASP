% Define
location(table).
location(X) :- block(X).
on(X, Y, 0) :- init(X, Y).
%
#cumulative t.
% Generate
1 { move(X,Y,t) : block(X) : location(Y) : X != Y } 1.
% Test
:- move(X,Y,t), 1 { on(A,X,t-1), on(B,Y,t-1) : B != X : Y != table }.
% Define
on(X,Y,t) :- move(X,Y,t).
on(X,Z,t) :- on(X,Z,t-1), { move(X,Y,t) : Y != Z } 0.
%
#volatile t.
% Test
:- goal(X,Y), not on(X,Y,t).
%
#base.
% Display
#hide.
#show move/3.
