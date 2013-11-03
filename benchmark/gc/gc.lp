#const c=3.
1 { color(X, 1..c) } 1 :- node(X).
:- color(X, C), color(Y, C), edge(X, Y).

#hide.
#show color/2.
