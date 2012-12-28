#cumulative k.
{ color(X, k) } :- node(X).
:- color(X, C), color(X, k), node(X), C < k.
:- edge(X, Y), color(X, k), color(Y, k).

#volatile k.
:- node(X), { color(X, D) : D <= k} 0.

#base.
#show color/2.
