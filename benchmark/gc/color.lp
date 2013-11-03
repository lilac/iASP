%%%%%%
#base.

% undirected edges and node degrees
edge(Y,X) :- edge(X,Y).
degree(X,D) :- node(X), D := #count{ Y != X : edge(X,Y) }.

% total node order (configurable whether by increasing or decreasing degree)
#const ord = -1.
next(X,Y) :- degree(X,DX;;Y,DY), (ord*DX,X) < (ord*DY,Y),
             (ord*DZ,Z) <= (ord*DX,X) : degree(Z,DZ) : (ord*DZ,Z) < (ord*DY,Y).
numb(X,1) :- (_,X) := #min[ degree(Y,DY) = (ord*DY,Y) ].
numb(Y,N+1) :- numb(X,N), next(X,Y).

%%%%%%%%%%%%%%
#cumulative k.

% generate (unique) color assignment
{ color(X,k) } :- numb(X,N), k <= N.
colored(X,k) :- numb(X,N), k <= N, color(X,k-1).
colored(X,k) :- numb(X,N), k <= N, colored(X,k-1).
 :- color(X,k), colored(X,k).

% enforce usage of smallest available color
exclude(X,k) :- numb(X,N), k <= N, edge(X,Y), X != Y, color(Y,k).
disable(X,k) :- exclude(X,k), disable(X,k-1) : 1 < k.
 :- numb(X,N), k <= N, disable(X,k-1) : 1 < k, not disable(X,k), not color(X,k).
 :- disable(X,k), numb(X,k).

% symmetry breaking by color usage
allow(Y,k) :- color(X,k-1), next(X,Y), 2 < k.
allow(Y,k) :- allow(X,k), next(X,Y).
 :- color(Y,k), 2 < k, not allow(Y,k).

%%%%%%%%%%%%
#volatile k.

% enforce totality of color assignment
 :- disable(X,k), numb(X,N), k < N.

%%%%%%
#base.

% output projection
#hide.
#show color/2.
