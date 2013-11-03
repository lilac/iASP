
#cumulative __inc.

fdom(__inc).
arg(__inc,__inc).

%{ assign(D,__inc) } :- dom(D), not functerm(D).
{ assign(D,__inc) } :- dom(D), order(D,O), O>=__inc, not functerm(D).

:- assign(T,E), assign(T,__inc), E<__inc, fdom(E).

#volatile __inc.

:- [ assign(D,X):fdom(X) ] 0, dom(D).
#cumulative __inc.

% flattened_0
:- q(X0, X0), fdom(X0), 1 [ arg(X0,__inc) ].

% flattened_1
:- assign(a,X0), not p(X0), fdom(X0), 1 [ arg(X0,__inc) ].

% flattened_2
:- assign(esk1_1(X0),X1), p(X0), not q(X0, X1), fdom(X0;X1), 1 [ arg(X0;X1,__inc) ].


% functions
dom(esk1_1(X0)) :- fdom(X0), 1 [ arg(X0,__inc) ].
functerm(esk1_1(X0)) :- fdom(X0), 1 [ arg(X0,__inc) ].
{ assign(esk1_1(X0),Y) } :- fdom(X0;Y), 1 [ arg(X0;Y,__inc) ].

% internal predicates

% input predicates
{ q(X0,X1) } :- fdom(X0;X1), 1 [ arg(X0;X1,__inc) ].
{ p(X0) } :- fdom(X0), 1 [ arg(X0,__inc) ].

#base.

dom(a).
order(a,1).

#hide.
#show assign/2.
#show q/2.
#show p/1.
