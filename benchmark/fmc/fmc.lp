
#cumulative __inc.

fdom(__inc).
arg(__inc,__inc).

%{ assign(D,__inc) } :- dom(D), not functerm(D).
{ assign(D,__inc) } :- dom(D), order(D,O), O>=__inc, not functerm(D).

:- assign(T,E), assign(T,__inc), E<__inc, fdom(E).

#volatile __inc.

:- [ assign(D,X):fdom(X) ] 0, dom(D).
