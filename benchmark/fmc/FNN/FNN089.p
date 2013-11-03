include('Axioms/GEO006+0.ax').
include('Axioms/GEO006+2.ax').
include('Axioms/GEO006+3.ax').
include('Axioms/GEO006+5.ax').
fof(con,conjecture,(
    ! [X,Y] :
      ( ? [Z] :
          ( point(Z)
          & ~ apt(Z,Y) )
     <= ( line(Y)
        & point(X) ) ) )).

