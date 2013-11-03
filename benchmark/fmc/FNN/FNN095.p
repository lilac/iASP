include('Axioms/GEO008+0.ax').
include('Axioms/GEO006+2.ax').
include('Axioms/GEO006+3.ax').
include('Axioms/GEO006+5.ax').
fof(con,conjecture,(
    ! [X,Y] :
      ( ? [Z] :
          ( point(Z)
          & ~ apt(Z,Y) )
     <= ( point(X)
        & line(Y) ) ) )).

