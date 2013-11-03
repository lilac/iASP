include('Axioms/GEO009+0.ax').
fof(con,conjecture,(
    ! [L,A,B,C] :
      ~ ( between_on_line(L,A,B,C)
        & between_on_line(L,B,A,C) ) )).

