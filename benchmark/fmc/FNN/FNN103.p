include('Axioms/GEO009+0.ax').
fof(con,conjecture,(
    ! [L,M] :
      ( equally_directed_lines(L,reverse_line(M))
     <= equally_directed_opposite_lines(L,reverse_line(M)) ) )).

