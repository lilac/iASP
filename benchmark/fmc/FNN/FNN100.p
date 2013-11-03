include('Axioms/GEO006+0.ax').
include('Axioms/GEO006+1.ax').
include('Axioms/GEO006+2.ax').
include('Axioms/GEO006+3.ax').
include('Axioms/GEO006+4.ax').
include('Axioms/GEO006+5.ax').
include('Axioms/GEO006+6.ax').
fof(con,conjecture,(
    ! [L,M] :
      ( not_orthogonal_lines(M,L)
     <= not_orthogonal_lines(L,M) ) )).

