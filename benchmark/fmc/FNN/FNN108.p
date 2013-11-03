include('Axioms/GEO009+0.ax').
fof(con,conjecture,(
    ! [A,B] :
      ( equally_directed_lines(line_connecting(B,A),reverse_line(line_connecting(A,B)))
      & equal_lines(line_connecting(A,B),line_connecting(B,A)) ) )).

