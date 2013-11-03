include('Axioms/GEO009+0.ax').
fof(con,conjecture,(
    ! [A,B,L] :
      ( ( before_on_line(L,A,B)
        & before_on_line(L,B,A) )
     <= ( incident_point_and_line(A,L)
        & incident_point_and_line(B,L)
        & distinct_points(A,B) ) ) )).

