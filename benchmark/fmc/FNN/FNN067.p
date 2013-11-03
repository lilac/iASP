fof(m2,axiom,(
    ! [P,Q,R,S] :
      ( ( meets(R,S)
        & meets(P,Q) )
     => ( ( ? [T] :
              ( meets(P,T)
              & meets(T,S) )
        <~> meets(P,S) )
      <~> ? [T] :
            ( meets(R,T)
            & meets(T,Q) ) ) ) )).

fof(not_m5,axiom,(
    ~ ! [P,Q] :
        ( meets(P,Q)
       => ? [R,S,T] :
            ( meets(R,P)
            & meets(R,T)
            & meets(T,S)
            & meets(Q,S) ) ) )).

fof(m1,axiom,(
    ! [P,Q,R,S] :
      ( meets(R,S)
     <= ( meets(P,Q)
        & meets(P,S)
        & meets(R,Q) ) ) )).

fof(m3,axiom,(
    ! [P] :
    ? [Q,R] :
      ( meets(P,R)
      & meets(Q,P) ) )).

