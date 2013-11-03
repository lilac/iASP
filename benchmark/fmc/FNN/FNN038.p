fof(goal_to_be_proved,conjecture,(
    goal )).

fof(n1_equal_n0,axiom,
    ( equalish(n1,n0)
   => goal )).

fof(n0_equal_n1,axiom,
    ( equalish(n0,n1)
   => goal )).

fof(relation_exists,axiom,(
    ! [A,B,C,D,E] :
      ( ( equalish(B,B)
        & equalish(C,C)
        & equalish(E,E)
        & equalish(D,D)
        & equalish(A,A) )
     => ? [F] : f(A,B,C,D,E,F) ) )).

fof(n0_and_n1_reflexive,axiom,
    ( equalish(n0,n0)
    & equalish(n1,n1) )).

fof(relation_injective,axiom,(
    ! [A,B,C,D,E,F,G,H,I,J,K] :
      ( ( equalish(D,I)
        & equalish(E,J)
        & equalish(C,H)
        & equalish(B,G)
        & equalish(A,F) )
     <= ( f(A,B,C,D,E,K)
        & f(F,G,H,I,J,K) ) ) )).

