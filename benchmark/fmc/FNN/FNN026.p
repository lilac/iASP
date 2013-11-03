fof(green_clique,axiom,(
    ! [A,B,C,D,E] :
      ( ( green(A,C)
        & green(A,D)
        & green(B,D)
        & green(D,E)
        & green(C,E)
        & green(B,E)
        & green(A,E)
        & green(C,D)
        & green(B,C)
        & green(A,B) )
     => goal ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

fof(partition,axiom,(
    ! [A,B] :
      ( ( green(A,B)
        | red(A,B) )
     <= less_than(A,B) ) )).

fof(red_clique,axiom,(
    ! [A,B,C,D,E] :
      ( goal
     <= ( red(A,B)
        & red(B,C)
        & red(B,E)
        & red(C,E)
        & red(D,E)
        & red(A,E)
        & red(C,D)
        & red(B,D)
        & red(A,D)
        & red(A,C) ) ) )).

fof(less_than_transitive,axiom,(
    ! [A,B,C] :
      ( less_than(A,C)
     <= ( less_than(A,B)
        & less_than(B,C) ) ) )).

fof(no_overlap,axiom,(
    ! [A,B] :
      ( goal
     <= ( green(A,B)
        & red(A,B) ) ) )).

fof(ordering,axiom,
    ( less_than(n1,n2)
    & less_than(n7,n8)
    & less_than(n9,n10)
    & less_than(n12,n13)
    & less_than(n13,n14)
    & less_than(n14,n15)
    & less_than(n15,n16)
    & less_than(n11,n12)
    & less_than(n10,n11)
    & less_than(n8,n9)
    & less_than(n6,n7)
    & less_than(n5,n6)
    & less_than(n4,n5)
    & less_than(n3,n4)
    & less_than(n2,n3) )).

