fof(partition,axiom,(
    ! [A,B] :
      ( ( red(A,B)
        | green(A,B) )
     <= less_than(A,B) ) )).

fof(red_clique,axiom,(
    ! [A,B,C,D,E,F] :
      ( goal
     <= ( red(B,E)
        & red(C,E)
        & red(D,E)
        & red(A,F)
        & red(D,F)
        & red(E,F)
        & red(C,F)
        & red(B,F)
        & red(A,E)
        & red(C,D)
        & red(B,D)
        & red(A,D)
        & red(B,C)
        & red(A,C)
        & red(A,B) ) ) )).

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

fof(green_clique,axiom,(
    ! [A,B,C,D,E,F] :
      ( ( green(A,B)
        & green(A,D)
        & green(B,D)
        & green(C,D)
        & green(A,E)
        & green(B,E)
        & green(D,E)
        & green(E,F)
        & green(D,F)
        & green(C,F)
        & green(B,F)
        & green(A,F)
        & green(C,E)
        & green(B,C)
        & green(A,C) )
     => goal ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

fof(ordering,axiom,
    ( less_than(n1,n2)
    & less_than(n5,n6)
    & less_than(n6,n7)
    & less_than(n7,n8)
    & less_than(n9,n10)
    & less_than(n11,n12)
    & less_than(n12,n13)
    & less_than(n16,n17)
    & less_than(n17,n18)
    & less_than(n19,n20)
    & less_than(n18,n19)
    & less_than(n15,n16)
    & less_than(n14,n15)
    & less_than(n13,n14)
    & less_than(n10,n11)
    & less_than(n8,n9)
    & less_than(n4,n5)
    & less_than(n3,n4)
    & less_than(n2,n3) )).

