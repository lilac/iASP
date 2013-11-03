fof(red_clique,axiom,(
    ! [A,B,C,D] :
      ( goal
     <= ( red(A,B)
        & red(C,D)
        & red(B,D)
        & red(A,D)
        & red(B,C)
        & red(A,C) ) ) )).

fof(ordering,axiom,
    ( less_than(n2,n3)
    & less_than(n3,n4)
    & less_than(n4,n5)
    & less_than(n6,n7)
    & less_than(n8,n9)
    & less_than(n12,n13)
    & less_than(n13,n14)
    & less_than(n15,n16)
    & less_than(n16,n17)
    & less_than(n14,n15)
    & less_than(n11,n12)
    & less_than(n10,n11)
    & less_than(n9,n10)
    & less_than(n7,n8)
    & less_than(n5,n6)
    & less_than(n1,n2) )).

fof(partition,axiom,(
    ! [A,B] :
      ( ( green(A,B)
        | red(A,B) )
     <= less_than(A,B) ) )).

fof(green_clique,axiom,(
    ! [A,B,C,D] :
      ( goal
     <= ( green(A,C)
        & green(A,D)
        & green(C,D)
        & green(B,D)
        & green(B,C)
        & green(A,B) ) ) )).

fof(less_than_transitive,axiom,(
    ! [A,B,C] :
      ( less_than(A,C)
     <= ( less_than(A,B)
        & less_than(B,C) ) ) )).

fof(no_overlap,axiom,(
    ! [A,B] :
      ( ( red(A,B)
        & green(A,B) )
     => goal ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

