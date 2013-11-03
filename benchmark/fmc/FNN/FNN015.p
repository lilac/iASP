fof(green_clique,axiom,(
    ! [A,B,C,D] :
      ( goal
     <= ( green(A,B)
        & green(A,C)
        & green(A,D)
        & green(B,D)
        & green(C,D)
        & green(B,C) ) ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

fof(red_clique,axiom,(
    ! [A,B,C,D] :
      ( ( red(B,C)
        & red(A,D)
        & red(B,D)
        & red(C,D)
        & red(A,C)
        & red(A,B) )
     => goal ) )).

fof(less_than_transitive,axiom,(
    ! [A,B,C] :
      ( ( less_than(A,B)
        & less_than(B,C) )
     => less_than(A,C) ) )).

fof(ordering,axiom,
    ( less_than(n1,n2)
    & less_than(n3,n4)
    & less_than(n5,n6)
    & less_than(n6,n7)
    & less_than(n7,n8)
    & less_than(n9,n10)
    & less_than(n11,n12)
    & less_than(n10,n11)
    & less_than(n8,n9)
    & less_than(n4,n5)
    & less_than(n2,n3) )).

fof(partition,axiom,(
    ! [A,B] :
      ( less_than(A,B)
     => ( red(A,B)
        | green(A,B) ) ) )).

fof(no_overlap,axiom,(
    ! [A,B] :
      ( goal
     <= ( green(A,B)
        & red(A,B) ) ) )).

