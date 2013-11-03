fof(no_overlap,axiom,(
    ! [A,B] :
      ( ( red(A,B)
        & green(A,B) )
     => goal ) )).

fof(less_than_transitive,axiom,(
    ! [A,B,C] :
      ( ( less_than(B,C)
        & less_than(A,B) )
     => less_than(A,C) ) )).

fof(green_clique,axiom,(
    ! [A,B,C,D] :
      ( ( green(A,C)
        & green(B,C)
        & green(A,D)
        & green(C,D)
        & green(B,D)
        & green(A,B) )
     => goal ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

fof(partition,axiom,(
    ! [A,B] :
      ( less_than(A,B)
     => ( red(A,B)
        | green(A,B) ) ) )).

fof(ordering,axiom,
    ( less_than(n3,n4)
    & less_than(n4,n5)
    & less_than(n6,n7)
    & less_than(n7,n8)
    & less_than(n9,n10)
    & less_than(n10,n11)
    & less_than(n8,n9)
    & less_than(n5,n6)
    & less_than(n2,n3)
    & less_than(n1,n2) )).

fof(red_clique,axiom,(
    ! [A,B,C,D] :
      ( goal
     <= ( red(A,B)
        & red(A,C)
        & red(A,D)
        & red(C,D)
        & red(B,D)
        & red(B,C) ) ) )).

