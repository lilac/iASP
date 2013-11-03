fof(partition,axiom,(
    ! [A,B] :
      ( less_than(A,B)
     => ( green(A,B)
        | red(A,B) ) ) )).

fof(less_than_transitive,axiom,(
    ! [A,B,C] :
      ( less_than(A,C)
     <= ( less_than(A,B)
        & less_than(B,C) ) ) )).

fof(red_clique,axiom,(
    ! [A,B,C,D,E,F] :
      ( ( red(B,C)
        & red(A,D)
        & red(C,D)
        & red(D,E)
        & red(A,F)
        & red(B,F)
        & red(C,F)
        & red(E,F)
        & red(D,F)
        & red(C,E)
        & red(B,E)
        & red(A,E)
        & red(B,D)
        & red(A,C)
        & red(A,B) )
     => goal ) )).

fof(green_clique,axiom,(
    ! [A,B,C,D,E,F] :
      ( goal
     <= ( green(A,B)
        & green(A,C)
        & green(B,C)
        & green(A,D)
        & green(C,D)
        & green(A,E)
        & green(C,E)
        & green(D,E)
        & green(D,F)
        & green(E,F)
        & green(C,F)
        & green(B,F)
        & green(A,F)
        & green(B,E)
        & green(B,D) ) ) )).

fof(ordering,axiom,
    ( less_than(n3,n4)
    & less_than(n4,n5)
    & less_than(n5,n6)
    & less_than(n10,n11)
    & less_than(n9,n10)
    & less_than(n8,n9)
    & less_than(n7,n8)
    & less_than(n6,n7)
    & less_than(n2,n3)
    & less_than(n1,n2) )).

fof(no_overlap,axiom,(
    ! [A,B] :
      ( ( red(A,B)
        & green(A,B) )
     => goal ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

