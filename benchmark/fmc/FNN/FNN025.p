fof(ordering,axiom,
    ( less_than(n1,n2)
    & less_than(n2,n3)
    & less_than(n3,n4)
    & less_than(n4,n5)
    & less_than(n5,n6)
    & less_than(n7,n8)
    & less_than(n8,n9)
    & less_than(n9,n10)
    & less_than(n10,n11)
    & less_than(n6,n7) )).

fof(partition,axiom,(
    ! [A,B] :
      ( ( red(A,B)
        | green(A,B) )
     <= less_than(A,B) ) )).

fof(no_overlap,axiom,(
    ! [A,B] :
      ( ( green(A,B)
        & red(A,B) )
     => goal ) )).

fof(green_clique,axiom,(
    ! [A,B,C,D,E] :
      ( ( green(A,B)
        & green(A,C)
        & green(B,C)
        & green(A,D)
        & green(B,D)
        & green(A,E)
        & green(D,E)
        & green(C,E)
        & green(B,E)
        & green(C,D) )
     => goal ) )).

fof(red_clique,axiom,(
    ! [A,B,C,D,E] :
      ( ( red(A,C)
        & red(B,C)
        & red(B,D)
        & red(C,D)
        & red(D,E)
        & red(C,E)
        & red(B,E)
        & red(A,E)
        & red(A,D)
        & red(A,B) )
     => goal ) )).

fof(less_than_transitive,axiom,(
    ! [A,B,C] :
      ( ( less_than(A,B)
        & less_than(B,C) )
     => less_than(A,C) ) )).

fof(goal_to_be_proved,conjecture,(
    goal )).

