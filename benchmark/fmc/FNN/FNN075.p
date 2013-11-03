fof(main,conjecture,(
    ~ ? [X] :
        ~ ( ! [Y] :
              ( ~ r1(X,Y)
              | ~ ! [X] :
                    ( ~ r1(Y,X)
                    | ~ ! [Y] :
                          ( p6(Y)
                          | ~ r1(X,Y) ) ) )
          | ! [Y] :
              ( ! [X] :
                  ( ~ p1(X)
                  | ~ r1(Y,X) )
              | ~ p6(Y)
              | ~ r1(X,Y) ) ) )).

fof(reflexivity,axiom,(
    ! [X] : r1(X,X) )).

fof(transitivity,axiom,(
    ! [X,Y,Z] :
      ( ( r1(Y,Z)
        & r1(X,Y) )
     => r1(X,Z) ) )).

