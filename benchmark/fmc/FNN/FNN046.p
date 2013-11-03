fof(axiom_0,axiom,(
    ! [X] :
      ( cowlThing(X)
      & ~ cowlNothing(X) ) )).

fof(axiom_1,axiom,(
    ! [X] :
      ( ~ xsd_integer(X)
    <=> xsd_string(X) ) )).

fof(the_axiom,conjecture,
    ( ! [X] :
        ( cowlThing(X)
        & ~ cowlNothing(X) )
    & ! [X] :
        ( xsd_string(X)
      <=> ~ xsd_integer(X) )
    & cowlThing(ifred)
    & rparent(i2003_11_14_17_14_14998,i2003_11_14_17_14_14920)
    & cowlThing(i2003_11_14_17_14_14998)
    & cowlThing(i2003_11_14_17_14_14920)
    & rparent(ifred,i2003_11_14_17_14_14998) )).

fof(axiom_3,axiom,(
    cperson(ifred) )).

fof(axiom_2,axiom,(
    ! [X] :
      ( ? [Y] :
          ( cperson(Y)
          & rparent(X,Y) )
    <=> cperson(X) ) )).

