fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     <= cannon(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( six(U,V)
    <=> ? [W] :
          ( member(U,W,V)
          & ? [X] :
              ( member(U,X,V)
              & X != W
              & ? [Y] :
                  ( ? [Z] :
                      ( member(U,Z,V)
                      & Y != Z
                      & ? [X1] :
                          ( Y != X1
                          & X1 != W
                          & ? [X2] :
                              ( X2 != X1
                              & Z != X2
                              & X != X2
                              & ! [X3] :
                                  ( member(U,X3,V)
                                 => ( X2 = X3
                                    | X3 = Z
                                    | W = X3
                                    | X3 = X
                                    | Y = X3
                                    | X3 = X1 ) )
                              & W != X2
                              & Y != X2
                              & member(U,X2,V) )
                          & X1 != X
                          & X1 != Z
                          & member(U,X1,V) )
                      & W != Z
                      & X != Z )
                  & Y != W
                  & X != Y
                  & member(U,Y,V) ) ) ) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= fire(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V,W,X] :
      ( ( nonreflexive(U,V)
        & agent(U,V,W)
        & patient(U,V,X) )
     => W != X ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(ax41,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     => weaponry(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( action(U,V)
     => act(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( actual_world(U)
        & ? [V,W,X] :
            ( ! [Y] :
                ( ? [Z] :
                    ( agent(U,Z,V)
                    & present(U,Z)
                    & nonreflexive(U,Z)
                    & from_loc(U,Z,Y)
                    & fire(U,Z)
                    & patient(U,Z,Y)
                    & event(U,Z) )
               <= ( cannon(U,Y)
                  & member(U,Y,X)
                  & of(U,Y,W) ) )
            & ! [X1] :
                ( member(U,X1,X)
               => shot(U,X1) )
            & group(U,X)
            & six(U,X)
            & male(U,W)
            & man(U,V) ) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( group(U,V)
     <= six(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= shot(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( man(U,V)
     => human_person(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= weaponry(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= eventuality(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= act(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     => object(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

