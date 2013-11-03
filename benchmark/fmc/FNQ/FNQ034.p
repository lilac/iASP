fof(ax33,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( sound(U,V)
     <= scream(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( fire(U,V)
     => event(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( ? [V,W,X,Y,Z,X1,X2] :
            ( male(U,W)
            & of(U,X,W)
            & ! [X3] :
                ( ? [X4] :
                    ( nonreflexive(U,X4)
                    & fire(U,X4)
                    & from_loc(U,X4,X)
                    & present(U,X4)
                    & patient(U,X4,X3)
                    & agent(U,X4,W)
                    & event(U,X4) )
               <= member(U,X3,Y) )
            & six(U,Y)
            & ! [X5] :
                ( member(U,X5,Y)
               => shot(U,X5) )
            & cry(U,X1)
            & agent(U,X2,V)
            & present(U,X2)
            & nonreflexive(U,X2)
            & of(U,X2,Z)
            & scream(U,X2)
            & patient(U,X2,X1)
            & event(U,X2)
            & revenge(U,Z)
            & group(U,Y)
            & cannon(U,X)
            & man(U,W)
            & male(U,V) )
        & actual_world(U) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( cry(U,V)
     => event(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( revenge(U,V)
     => action(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( ? [W] :
          ( ? [X] :
              ( W != X
              & ? [Y] :
                  ( member(U,Y,V)
                  & W != Y
                  & ? [Z] :
                      ( member(U,Z,V)
                      & Z != W
                      & ? [X1] :
                          ( X1 != Y
                          & X1 != X
                          & X1 != W
                          & ? [X2] :
                              ( X2 != Y
                              & ! [X3] :
                                  ( member(U,X3,V)
                                 => ( X3 = X
                                    | W = X3
                                    | Y = X3
                                    | Z = X3
                                    | X1 = X3
                                    | X3 = X2 ) )
                              & X2 != W
                              & X2 != X
                              & X2 != Z
                              & X2 != X1
                              & member(U,X2,V) )
                          & Z != X1
                          & member(U,X1,V) )
                      & X != Z
                      & Y != Z )
                  & X != Y )
              & member(U,X,V) )
          & member(U,W,V) )
    <=> six(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax45,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= scream(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V,W,X] :
      ( ( agent(U,V,W)
        & patient(U,V,X)
        & nonreflexive(U,V) )
     => W != X ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => existent(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( act(U,V)
     => event(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( act(U,V)
     <= action(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( sound(U,V)
     => event(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weaponry(U,V)
     <= weapon(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= weaponry(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= shot(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     <= cannon(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( six(U,V)
     => group(U,V) ) )).

