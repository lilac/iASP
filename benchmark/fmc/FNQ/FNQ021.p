fof(ax27,axiom,(
    ! [U,V] :
      ( action(U,V)
     => act(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= fire(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= revenge(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= organism(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( actual_world(U)
        & ? [V,W,X,Y,Z,X1] :
            ( ! [X4] :
                ( member(U,X4,X)
               => shot(U,X4) )
            & revenge(U,Y)
            & present(U,X1)
            & nonreflexive(U,X1)
            & scream(U,X1)
            & of(U,X1,Y)
            & patient(U,X1,Z)
            & agent(U,X1,X)
            & event(U,X1)
            & cry(U,Z)
            & group(U,X)
            & six(U,X)
            & ! [X2] :
                ( ? [X3] :
                    ( event(U,X3)
                    & agent(U,X3,V)
                    & present(U,X3)
                    & from_loc(U,X3,W)
                    & fire(U,X3)
                    & nonreflexive(U,X3)
                    & patient(U,X3,X2) )
               <= member(U,X2,X) )
            & cannon(U,W)
            & of(U,W,V)
            & man(U,V)
            & male(U,V)
            & male(U,X) ) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( six(U,V)
     => group(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax45,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( cry(U,V)
     => event(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V,W,X] :
      ( ( agent(U,V,W)
        & patient(U,V,X)
        & nonreflexive(U,V) )
     => W != X ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= eventuality(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= sound(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= scream(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( shot(U,V)
     => action(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( cannon(U,V)
     => weapon(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= weaponry(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( ? [W] :
          ( ? [X] :
              ( W != X
              & ? [Y] :
                  ( Y != X
                  & Y != W
                  & ? [Z] :
                      ( member(U,Z,V)
                      & Y != Z
                      & X != Z
                      & ? [X1] :
                          ( X1 != Z
                          & ? [X2] :
                              ( member(U,X2,V)
                              & X2 != X1
                              & X2 != Z
                              & Y != X2
                              & X2 != W
                              & ! [X3] :
                                  ( ( X2 = X3
                                    | X3 = X1
                                    | W = X3
                                    | X = X3
                                    | X3 = Y
                                    | X3 = Z )
                                 <= member(U,X3,V) )
                              & X != X2 )
                          & W != X1
                          & X != X1
                          & X1 != Y
                          & member(U,X1,V) )
                      & W != Z )
                  & member(U,Y,V) )
              & member(U,X,V) )
          & member(U,W,V) )
    <=> six(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     => weaponry(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => sound(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= act(U,V) ) )).

