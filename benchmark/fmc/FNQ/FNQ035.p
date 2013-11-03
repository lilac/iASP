fof(ax24,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     => object(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( cannon(U,V)
     => weapon(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ? [U] :
          ( actual_world(U)
          & ? [V,W,X] :
              ( male(U,V)
              & of(U,W,V)
              & six(U,X)
              & ! [X1] :
                  ( member(U,X1,X)
                 => shot(U,X1) )
              & group(U,X)
              & ! [Y] :
                  ( ? [Z] :
                      ( present(U,Z)
                      & nonreflexive(U,Z)
                      & fire(U,Z)
                      & from_loc(U,Z,W)
                      & patient(U,Z,Y)
                      & agent(U,Z,V)
                      & event(U,Z) )
                 <= member(U,Y,X) )
              & cannon(U,W)
              & man(U,V) ) )
      & ~ ? [X2] :
            ( ? [V,W,X,X3,X4,X5] :
                ( male(X2,X)
                & male(X2,V)
                & of(X2,W,V)
                & ! [X1] :
                    ( shot(X2,X1)
                   <= member(X2,X1,X) )
                & scream(X2,X5)
                & of(X2,X5,X3)
                & nonreflexive(X2,X5)
                & present(X2,X5)
                & patient(X2,X5,X4)
                & agent(X2,X5,X)
                & event(X2,X5)
                & cry(X2,X4)
                & revenge(X2,X3)
                & group(X2,X)
                & six(X2,X)
                & ! [Y] :
                    ( ? [Z] :
                        ( agent(X2,Z,V)
                        & present(X2,Z)
                        & nonreflexive(X2,Z)
                        & fire(X2,Z)
                        & from_loc(X2,Z,W)
                        & patient(X2,Z,Y)
                        & event(X2,Z) )
                   <= member(X2,Y,X) )
                & cannon(X2,W)
                & man(X2,V) )
            & actual_world(X2) ) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( man(U,V)
     => human_person(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( six(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( ? [Y] :
                  ( member(U,Y,V)
                  & Y != X
                  & W != Y
                  & ? [Z] :
                      ( Z != Y
                      & ? [X1] :
                          ( member(U,X1,V)
                          & X1 != Z
                          & X1 != Y
                          & X1 != X
                          & ? [X2] :
                              ( X2 != X1
                              & Z != X2
                              & X != X2
                              & W != X2
                              & ! [X3] :
                                  ( ( X2 = X3
                                    | X1 = X3
                                    | Z = X3
                                    | X3 = Y
                                    | X3 = W
                                    | X3 = X )
                                 <= member(U,X3,V) )
                              & Y != X2
                              & member(U,X2,V) )
                          & X1 != W )
                      & W != Z
                      & X != Z
                      & member(U,Z,V) ) )
              & X != W
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V,W,X] :
      ( X != W
     <= ( patient(U,V,X)
        & agent(U,V,W)
        & nonreflexive(U,V) ) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= weaponry(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( revenge(U,V)
     => action(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= cry(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= organism(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weaponry(U,V)
     <= weapon(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( six(U,V)
     => group(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax45,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => event(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= sound(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( sound(U,V)
     <= scream(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= act(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => existent(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( action(U,V)
     => act(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( shot(U,V)
     => action(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= fire(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

