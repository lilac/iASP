fof(ax19,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     => weaponry(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( cannon(U,V)
     => weapon(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( fire(U,V)
     => event(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= revenge(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( act(U,V)
     => event(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= shot(U,V) ) )).

fof(ax45,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( cry(U,V)
     => event(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( action(U,V)
     => act(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( group(U,V)
     <= six(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( sound(U,V)
     => event(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( weaponry(U,V)
     => instrumentality(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ~ ? [X2] :
            ( ? [V,W,X,X3,X4,X5] :
                ( male(X2,V)
                & man(X2,V)
                & of(X2,W,V)
                & event(X2,X5)
                & patient(X2,X5,X4)
                & nonreflexive(X2,X5)
                & scream(X2,X5)
                & of(X2,X5,X3)
                & present(X2,X5)
                & agent(X2,X5,V)
                & cry(X2,X4)
                & revenge(X2,X3)
                & ! [X1] :
                    ( member(X2,X1,X)
                   => shot(X2,X1) )
                & group(X2,X)
                & six(X2,X)
                & ! [Y] :
                    ( ? [Z] :
                        ( present(X2,Z)
                        & fire(X2,Z)
                        & from_loc(X2,Z,W)
                        & nonreflexive(X2,Z)
                        & patient(X2,Z,Y)
                        & agent(X2,Z,V)
                        & event(X2,Z) )
                   <= member(X2,Y,X) )
                & cannon(X2,W) )
            & actual_world(X2) )
      & ? [U] :
          ( actual_world(U)
          & ? [V,W,X] :
              ( male(U,V)
              & man(U,V)
              & of(U,W,V)
              & cannon(U,W)
              & ! [X1] :
                  ( shot(U,X1)
                 <= member(U,X1,X) )
              & group(U,X)
              & six(U,X)
              & ! [Y] :
                  ( ? [Z] :
                      ( agent(U,Z,V)
                      & patient(U,Z,Y)
                      & present(U,Z)
                      & fire(U,Z)
                      & from_loc(U,Z,W)
                      & nonreflexive(U,Z)
                      & event(U,Z) )
                 <= member(U,Y,X) ) ) ) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( six(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( ? [Y] :
                  ( member(U,Y,V)
                  & ? [Z] :
                      ( X != Z
                      & ? [X1] :
                          ( X1 != Z
                          & Y != X1
                          & X != X1
                          & X1 != W
                          & ? [X2] :
                              ( Z != X2
                              & X2 != Y
                              & X != X2
                              & ! [X3] :
                                  ( member(U,X3,V)
                                 => ( X3 = W
                                    | X3 = X
                                    | X3 = Y
                                    | Z = X3
                                    | X1 = X3
                                    | X2 = X3 ) )
                              & X2 != W
                              & X1 != X2
                              & member(U,X2,V) )
                          & member(U,X1,V) )
                      & Z != W
                      & Y != Z
                      & member(U,Z,V) )
                  & Y != W
                  & Y != X )
              & W != X
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( man(U,V)
     => human_person(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= organism(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => event(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V,W,X] :
      ( ( nonreflexive(U,V)
        & agent(U,V,W)
        & patient(U,V,X) )
     => W != X ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => sound(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

