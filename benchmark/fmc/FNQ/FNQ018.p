fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= cry(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( cannon(U,V)
     => weapon(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( six(U,V)
    <=> ? [W] :
          ( member(U,W,V)
          & ? [X] :
              ( W != X
              & ? [Y] :
                  ( W != Y
                  & ? [Z] :
                      ( Y != Z
                      & Z != W
                      & ? [X1] :
                          ( Z != X1
                          & X1 != Y
                          & X != X1
                          & W != X1
                          & ? [X2] :
                              ( member(U,X2,V)
                              & ! [X3] :
                                  ( ( X3 = X2
                                    | X1 = X3
                                    | X3 = Z
                                    | X3 = Y
                                    | X = X3
                                    | W = X3 )
                                 <= member(U,X3,V) )
                              & W != X2
                              & X2 != X
                              & X2 != Y
                              & Z != X2
                              & X1 != X2 )
                          & member(U,X1,V) )
                      & Z != X
                      & member(U,Z,V) )
                  & X != Y
                  & member(U,Y,V) )
              & member(U,X,V) ) ) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( sound(U,V)
     => event(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( man(U,V)
     => human_person(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => event(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => existent(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( act(U,V)
     => event(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax45,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= revenge(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( weaponry(U,V)
     => instrumentality(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= shot(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => sound(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( ? [V,W,X,Y,Z,X1] :
            ( male(U,X)
            & revenge(U,Z)
            & event(U,X1)
            & of(U,X1,Z)
            & scream(U,X1)
            & nonreflexive(U,X1)
            & present(U,X1)
            & patient(U,X1,Y)
            & agent(U,X1,X)
            & cry(U,Y)
            & ! [X4] :
                ( member(U,X4,X)
               => shot(U,X4) )
            & group(U,X)
            & six(U,X)
            & ! [X2] :
                ( member(U,X2,X)
               => ? [X3] :
                    ( patient(U,X3,X2)
                    & nonreflexive(U,X3)
                    & fire(U,X3)
                    & from_loc(U,X3,W)
                    & present(U,X3)
                    & agent(U,X3,V)
                    & event(U,X3) ) )
            & cannon(U,W)
            & of(U,W,V)
            & man(U,V)
            & male(U,V) )
        & actual_world(U) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= organism(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     => weaponry(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( six(U,V)
     => group(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & patient(U,V,X)
        & agent(U,V,W) ) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( action(U,V)
     => act(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= fire(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

