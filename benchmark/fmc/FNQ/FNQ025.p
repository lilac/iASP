fof(ax40,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V,W,X] :
      ( X != W
     <= ( nonreflexive(U,V)
        & agent(U,V,W)
        & patient(U,V,X) ) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( six(U,V)
    <=> ? [W] :
          ( member(U,W,V)
          & ? [X] :
              ( member(U,X,V)
              & ? [Y] :
                  ( Y != W
                  & ? [Z] :
                      ( Y != Z
                      & ? [X1] :
                          ( member(U,X1,V)
                          & X1 != Z
                          & Y != X1
                          & ? [X2] :
                              ( X2 != X1
                              & Y != X2
                              & X2 != X
                              & X2 != W
                              & ! [X3] :
                                  ( member(U,X3,V)
                                 => ( X3 = X2
                                    | X1 = X3
                                    | X3 = X
                                    | X3 = W
                                    | Y = X3
                                    | Z = X3 ) )
                              & Z != X2
                              & member(U,X2,V) )
                          & X1 != W
                          & X != X1 )
                      & W != Z
                      & X != Z
                      & member(U,Z,V) )
                  & Y != X
                  & member(U,Y,V) )
              & X != W ) ) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( fire(U,V)
     => event(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( cannon(U,V)
     => weapon(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= sound(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( act(U,V)
     => event(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( sound(U,V)
     <= scream(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= revenge(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( actual_world(U)
        & ? [V,W,X,Y,Z,X1,X2] :
            ( male(U,W)
            & ! [X3] :
                ( member(U,X3,Y)
               => ? [X4] :
                    ( patient(U,X4,X3)
                    & present(U,X4)
                    & from_loc(U,X4,X)
                    & fire(U,X4)
                    & nonreflexive(U,X4)
                    & agent(U,X4,W)
                    & event(U,X4) ) )
            & ! [X5] :
                ( shot(U,X5)
               <= member(U,X5,Y) )
            & cry(U,Z)
            & agent(U,X2,V)
            & patient(U,X2,Z)
            & nonreflexive(U,X2)
            & of(U,X2,X1)
            & scream(U,X2)
            & present(U,X2)
            & event(U,X2)
            & revenge(U,X1)
            & group(U,Y)
            & six(U,Y)
            & cannon(U,X)
            & of(U,X,W)
            & man(U,W)
            & male(U,V) ) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( cry(U,V)
     => event(U,V) ) )).

fof(ax45,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     => object(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( weaponry(U,V)
     => instrumentality(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( act(U,V)
     <= action(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( shot(U,V)
     => action(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( scream(U,V)
     => event(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weaponry(U,V)
     <= weapon(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( six(U,V)
     => group(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

