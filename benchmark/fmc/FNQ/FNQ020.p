fof(ax11,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V,W,X] :
      ( ( patient(U,V,X)
        & agent(U,V,W)
        & nonreflexive(U,V) )
     => X != W ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= weaponry(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( cannon(U,V)
     => weapon(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( action(U,V)
     <= shot(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( ? [W] :
          ( ? [X] :
              ( ? [Y] :
                  ( Y != W
                  & ? [Z] :
                      ( X != Z
                      & Z != W
                      & ? [X1] :
                          ( member(U,X1,V)
                          & X1 != Z
                          & X1 != X
                          & W != X1
                          & ? [X2] :
                              ( member(U,X2,V)
                              & Z != X2
                              & X2 != X
                              & ! [X3] :
                                  ( member(U,X3,V)
                                 => ( X3 = X2
                                    | X1 = X3
                                    | X3 = Z
                                    | X = X3
                                    | X3 = W
                                    | Y = X3 ) )
                              & W != X2
                              & X2 != Y
                              & X1 != X2 )
                          & X1 != Y )
                      & Z != Y
                      & member(U,Z,V) )
                  & X != Y
                  & member(U,Y,V) )
              & W != X
              & member(U,X,V) )
          & member(U,W,V) )
    <=> six(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax41,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( ? [V] :
            ( ! [W,X] :
                ( ? [Y] :
                    ( event(U,Y)
                    & present(U,Y)
                    & from_loc(U,Y,X)
                    & fire(U,Y)
                    & nonreflexive(U,Y)
                    & patient(U,Y,X)
                    & agent(U,Y,W) )
               <= ( male(U,W)
                  & man(U,W)
                  & member(U,X,V)
                  & cannon(U,X)
                  & of(U,X,W) ) )
            & group(U,V)
            & ! [Z] :
                ( shot(U,Z)
               <= member(U,Z,V) )
            & six(U,V) )
        & actual_world(U) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= eventuality(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( weapon(U,V)
     => weaponry(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= act(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( action(U,V)
     => act(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( six(U,V)
     => group(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= fire(U,V) ) )).

