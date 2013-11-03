fof(ax52,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     <= coat(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= transport(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( ~ human(U,V)
     <= nonhuman(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax64,axiom,(
    ! [U,V] :
      ( two(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( W != X
              & ! [Y] :
                  ( ( Y = X
                    | W = Y )
                 <= member(U,Y,V) )
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     => transport(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => unisex(U,V) ) )).

fof(ax66,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( state(U,V)
     => event(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => existent(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= placename(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V,W,X] :
      ( W = X
     <= be(U,V,W,X) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     => artifact(U,V) ) )).

fof(ax65,axiom,(
    ! [U,V,W,X] :
      ( X != W
     <= ( agent(U,V,W)
        & patient(U,V,X)
        & nonreflexive(U,V) ) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( ~ old(U,V)
     <= young(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     => instrumentality(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     <= seat(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ? [U] :
          ( ? [V,W,X,Y,Z,X1] :
              ( placename(U,W)
              & white(U,X)
              & street(U,Y)
              & lonely(U,Y)
              & agent(U,Z,X)
              & down(U,Z,Y)
              & two(U,X1)
              & ! [X5] :
                  ( member(U,X5,X1)
                 => ( fellow(U,X5)
                    & young(U,X5) ) )
              & group(U,X1)
              & ! [X2] :
                  ( ? [X3,X4] :
                      ( be(U,X3,X2,X4)
                      & in(U,X4,V)
                      & state(U,X3) )
                 <= member(U,X2,X1) )
              & in(U,Z,X)
              & barrel(U,Z)
              & present(U,Z)
              & event(U,Z)
              & old(U,X)
              & dirty(U,X)
              & chevy(U,X)
              & hollywood_placename(U,W)
              & city(U,X)
              & of(U,W,X)
              & frontseat(U,V) )
          & actual_world(U) )
      & ~ ? [X6] :
            ( actual_world(X6)
            & ? [X7,V,W,X,Y,Z,X1,X8] :
                ( group(X6,X7)
                & frontseat(X6,V)
                & city(X6,X)
                & hollywood_placename(X6,W)
                & white(X6,X)
                & old(X6,X)
                & lonely(X6,Y)
                & agent(X6,Z,X)
                & down(X6,Z,Y)
                & in(X6,Z,X)
                & two(X6,X1)
                & group(X6,X1)
                & ! [X9] :
                    ( member(X6,X9,X8)
                   => ! [X10] :
                        ( ? [X11] :
                            ( event(X6,X11)
                            & agent(X6,X11,X10)
                            & patient(X6,X11,X9)
                            & nonreflexive(X6,X11)
                            & wear(X6,X11)
                            & present(X6,X11) )
                       <= member(X6,X10,X7) ) )
                & ! [X12] :
                    ( member(X6,X12,X8)
                   => ( cheap(X6,X12)
                      & black(X6,X12)
                      & coat(X6,X12) ) )
                & group(X6,X8)
                & ! [X5] :
                    ( ( fellow(X6,X5)
                      & young(X6,X5) )
                   <= member(X6,X5,X1) )
                & ! [X2] :
                    ( member(X6,X2,X1)
                   => ? [X3,X4] :
                        ( in(X6,X4,V)
                        & be(X6,X3,X2,X4)
                        & state(X6,X3) ) )
                & barrel(X6,Z)
                & present(X6,Z)
                & event(X6,Z)
                & street(X6,Y)
                & dirty(X6,X)
                & chevy(X6,X)
                & placename(X6,W)
                & of(X6,W,X) ) ) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= state(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( barrel(U,V)
     => event(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( chevy(U,V)
     => car(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( street(U,V)
     => way(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= way(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     <= abstraction(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( location(U,V)
     <= city(U,V) ) )).

fof(ax62,axiom,(
    ! [U,V,W] :
      ( ~ ? [X] :
            ( X != W
            & of(U,X,V)
            & placename(U,X) )
     <= ( of(U,W,V)
        & placename(U,W)
        & entity(U,V) ) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= wear(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( fellow(U,V)
     => man(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( specific(U,V)
     => ~ general(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( frontseat(U,V)
     => seat(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( hollywood_placename(U,V)
     => placename(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => general(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= location(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( two(U,V)
     => group(U,V) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( white(U,V)
     => ~ black(U,V) ) )).

