fof(ax65,axiom,(
    ! [U,V] :
      ( young(U,V)
     => ~ old(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= forename(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( wheel(U,V)
     => device(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => unisex(U,V) ) )).

fof(ax70,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     => artifact(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( placename(U,V)
     => relname(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     => instrumentality(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => existent(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( man(U,V)
     <= fellow(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= organism(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax67,axiom,(
    ! [U,V,W] :
      ( ( entity(U,V)
        & of(U,W,V)
        & placename(U,W) )
     => ~ ? [X] :
            ( X != W
            & of(U,X,V)
            & placename(U,X) ) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax69,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & agent(U,V,W)
        & patient(U,V,X) ) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( forename(U,V)
     <= jules_forename(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax71,axiom,(
    ! [U,V,W,X] :
      ( W = X
     <= be(U,V,W,X) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( two(U,V)
     => group(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( street(U,V)
     => way(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     <= abstraction(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax68,axiom,(
    ! [U,V] :
      ( ? [W] :
          ( member(U,W,V)
          & ? [X] :
              ( ! [Y] :
                  ( member(U,Y,V)
                 => ( W = Y
                    | Y = X ) )
              & X != W
              & member(U,X,V) ) )
    <=> two(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( man(U,V)
     => male(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( state(U,V)
     => eventuality(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( placename(U,V)
     <= hollywood_placename(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= location(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= device(U,V) ) )).

fof(ax62,axiom,(
    ! [U,V] :
      ( specific(U,V)
     => ~ general(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( barrel(U,V)
     => event(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ~ ? [X11] :
            ( ? [X12,X13,V,W,X,Y,Z,X1,X2,X14,X15] :
                ( of(X11,X13,X12)
                & man(X11,X12)
                & forename(X11,X13)
                & wheel(X11,Y)
                & white(X11,W)
                & old(X11,W)
                & hollywood_placename(X11,X)
                & placename(X11,X)
                & street(X11,Y)
                & event(X11,Z)
                & agent(X11,Z,W)
                & present(X11,Z)
                & barrel(X11,Z)
                & in(X11,Z,Y)
                & ! [X3] :
                    ( ? [X4,X5] :
                        ( state(X11,X4)
                        & in(X11,X5,V)
                        & be(X11,X4,X3,X5) )
                   <= member(X11,X3,X1) )
                & group(X11,X1)
                & ! [X6] :
                    ( member(X11,X6,X1)
                   => ( young(X11,X6)
                      & fellow(X11,X6) ) )
                & ! [X7] :
                    ( ! [X8] :
                        ( ? [X9] :
                            ( agent(X11,X9,X8)
                            & present(X11,X9)
                            & nonreflexive(X11,X9)
                            & wear(X11,X9)
                            & patient(X11,X9,X7)
                            & event(X11,X9) )
                       <= member(X11,X8,X1) )
                   <= member(X11,X7,X2) )
                & ! [X10] :
                    ( ( coat(X11,X10)
                      & black(X11,X10)
                      & cheap(X11,X10) )
                   <= member(X11,X10,X2) )
                & state(X11,X14)
                & be(X11,X14,X12,X15)
                & behind(X11,X15,Y)
                & group(X11,X2)
                & two(X11,X1)
                & down(X11,Z,Y)
                & lonely(X11,Y)
                & city(X11,Y)
                & of(X11,X,Y)
                & dirty(X11,W)
                & chevy(X11,W)
                & frontseat(X11,V)
                & jules_forename(X11,X13) )
            & actual_world(X11) )
      & ? [U] :
          ( ? [V,W,X,Y,Z,X1,X2] :
              ( frontseat(U,V)
              & white(U,W)
              & dirty(U,W)
              & old(U,W)
              & of(U,X,Y)
              & city(U,Y)
              & street(U,Y)
              & agent(U,Z,W)
              & present(U,Z)
              & down(U,Z,Y)
              & group(U,X1)
              & ! [X7] :
                  ( member(U,X7,X2)
                 => ! [X8] :
                      ( member(U,X8,X1)
                     => ? [X9] :
                          ( event(U,X9)
                          & agent(U,X9,X8)
                          & present(U,X9)
                          & wear(U,X9)
                          & nonreflexive(U,X9)
                          & patient(U,X9,X7) ) ) )
              & ! [X10] :
                  ( member(U,X10,X2)
                 => ( black(U,X10)
                    & cheap(U,X10)
                    & coat(U,X10) ) )
              & group(U,X2)
              & ! [X6] :
                  ( ( young(U,X6)
                    & fellow(U,X6) )
                 <= member(U,X6,X1) )
              & two(U,X1)
              & ! [X3] :
                  ( member(U,X3,X1)
                 => ? [X4,X5] :
                      ( be(U,X4,X3,X5)
                      & in(U,X5,V)
                      & state(U,X4) ) )
              & in(U,Z,Y)
              & barrel(U,Z)
              & event(U,Z)
              & lonely(U,Y)
              & placename(U,X)
              & hollywood_placename(U,X)
              & chevy(U,W) )
          & actual_world(U) ) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax64,axiom,(
    ! [U,V] :
      ( ~ black(U,V)
     <= white(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( way(U,V)
     => artifact(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( frontseat(U,V)
     => seat(U,V) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( location(U,V)
     <= city(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( seat(U,V)
     => furniture(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= transport(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( chevy(U,V)
     => car(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= wear(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( state(U,V)
     => event(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax66,axiom,(
    ! [U,V,W] :
      ( ~ ? [X] :
            ( forename(U,X)
            & of(U,X,V)
            & W != X )
     <= ( forename(U,W)
        & of(U,W,V)
        & entity(U,V) ) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     <= coat(U,V) ) )).

