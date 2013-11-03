fof(ax9,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= forename(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( man(U,V)
     => male(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( placename(U,V)
     => relname(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( state(U,V)
     => eventuality(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( hollywood_placename(U,V)
     => placename(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ? [U] :
          ( ? [V,W,X,Y,Z,X1,X2] :
              ( frontseat(U,V)
              & white(U,W)
              & dirty(U,W)
              & of(U,X,Y)
              & street(U,Y)
              & event(U,Z)
              & present(U,Z)
              & barrel(U,Z)
              & down(U,Z,Y)
              & group(U,X1)
              & ! [X6] :
                  ( member(U,X6,X1)
                 => ( young(U,X6)
                    & fellow(U,X6) ) )
              & ! [X10] :
                  ( member(U,X10,X2)
                 => ( coat(U,X10)
                    & black(U,X10)
                    & cheap(U,X10) ) )
              & group(U,X2)
              & ! [X7] :
                  ( ! [X8] :
                      ( ? [X9] :
                          ( event(U,X9)
                          & agent(U,X9,X8)
                          & patient(U,X9,X7)
                          & nonreflexive(U,X9)
                          & wear(U,X9)
                          & present(U,X9) )
                     <= member(U,X8,X1) )
                 <= member(U,X7,X2) )
              & two(U,X1)
              & ! [X3] :
                  ( ? [X4,X5] :
                      ( in(U,X5,V)
                      & be(U,X4,X3,X5)
                      & state(U,X4) )
                 <= member(U,X3,X1) )
              & in(U,Z,Y)
              & agent(U,Z,W)
              & lonely(U,Y)
              & placename(U,X)
              & hollywood_placename(U,X)
              & city(U,Y)
              & old(U,W)
              & chevy(U,W) )
          & actual_world(U) )
      & ~ ? [X11] :
            ( actual_world(X11)
            & ? [X12,X13,V,W,X,Y,Z,X1,X2,X14,X15] :
                ( man(X11,X12)
                & jules_forename(X11,X13)
                & forename(X11,X13)
                & white(X11,W)
                & dirty(X11,W)
                & of(X11,X,Y)
                & city(X11,Y)
                & event(X11,Z)
                & agent(X11,Z,W)
                & present(X11,Z)
                & barrel(X11,Z)
                & ! [X3] :
                    ( member(X11,X3,X1)
                   => ? [X4,X5] :
                        ( state(X11,X4)
                        & in(X11,X5,V)
                        & be(X11,X4,X3,X5) ) )
                & two(X11,X1)
                & ! [X6] :
                    ( ( fellow(X11,X6)
                      & young(X11,X6) )
                   <= member(X11,X6,X1) )
                & group(X11,X2)
                & ! [X10] :
                    ( ( cheap(X11,X10)
                      & black(X11,X10)
                      & coat(X11,X10) )
                   <= member(X11,X10,X2) )
                & state(X11,X14)
                & be(X11,X14,X12,X15)
                & behind(X11,X15,W)
                & ! [X7] :
                    ( ! [X8] :
                        ( member(X11,X8,X1)
                       => ? [X9] :
                            ( event(X11,X9)
                            & agent(X11,X9,X8)
                            & patient(X11,X9,X7)
                            & present(X11,X9)
                            & nonreflexive(X11,X9)
                            & wear(X11,X9) ) )
                   <= member(X11,X7,X2) )
                & group(X11,X1)
                & in(X11,Z,Y)
                & down(X11,Z,Y)
                & lonely(X11,Y)
                & street(X11,Y)
                & placename(X11,X)
                & hollywood_placename(X11,X)
                & old(X11,W)
                & chevy(X11,W)
                & frontseat(X11,V)
                & wheel(X11,W)
                & of(X11,X13,X12) ) ) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax69,axiom,(
    ! [U,V,W,X] :
      ( ( patient(U,V,X)
        & agent(U,V,W)
        & nonreflexive(U,V) )
     => W != X ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( location(U,V)
     => object(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( way(U,V)
     => artifact(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= clothes(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( barrel(U,V)
     => event(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= transport(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( relname(U,V)
     => relation(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     => instrumentality(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( man(U,V)
     <= fellow(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => nonhuman(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( wheel(U,V)
     => device(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => existent(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( seat(U,V)
     <= frontseat(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( group(U,V)
     <= two(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= eventuality(U,V) ) )).

fof(ax70,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax63,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( ~ human(U,V)
     <= nonhuman(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax66,axiom,(
    ! [U,V,W] :
      ( ~ ? [X] :
            ( W != X
            & of(U,X,V)
            & forename(U,X) )
     <= ( entity(U,V)
        & forename(U,W)
        & of(U,W,V) ) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

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

fof(ax12,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => thing(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( coat(U,V)
     => clothes(U,V) ) )).

fof(ax64,axiom,(
    ! [U,V] :
      ( white(U,V)
     => ~ black(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( device(U,V)
     => instrumentality(U,V) ) )).

fof(ax65,axiom,(
    ! [U,V] :
      ( ~ old(U,V)
     <= young(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= state(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax71,axiom,(
    ! [U,V,W,X] :
      ( be(U,V,W,X)
     => X = W ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => general(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax62,axiom,(
    ! [U,V] :
      ( ~ general(U,V)
     <= specific(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( car(U,V)
     <= chevy(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( forename(U,V)
     <= jules_forename(U,V) ) )).

fof(ax67,axiom,(
    ! [U,V,W] :
      ( ( of(U,W,V)
        & placename(U,W)
        & entity(U,V) )
     => ~ ? [X] :
            ( of(U,X,V)
            & W != X
            & placename(U,X) ) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( city(U,V)
     => location(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( seat(U,V)
     => furniture(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( street(U,V)
     => way(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( wear(U,V)
     => event(U,V) ) )).

