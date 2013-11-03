fof(ax64,axiom,(
    ! [U,V] :
      ( white(U,V)
     => ~ black(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= device(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( device(U,V)
     <= wheel(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( seat(U,V)
     <= frontseat(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax67,axiom,(
    ! [U,V,W] :
      ( ( of(U,W,V)
        & placename(U,W)
        & entity(U,V) )
     => ~ ? [X] :
            ( W != X
            & of(U,X,V)
            & placename(U,X) ) ) )).

fof(ax71,axiom,(
    ! [U,V,W,X] :
      ( be(U,V,W,X)
     => W = X ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => thing(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( seat(U,V)
     => furniture(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= location(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= barrel(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     => artifact(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     <= coat(U,V) ) )).

fof(ax62,axiom,(
    ! [U,V] :
      ( specific(U,V)
     => ~ general(U,V) ) )).

fof(ax65,axiom,(
    ! [U,V] :
      ( young(U,V)
     => ~ old(U,V) ) )).

fof(ax69,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & patient(U,V,X)
        & agent(U,V,W) ) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax68,axiom,(
    ! [U,V] :
      ( two(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( X != W
              & ! [Y] :
                  ( ( X = Y
                    | Y = W )
                 <= member(U,Y,V) )
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= placename(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( car(U,V)
     <= chevy(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( transport(U,V)
     => instrumentality(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( two(U,V)
     => group(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= furniture(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( street(U,V)
     => way(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= wear(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= state(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( forename(U,V)
     => relname(U,V) ) )).

fof(ax70,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax66,axiom,(
    ! [U,V,W] :
      ( ( entity(U,V)
        & forename(U,W)
        & of(U,W,V) )
     => ~ ? [X] :
            ( forename(U,X)
            & of(U,X,V)
            & W != X ) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( way(U,V)
     => artifact(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     <= abstraction(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( man(U,V)
     <= fellow(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     <= relation(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( city(U,V)
     => location(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( forename(U,V)
     <= jules_forename(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( hollywood_placename(U,V)
     => placename(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ? [U] :
          ( actual_world(U)
          & ? [V,W,X,Y,Z,X1] :
              ( chevy(U,V)
              & white(U,V)
              & of(U,W,X)
              & street(U,X)
              & barrel(U,Y)
              & down(U,Y,X)
              & ! [X2] :
                  ( member(U,X2,Z)
                 => ? [X3,X4] :
                      ( in(U,X4,X)
                      & be(U,X3,X2,X4)
                      & state(U,X3) ) )
              & two(U,Z)
              & group(U,Z)
              & ! [X5] :
                  ( member(U,X5,Z)
                 => ( young(U,X5)
                    & fellow(U,X5) ) )
              & ! [X6] :
                  ( member(U,X6,X1)
                 => ! [X7] :
                      ( ? [X8] :
                          ( agent(U,X8,X7)
                          & present(U,X8)
                          & wear(U,X8)
                          & nonreflexive(U,X8)
                          & patient(U,X8,X6)
                          & event(U,X8) )
                     <= member(U,X7,Z) ) )
              & ! [X9] :
                  ( member(U,X9,X1)
                 => ( black(U,X9)
                    & cheap(U,X9)
                    & coat(U,X9) ) )
              & group(U,X1)
              & in(U,Y,X)
              & present(U,Y)
              & agent(U,Y,V)
              & event(U,Y)
              & lonely(U,X)
              & placename(U,W)
              & hollywood_placename(U,W)
              & city(U,X)
              & old(U,V)
              & dirty(U,V)
              & frontseat(U,X) ) )
      & ~ ? [X10] :
            ( actual_world(X10)
            & ? [X11,X12,V,W,X,Y,Z,X1,X13,X14] :
                ( jules_forename(X10,X12)
                & forename(X10,X12)
                & wheel(X10,V)
                & dirty(X10,V)
                & city(X10,X)
                & hollywood_placename(X10,W)
                & event(X10,Y)
                & ! [X2] :
                    ( ? [X3,X4] :
                        ( state(X10,X3)
                        & be(X10,X3,X2,X4)
                        & in(X10,X4,X) )
                   <= member(X10,X2,Z) )
                & two(X10,Z)
                & group(X10,Z)
                & ! [X9] :
                    ( ( cheap(X10,X9)
                      & black(X10,X9)
                      & coat(X10,X9) )
                   <= member(X10,X9,X1) )
                & state(X10,X13)
                & behind(X10,X14,V)
                & be(X10,X13,X11,X14)
                & group(X10,X1)
                & ! [X6] :
                    ( ! [X7] :
                        ( ? [X8] :
                            ( agent(X10,X8,X7)
                            & present(X10,X8)
                            & nonreflexive(X10,X8)
                            & wear(X10,X8)
                            & patient(X10,X8,X6)
                            & event(X10,X8) )
                       <= member(X10,X7,Z) )
                   <= member(X10,X6,X1) )
                & ! [X5] :
                    ( member(X10,X5,Z)
                   => ( fellow(X10,X5)
                      & young(X10,X5) ) )
                & in(X10,Y,X)
                & down(X10,Y,X)
                & barrel(X10,Y)
                & present(X10,Y)
                & agent(X10,Y,V)
                & lonely(X10,X)
                & street(X10,X)
                & placename(X10,W)
                & of(X10,W,X)
                & old(X10,V)
                & white(X10,V)
                & chevy(X10,V)
                & frontseat(X10,X)
                & man(X10,X11)
                & of(X10,X12,X11) ) ) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     => object(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= state(U,V) ) )).

