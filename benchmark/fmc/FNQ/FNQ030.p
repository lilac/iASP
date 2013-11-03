fof(ax8,axiom,(
    ! [U,V] :
      ( chevy(U,V)
     => car(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( location(U,V)
     <= city(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ~ ? [X7] :
            ( ? [X8,V,W,X,Y,Z,X1,X2,X9] :
                ( frontseat(X7,V)
                & of(X7,X,W)
                & hollywood_placename(X7,X)
                & placename(X7,X)
                & dirty(X7,Y)
                & lonely(X7,Z)
                & agent(X7,X1,Y)
                & present(X7,X1)
                & down(X7,X1,Z)
                & in(X7,X1,W)
                & ! [X6] :
                    ( member(X7,X6,X2)
                   => ( young(X7,X6)
                      & fellow(X7,X6) ) )
                & ! [X10] :
                    ( member(X7,X10,X9)
                   => ! [X11] :
                        ( member(X7,X11,X8)
                       => ? [X12] :
                            ( agent(X7,X12,X11)
                            & patient(X7,X12,X10)
                            & nonreflexive(X7,X12)
                            & wear(X7,X12)
                            & present(X7,X12)
                            & event(X7,X12) ) ) )
                & group(X7,X9)
                & ! [X13] :
                    ( member(X7,X13,X9)
                   => ( cheap(X7,X13)
                      & black(X7,X13)
                      & coat(X7,X13) ) )
                & group(X7,X2)
                & two(X7,X2)
                & ! [X3] :
                    ( member(X7,X3,X2)
                   => ? [X4,X5] :
                        ( state(X7,X4)
                        & be(X7,X4,X3,X5)
                        & in(X7,X5,V) ) )
                & barrel(X7,X1)
                & event(X7,X1)
                & street(X7,Z)
                & old(X7,Y)
                & white(X7,Y)
                & chevy(X7,Y)
                & city(X7,W)
                & group(X7,X8) )
            & actual_world(X7) )
      & ? [U] :
          ( actual_world(U)
          & ? [V,W,X,Y,Z,X1,X2] :
              ( frontseat(U,V)
              & hollywood_placename(U,X)
              & placename(U,X)
              & chevy(U,Y)
              & white(U,Y)
              & event(U,X1)
              & down(U,X1,Z)
              & in(U,X1,W)
              & ! [X3] :
                  ( member(U,X3,X2)
                 => ? [X4,X5] :
                      ( state(U,X4)
                      & be(U,X4,X3,X5)
                      & in(U,X5,V) ) )
              & group(U,X2)
              & ! [X6] :
                  ( ( fellow(U,X6)
                    & young(U,X6) )
                 <= member(U,X6,X2) )
              & two(U,X2)
              & barrel(U,X1)
              & present(U,X1)
              & agent(U,X1,Y)
              & lonely(U,Z)
              & street(U,Z)
              & old(U,Y)
              & dirty(U,Y)
              & city(U,W)
              & of(U,X,W) ) ) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => nonhuman(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= state(U,V) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( ~ black(U,V)
     <= white(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     <= relation(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= location(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     => instrumentality(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( two(U,V)
     => group(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( coat(U,V)
     => clothes(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( seat(U,V)
     <= frontseat(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= state(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( way(U,V)
     <= street(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( young(U,V)
     => ~ old(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax62,axiom,(
    ! [U,V,W] :
      ( ~ ? [X] :
            ( of(U,X,V)
            & W != X
            & placename(U,X) )
     <= ( of(U,W,V)
        & placename(U,W)
        & entity(U,V) ) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     => object(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= way(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( specific(U,V)
     => ~ general(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= placename(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= wear(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax66,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax65,axiom,(
    ! [U,V,W,X] :
      ( ( nonreflexive(U,V)
        & patient(U,V,X)
        & agent(U,V,W) )
     => X != W ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( barrel(U,V)
     => event(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= object(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( man(U,V)
     => male(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax64,axiom,(
    ! [U,V] :
      ( two(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( ! [Y] :
                  ( member(U,Y,V)
                 => ( X = Y
                    | W = Y ) )
              & W != X
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V,W,X] :
      ( W = X
     <= be(U,V,W,X) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( fellow(U,V)
     => man(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( hollywood_placename(U,V)
     => placename(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( man(U,V)
     => human_person(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= transport(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     => artifact(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => thing(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( seat(U,V)
     => furniture(U,V) ) )).

