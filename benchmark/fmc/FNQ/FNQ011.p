fof(ax52,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax68,axiom,(
    ! [U,V] :
      ( ? [W] :
          ( ? [X] :
              ( X != W
              & ! [Y] :
                  ( ( Y = W
                    | X = Y )
                 <= member(U,Y,V) )
              & member(U,X,V) )
          & member(U,W,V) )
    <=> two(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= state(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => nonhuman(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( location(U,V)
     <= city(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( transport(U,V)
     => instrumentality(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     <= relation(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= location(U,V) ) )).

fof(ax64,axiom,(
    ! [U,V] :
      ( ~ black(U,V)
     <= white(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax66,axiom,(
    ! [U,V,W] :
      ( ( entity(U,V)
        & of(U,W,V)
        & forename(U,W) )
     => ~ ? [X] :
            ( of(U,X,V)
            & X != W
            & forename(U,X) ) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax71,axiom,(
    ! [U,V,W,X] :
      ( X = W
     <= be(U,V,W,X) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( wear(U,V)
     => event(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= barrel(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     => object(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( jules_forename(U,V)
     => forename(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= state(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( placename(U,V)
     <= hollywood_placename(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= placename(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ~ ? [X11] :
            ( actual_world(X11)
            & ? [X12,X13,V,W,X,Y,Z,X1,X2,X14,X15] :
                ( of(X11,X13,X12)
                & man(X11,X12)
                & jules_forename(X11,X13)
                & wheel(X11,V)
                & frontseat(X11,V)
                & white(X11,W)
                & dirty(X11,W)
                & of(X11,X,Y)
                & placename(X11,X)
                & street(X11,Y)
                & agent(X11,Z,W)
                & in(X11,Z,Y)
                & ! [X3] :
                    ( member(X11,X3,X1)
                   => ? [X4,X5] :
                        ( be(X11,X4,X3,X5)
                        & in(X11,X5,V)
                        & state(X11,X4) ) )
                & ! [X6] :
                    ( member(X11,X6,X1)
                   => ( young(X11,X6)
                      & fellow(X11,X6) ) )
                & ! [X7] :
                    ( member(X11,X7,X2)
                   => ! [X8] :
                        ( member(X11,X8,X1)
                       => ? [X9] :
                            ( event(X11,X9)
                            & nonreflexive(X11,X9)
                            & wear(X11,X9)
                            & present(X11,X9)
                            & patient(X11,X9,X7)
                            & agent(X11,X9,X8) ) ) )
                & group(X11,X2)
                & be(X11,X14,X12,X15)
                & behind(X11,X15,V)
                & state(X11,X14)
                & ! [X10] :
                    ( ( coat(X11,X10)
                      & cheap(X11,X10)
                      & black(X11,X10) )
                   <= member(X11,X10,X2) )
                & group(X11,X1)
                & two(X11,X1)
                & down(X11,Z,Y)
                & barrel(X11,Z)
                & present(X11,Z)
                & event(X11,Z)
                & lonely(X11,Y)
                & hollywood_placename(X11,X)
                & city(X11,Y)
                & old(X11,W)
                & chevy(X11,W)
                & forename(X11,X13) ) )
      & ? [U] :
          ( actual_world(U)
          & ? [V,W,X,Y,Z,X1,X2] :
              ( frontseat(U,V)
              & placename(U,X)
              & event(U,Z)
              & agent(U,Z,W)
              & two(U,X1)
              & ! [X10] :
                  ( member(U,X10,X2)
                 => ( coat(U,X10)
                    & cheap(U,X10)
                    & black(U,X10) ) )
              & group(U,X2)
              & ! [X7] :
                  ( member(U,X7,X2)
                 => ! [X8] :
                      ( member(U,X8,X1)
                     => ? [X9] :
                          ( wear(U,X9)
                          & nonreflexive(U,X9)
                          & present(U,X9)
                          & patient(U,X9,X7)
                          & agent(U,X9,X8)
                          & event(U,X9) ) ) )
              & ! [X6] :
                  ( member(U,X6,X1)
                 => ( young(U,X6)
                    & fellow(U,X6) ) )
              & group(U,X1)
              & ! [X3] :
                  ( member(U,X3,X1)
                 => ? [X4,X5] :
                      ( state(U,X4)
                      & be(U,X4,X3,X5)
                      & in(U,X5,V) ) )
              & in(U,Z,Y)
              & down(U,Z,Y)
              & barrel(U,Z)
              & present(U,Z)
              & lonely(U,Y)
              & street(U,Y)
              & hollywood_placename(U,X)
              & city(U,Y)
              & of(U,X,Y)
              & old(U,W)
              & dirty(U,W)
              & white(U,W)
              & chevy(U,W) ) ) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax70,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax67,axiom,(
    ! [U,V,W] :
      ( ( placename(U,W)
        & of(U,W,V)
        & entity(U,V) )
     => ~ ? [X] :
            ( of(U,X,V)
            & X != W
            & placename(U,X) ) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( object(U,V)
     => impartial(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => general(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( device(U,V)
     <= wheel(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( frontseat(U,V)
     => seat(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     <= seat(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     => artifact(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( way(U,V)
     => artifact(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( chevy(U,V)
     => car(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     <= coat(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     => artifact(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( device(U,V)
     => instrumentality(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( man(U,V)
     <= fellow(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( street(U,V)
     => way(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( man(U,V)
     => male(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= forename(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V] :
      ( ~ male(U,V)
     <= unisex(U,V) ) )).

fof(ax65,axiom,(
    ! [U,V] :
      ( young(U,V)
     => ~ old(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( group(U,V)
     <= two(U,V) ) )).

fof(ax69,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & patient(U,V,X)
        & agent(U,V,W) ) ) )).

fof(ax62,axiom,(
    ! [U,V] :
      ( specific(U,V)
     => ~ general(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= furniture(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

