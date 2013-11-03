fof(ax66,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => nonhuman(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( transport(U,V)
     => instrumentality(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( hollywood_placename(U,V)
     => placename(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     <= seat(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( ~ living(U,V)
     <= nonliving(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( group(U,V)
     <= two(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     <= coat(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V,W,X] :
      ( be(U,V,W,X)
     => W = X ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= state(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( state(U,V)
     => event(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( way(U,V)
     <= street(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( seat(U,V)
     <= frontseat(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( man(U,V)
     => male(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( ~ general(U,V)
     <= specific(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ~ ? [X5] :
            ( actual_world(X5)
            & ? [X6,V,W,X,Y,Z,X7] :
                ( group(X5,X6)
                & dirty(X5,V)
                & of(X5,W,X)
                & placename(X5,W)
                & street(X5,X)
                & lonely(X5,X)
                & in(X5,Y,X)
                & ! [X11] :
                    ( member(X5,X11,X7)
                   => ( cheap(X5,X11)
                      & black(X5,X11)
                      & coat(X5,X11) ) )
                & group(X5,X7)
                & ! [X8] :
                    ( ! [X9] :
                        ( ? [X10] :
                            ( event(X5,X10)
                            & wear(X5,X10)
                            & nonreflexive(X5,X10)
                            & present(X5,X10)
                            & patient(X5,X10,X8)
                            & agent(X5,X10,X9) )
                       <= member(X5,X9,X6) )
                   <= member(X5,X8,X7) )
                & ! [X4] :
                    ( ( young(X5,X4)
                      & fellow(X5,X4) )
                   <= member(X5,X4,Z) )
                & group(X5,Z)
                & two(X5,Z)
                & ! [X1] :
                    ( member(X5,X1,Z)
                   => ? [X2,X3] :
                        ( state(X5,X2)
                        & in(X5,X3,X)
                        & be(X5,X2,X1,X3) ) )
                & down(X5,Y,X)
                & barrel(X5,Y)
                & present(X5,Y)
                & agent(X5,Y,V)
                & event(X5,Y)
                & hollywood_placename(X5,W)
                & city(X5,X)
                & old(X5,V)
                & white(X5,V)
                & chevy(X5,V)
                & frontseat(X5,X) ) )
      & ? [U] :
          ( ? [V,W,X,Y,Z] :
              ( dirty(U,V)
              & old(U,V)
              & city(U,X)
              & street(U,X)
              & lonely(U,X)
              & barrel(U,Y)
              & down(U,Y,X)
              & in(U,Y,X)
              & two(U,Z)
              & group(U,Z)
              & ! [X4] :
                  ( ( young(U,X4)
                    & fellow(U,X4) )
                 <= member(U,X4,Z) )
              & ! [X1] :
                  ( member(U,X1,Z)
                 => ? [X2,X3] :
                      ( state(U,X2)
                      & in(U,X3,X)
                      & be(U,X2,X1,X3) ) )
              & present(U,Y)
              & agent(U,Y,V)
              & event(U,Y)
              & placename(U,W)
              & hollywood_placename(U,W)
              & of(U,W,X)
              & white(U,V)
              & chevy(U,V)
              & frontseat(U,X) )
          & actual_world(U) ) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( white(U,V)
     => ~ black(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax64,axiom,(
    ! [U,V] :
      ( two(U,V)
    <=> ? [W] :
          ( member(U,W,V)
          & ? [X] :
              ( member(U,X,V)
              & W != X
              & ! [Y] :
                  ( member(U,Y,V)
                 => ( X = Y
                    | W = Y ) ) ) ) ) )).

fof(ax62,axiom,(
    ! [U,V,W] :
      ( ( of(U,W,V)
        & placename(U,W)
        & entity(U,V) )
     => ~ ? [X] :
            ( X != W
            & of(U,X,V)
            & placename(U,X) ) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( city(U,V)
     => location(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( furniture(U,V)
     => instrumentality(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= wear(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax65,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & agent(U,V,W)
        & patient(U,V,X) ) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( placename(U,V)
     => relname(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( chevy(U,V)
     => car(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= clothes(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= man(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( fellow(U,V)
     => man(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( location(U,V)
     => object(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= eventuality(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( young(U,V)
     => ~ old(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( barrel(U,V)
     => event(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( way(U,V)
     => artifact(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( car(U,V)
     => vehicle(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

