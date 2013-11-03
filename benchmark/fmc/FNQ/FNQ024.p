fof(ax64,axiom,(
    ! [U,V] :
      ( two(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( W != X
              & ! [Y] :
                  ( ( Y = W
                    | Y = X )
                 <= member(U,Y,V) )
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V] :
      ( ~ nonexistent(U,V)
     <= existent(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax56,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= wear(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= eventuality(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V] :
      ( ~ nonliving(U,V)
     <= animate(U,V) ) )).

fof(ax61,axiom,(
    ! [U,V] :
      ( ~ old(U,V)
     <= young(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     <= abstraction(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( man(U,V)
     <= fellow(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax66,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= state(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( seat(U,V)
     <= frontseat(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( man(U,V)
     => human_person(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( seat(U,V)
     => furniture(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( male(U,V)
     <= man(U,V) ) )).

fof(ax58,axiom,(
    ! [U,V] :
      ( ~ general(U,V)
     <= specific(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( state(U,V)
     => event(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( ~ human(U,V)
     <= nonhuman(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= way(U,V) ) )).

fof(ax65,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & patient(U,V,X)
        & agent(U,V,W) ) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax63,axiom,(
    ! [U,V,W,X] :
      ( be(U,V,W,X)
     => X = W ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= artifact(U,V) ) )).

fof(ax60,axiom,(
    ! [U,V] :
      ( white(U,V)
     => ~ black(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( transport(U,V)
     => instrumentality(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax59,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ male(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( location(U,V)
     <= city(U,V) ) )).

fof(ax57,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( placename(U,V)
     <= hollywood_placename(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( group(U,V)
     <= two(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( vehicle(U,V)
     <= car(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= barrel(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( transport(U,V)
     <= vehicle(U,V) ) )).

fof(co1,conjecture,(
    ~ ( ? [U] :
          ( actual_world(U)
          & ? [V,W,X,Y,Z] :
              ( hollywood_placename(U,V)
              & white(U,W)
              & event(U,Y)
              & agent(U,Y,W)
              & present(U,Y)
              & barrel(U,Y)
              & down(U,Y,X)
              & in(U,Y,W)
              & ! [X1] :
                  ( ? [X2,X3] :
                      ( be(U,X2,X1,X3)
                      & in(U,X3,W)
                      & state(U,X2) )
                 <= member(U,X1,Z) )
              & two(U,Z)
              & group(U,Z)
              & ! [X4] :
                  ( ( fellow(U,X4)
                    & young(U,X4) )
                 <= member(U,X4,Z) )
              & lonely(U,X)
              & street(U,X)
              & old(U,W)
              & dirty(U,W)
              & chevy(U,W)
              & placename(U,V)
              & city(U,W)
              & of(U,V,W)
              & frontseat(U,W) ) )
      & ~ ? [X5] :
            ( actual_world(X5)
            & ? [X6,V,W,X,Y,Z,X7] :
                ( of(X5,V,W)
                & city(X5,W)
                & chevy(X5,W)
                & white(X5,W)
                & dirty(X5,W)
                & street(X5,X)
                & lonely(X5,X)
                & event(X5,Y)
                & agent(X5,Y,W)
                & barrel(X5,Y)
                & ! [X1] :
                    ( ? [X2,X3] :
                        ( state(X5,X2)
                        & be(X5,X2,X1,X3)
                        & in(X5,X3,W) )
                   <= member(X5,X1,Z) )
                & group(X5,Z)
                & ! [X4] :
                    ( member(X5,X4,Z)
                   => ( fellow(X5,X4)
                      & young(X5,X4) ) )
                & ! [X8] :
                    ( member(X5,X8,X7)
                   => ! [X9] :
                        ( member(X5,X9,X6)
                       => ? [X10] :
                            ( agent(X5,X10,X9)
                            & present(X5,X10)
                            & wear(X5,X10)
                            & nonreflexive(X5,X10)
                            & patient(X5,X10,X8)
                            & event(X5,X10) ) ) )
                & ! [X11] :
                    ( member(X5,X11,X7)
                   => ( coat(X5,X11)
                      & cheap(X5,X11)
                      & black(X5,X11) ) )
                & group(X5,X7)
                & two(X5,Z)
                & in(X5,Y,W)
                & down(X5,Y,X)
                & present(X5,Y)
                & old(X5,W)
                & placename(X5,V)
                & hollywood_placename(X5,V)
                & frontseat(X5,W)
                & group(X5,X6) ) ) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( clothes(U,V)
     <= coat(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( relname(U,V)
     => relation(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= clothes(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( instrumentality(U,V)
     <= furniture(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( street(U,V)
     => way(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     <= relation(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax62,axiom,(
    ! [U,V,W] :
      ( ~ ? [X] :
            ( of(U,X,V)
            & X != W
            & placename(U,X) )
     <= ( placename(U,W)
        & of(U,W,V)
        & entity(U,V) ) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= location(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( car(U,V)
     <= chevy(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( artifact(U,V)
     <= instrumentality(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= placename(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

