fof(ax53,axiom,(
    ! [U,V,W] :
      ( ( forename(U,W)
        & of(U,W,V)
        & entity(U,V) )
     => ~ ? [X] :
            ( of(U,X,V)
            & W != X
            & forename(U,X) ) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( group(U,V)
     => set(U,V) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( ? [V,W,X,Y,Z] :
            ( of(U,W,V)
            & woman(U,V)
            & mia_forename(U,W)
            & shake_beverage(U,X)
            & agent(U,Y,V)
            & patient(U,Y,X)
            & past(U,Y)
            & nonreflexive(U,Y)
            & ! [X1] :
                ( ? [X2] :
                    ( event(U,X2)
                    & patient(U,X2,X1)
                    & present(U,X2)
                    & nonreflexive(U,X2)
                    & cost(U,X2)
                    & agent(U,X2,Y) )
               <= member(U,X1,Z) )
            & five(U,Z)
            & group(U,Z)
            & ! [X3] :
                ( dollar(U,X3)
               <= member(U,X3,Z) )
            & order(U,Y)
            & event(U,Y)
            & forename(U,W)
            & nonhuman(U,Y) )
        & actual_world(U) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => nonhuman(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( possession(U,V)
     <= currency(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( currency(U,V)
     <= cash(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( living(U,V)
     <= organism(U,V) ) )).

fof(ax56,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => general(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= forename(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( female(U,V)
     <= woman(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( order(U,V)
     => act(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( five(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( ? [Y] :
                  ( Y != W
                  & ? [Z] :
                      ( member(U,Z,V)
                      & Z != Y
                      & W != Z
                      & ? [X1] :
                          ( member(U,X1,V)
                          & X1 != Z
                          & X1 != W
                          & ! [X2] :
                              ( ( X2 = Y
                                | W = X2
                                | X = X2
                                | X2 = Z
                                | X1 = X2 )
                             <= member(U,X2,V) )
                          & X1 != X
                          & Y != X1 )
                      & Z != X )
                  & X != Y
                  & member(U,Y,V) )
              & W != X
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( organism(U,V)
     <= human_person(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( order(U,V)
     => event(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( cash(U,V)
     <= dollar(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( forename(U,V)
     <= mia_forename(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( woman(U,V)
     => human_person(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( food(U,V)
     => substance_matter(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     <= thing(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= substance_matter(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => nonexistent(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( ~ past(U,V)
     <= present(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( cost(U,V)
     => event(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( possession(U,V)
     => abstraction(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( act(U,V)
     => event(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( agent(U,V,W)
        & patient(U,V,X)
        & nonreflexive(U,V) ) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( beverage(U,V)
     <= shake_beverage(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     <= event(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( food(U,V)
     <= beverage(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( relname(U,V)
     => relation(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( five(U,V)
     => group(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     => ~ female(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( ~ general(U,V)
     <= specific(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => unisex(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( ~ human(U,V)
     <= nonhuman(U,V) ) )).

