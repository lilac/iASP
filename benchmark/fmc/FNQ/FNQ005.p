fof(ax15,axiom,(
    ! [U,V] :
      ( object(U,V)
     => nonliving(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax13,axiom,(
    ! [U,V] :
      ( object(U,V)
     => unisex(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( beverage(U,V)
     => food(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= abstraction(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( act(U,V)
     <= order(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( act(U,V)
     => event(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( substance_matter(U,V)
     => object(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => thing(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( cash(U,V)
     => currency(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( five(U,V)
    <=> ? [W] :
          ( ? [X] :
              ( ? [Y] :
                  ( ? [Z] :
                      ( member(U,Z,V)
                      & ? [X1] :
                          ( X != X1
                          & ! [X2] :
                              ( ( X2 = X1
                                | Y = X2
                                | W = X2
                                | X = X2
                                | Z = X2 )
                             <= member(U,X2,V) )
                          & X1 != W
                          & Y != X1
                          & Z != X1
                          & member(U,X1,V) )
                      & W != Z
                      & X != Z
                      & Z != Y )
                  & Y != W
                  & Y != X
                  & member(U,Y,V) )
              & W != X
              & member(U,X,V) )
          & member(U,W,V) ) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => entity(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( ~ multiple(U,V)
     <= singleton(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( cash(U,V)
     <= dollar(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( relation(U,V)
     <= relname(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( agent(U,V,W)
        & patient(U,V,X)
        & nonreflexive(U,V) ) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     <= abstraction(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( currency(U,V)
     => possession(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( specific(U,V)
     => ~ general(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( animate(U,V)
     <= human_person(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( possession(U,V)
     => abstraction(U,V) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( ~ past(U,V)
     <= present(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( ~ female(U,V)
     <= unisex(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( forename(U,V)
     <= mia_forename(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( beverage(U,V)
     <= shake_beverage(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( entity(U,V)
     => specific(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => human(U,V) ) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V,W] :
      ( ( of(U,W,V)
        & forename(U,W)
        & entity(U,V) )
     => ~ ? [X] :
            ( of(U,X,V)
            & X != W
            & forename(U,X) ) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( actual_world(U)
        & ? [V,W,X,Y,Z,X1] :
            ( of(U,X,W)
            & woman(U,W)
            & mia_forename(U,X)
            & forename(U,X)
            & shake_beverage(U,Y)
            & agent(U,Z,W)
            & patient(U,Z,Y)
            & nonreflexive(U,Z)
            & order(U,Z)
            & group(U,X1)
            & ! [X4] :
                ( dollar(U,X4)
               <= member(U,X4,X1) )
            & five(U,X1)
            & ! [X2] :
                ( ? [X3] :
                    ( event(U,X3)
                    & agent(U,X3,V)
                    & cost(U,X3)
                    & nonreflexive(U,X3)
                    & present(U,X3)
                    & patient(U,X3,X2) )
               <= member(U,X2,X1) )
            & past(U,Z)
            & event(U,Z)
            & nonhuman(U,V) ) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( set(U,V)
     => multiple(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( female(U,V)
     <= woman(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( food(U,V)
     => substance_matter(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( relname(U,V)
     <= forename(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= order(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= eventuality(U,V) ) )).

fof(ax56,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= woman(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= cost(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= eventuality(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( five(U,V)
     => group(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

