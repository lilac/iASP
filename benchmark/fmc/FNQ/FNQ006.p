fof(ax13,axiom,(
    ! [U,V] :
      ( unisex(U,V)
     <= object(U,V) ) )).

fof(ax38,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     <= abstraction(U,V) ) )).

fof(ax5,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => impartial(U,V) ) )).

fof(ax36,axiom,(
    ! [U,V] :
      ( abstraction(U,V)
     => unisex(U,V) ) )).

fof(ax31,axiom,(
    ! [U,V] :
      ( event(U,V)
     => eventuality(U,V) ) )).

fof(ax30,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => thing(U,V) ) )).

fof(ax33,axiom,(
    ! [U,V] :
      ( five(U,V)
     => group(U,V) ) )).

fof(ax19,axiom,(
    ! [U,V] :
      ( object(U,V)
     => entity(U,V) ) )).

fof(ax54,axiom,(
    ! [U,V,W,X] :
      ( W != X
     <= ( nonreflexive(U,V)
        & agent(U,V,W)
        & patient(U,V,X) ) ) )).

fof(ax7,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => organism(U,V) ) )).

fof(ax4,axiom,(
    ! [U,V] :
      ( organism(U,V)
     => living(U,V) ) )).

fof(ax40,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= abstraction(U,V) ) )).

fof(ax26,axiom,(
    ! [U,V] :
      ( order(U,V)
     => act(U,V) ) )).

fof(ax35,axiom,(
    ! [U,V] :
      ( set(U,V)
     <= group(U,V) ) )).

fof(ax45,axiom,(
    ! [U,V] :
      ( animate(U,V)
     => ~ nonliving(U,V) ) )).

fof(ax2,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     => animate(U,V) ) )).

fof(ax17,axiom,(
    ! [U,V] :
      ( specific(U,V)
     <= entity(U,V) ) )).

fof(ax10,axiom,(
    ! [U,V] :
      ( relation(U,V)
     => abstraction(U,V) ) )).

fof(ax52,axiom,(
    ! [U,V] :
      ( ~ female(U,V)
     <= unisex(U,V) ) )).

fof(ax12,axiom,(
    ! [U,V] :
      ( forename(U,V)
     => relname(U,V) ) )).

fof(ax25,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= act(U,V) ) )).

fof(ax11,axiom,(
    ! [U,V] :
      ( relname(U,V)
     => relation(U,V) ) )).

fof(ax1,axiom,(
    ! [U,V] :
      ( woman(U,V)
     => female(U,V) ) )).

fof(ax24,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= order(U,V) ) )).

fof(ax56,axiom,(
    ! [U] :
      ~ ? [V] : member(U,V,V) )).

fof(ax39,axiom,(
    ! [U,V] :
      ( thing(U,V)
     => singleton(U,V) ) )).

fof(ax50,axiom,(
    ! [U,V] :
      ( singleton(U,V)
     => ~ multiple(U,V) ) )).

fof(ax34,axiom,(
    ! [U,V] :
      ( multiple(U,V)
     <= set(U,V) ) )).

fof(ax47,axiom,(
    ! [U,V] :
      ( nonhuman(U,V)
     => ~ human(U,V) ) )).

fof(ax37,axiom,(
    ! [U,V] :
      ( general(U,V)
     <= abstraction(U,V) ) )).

fof(ax23,axiom,(
    ! [U,V] :
      ( beverage(U,V)
     <= shake_beverage(U,V) ) )).

fof(ax51,axiom,(
    ! [U,V] :
      ( ~ general(U,V)
     <= specific(U,V) ) )).

fof(ax6,axiom,(
    ! [U,V] :
      ( entity(U,V)
     <= organism(U,V) ) )).

fof(ax18,axiom,(
    ! [U,V] :
      ( thing(U,V)
     <= entity(U,V) ) )).

fof(ax32,axiom,(
    ! [U,V] :
      ( event(U,V)
     <= cost(U,V) ) )).

fof(ax44,axiom,(
    ! [U,V] :
      ( cash(U,V)
     <= dollar(U,V) ) )).

fof(ax53,axiom,(
    ! [U,V,W] :
      ( ( of(U,W,V)
        & forename(U,W)
        & entity(U,V) )
     => ~ ? [X] :
            ( forename(U,X)
            & of(U,X,V)
            & W != X ) ) )).

fof(co1,conjecture,(
    ~ ? [U] :
        ( actual_world(U)
        & ? [V,W,X,Y,Z] :
            ( woman(U,V)
            & mia_forename(U,W)
            & forename(U,W)
            & patient(U,Y,X)
            & group(U,Z)
            & ! [X3] :
                ( member(U,X3,Z)
               => dollar(U,X3) )
            & five(U,Z)
            & ! [X1] :
                ( member(U,X1,Z)
               => ? [X2] :
                    ( event(U,X2)
                    & patient(U,X2,X1)
                    & nonreflexive(U,X2)
                    & cost(U,X2)
                    & present(U,X2)
                    & agent(U,X2,X) ) )
            & order(U,Y)
            & nonreflexive(U,Y)
            & past(U,Y)
            & agent(U,Y,V)
            & event(U,Y)
            & shake_beverage(U,X)
            & of(U,W,V)
            & nonhuman(U,X) ) ) )).

fof(ax46,axiom,(
    ! [U,V] :
      ( existent(U,V)
     => ~ nonexistent(U,V) ) )).

fof(ax41,axiom,(
    ! [U,V] :
      ( possession(U,V)
     => abstraction(U,V) ) )).

fof(ax28,axiom,(
    ! [U,V] :
      ( nonexistent(U,V)
     <= eventuality(U,V) ) )).

fof(ax8,axiom,(
    ! [U,V] :
      ( human_person(U,V)
     <= woman(U,V) ) )).

fof(ax48,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     => ~ living(U,V) ) )).

fof(ax42,axiom,(
    ! [U,V] :
      ( possession(U,V)
     <= currency(U,V) ) )).

fof(ax9,axiom,(
    ! [U,V] :
      ( forename(U,V)
     <= mia_forename(U,V) ) )).

fof(ax55,axiom,(
    ! [U,V] :
      ( ? [W] :
          ( ? [X] :
              ( ? [Y] :
                  ( X != Y
                  & Y != W
                  & ? [Z] :
                      ( ? [X1] :
                          ( member(U,X1,V)
                          & X1 != Y
                          & X1 != X
                          & ! [X2] :
                              ( ( W = X2
                                | X2 = X
                                | X2 = Y
                                | Z = X2
                                | X1 = X2 )
                             <= member(U,X2,V) )
                          & W != X1
                          & X1 != Z )
                      & Z != W
                      & Z != X
                      & Z != Y
                      & member(U,Z,V) )
                  & member(U,Y,V) )
              & X != W
              & member(U,X,V) )
          & member(U,W,V) )
    <=> five(U,V) ) )).

fof(ax49,axiom,(
    ! [U,V] :
      ( ~ past(U,V)
     <= present(U,V) ) )).

fof(ax22,axiom,(
    ! [U,V] :
      ( food(U,V)
     <= beverage(U,V) ) )).

fof(ax15,axiom,(
    ! [U,V] :
      ( nonliving(U,V)
     <= object(U,V) ) )).

fof(ax29,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => specific(U,V) ) )).

fof(ax21,axiom,(
    ! [U,V] :
      ( food(U,V)
     => substance_matter(U,V) ) )).

fof(ax20,axiom,(
    ! [U,V] :
      ( object(U,V)
     <= substance_matter(U,V) ) )).

fof(ax43,axiom,(
    ! [U,V] :
      ( currency(U,V)
     <= cash(U,V) ) )).

fof(ax27,axiom,(
    ! [U,V] :
      ( eventuality(U,V)
     => unisex(U,V) ) )).

fof(ax3,axiom,(
    ! [U,V] :
      ( human(U,V)
     <= human_person(U,V) ) )).

fof(ax14,axiom,(
    ! [U,V] :
      ( impartial(U,V)
     <= object(U,V) ) )).

fof(ax16,axiom,(
    ! [U,V] :
      ( existent(U,V)
     <= entity(U,V) ) )).

