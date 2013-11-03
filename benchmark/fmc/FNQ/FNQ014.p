fof(rf_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( rf(C,A)
        & A = B )
     => rf(C,B) ) )).

fof(axiom_8,axiom,(
    ! [X] :
      ( ? [Y0] : ra_Px2(X,Y0)
    <=> ca_Cx2xcomp(X) ) )).

fof(ra_Px3_substitution_1,axiom,(
    ! [A,B,C] :
      ( ra_Px3(B,C)
     <= ( A = B
        & ra_Px3(A,C) ) ) )).

fof(xsd_string_substitution_1,axiom,(
    ! [A,B] :
      ( ( xsd_string(A)
        & A = B )
     => xsd_string(B) ) )).

fof(axiom_14,axiom,(
    ! [X,Y] :
      ( rf(Y,X)
    <=> rinvF(X,Y) ) )).

fof(ca_Ax4_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Ax4(B)
     <= ( A = B
        & ca_Ax4(A) ) ) )).

fof(rr_substitution_2,axiom,(
    ! [A,B,C] :
      ( rr(C,B)
     <= ( rr(C,A)
        & B = A ) ) )).

fof(axiom_10,axiom,(
    ! [X] :
      ( ca_Cx3xcomp(X)
    <=> ? [Y0] : ra_Px3(X,Y0) ) )).

fof(ca_Cx3_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Cx3(B)
     <= ( A = B
        & ca_Cx3(A) ) ) )).

fof(rinvR_substitution_2,axiom,(
    ! [A,B,C] :
      ( rinvR(C,B)
     <= ( B = A
        & rinvR(C,A) ) ) )).

fof(axiom_1,axiom,(
    ! [X] :
      ( ~ xsd_integer(X)
    <=> xsd_string(X) ) )).

fof(rr_substitution_1,axiom,(
    ! [A,B,C] :
      ( ( A = B
        & rr(A,C) )
     => rr(B,C) ) )).

fof(axiom_9,axiom,(
    ! [X] :
      ( ca_Cx3(X)
    <=> ~ ? [Y] : ra_Px3(X,Y) ) )).

fof(cp1xcomp_substitution_1,axiom,(
    ! [A,B] :
      ( ( A = B
        & cp1xcomp(A) )
     => cp1xcomp(B) ) )).

fof(rf_substitution_1,axiom,(
    ! [A,B,C] :
      ( rf(B,C)
     <= ( B = A
        & rf(A,C) ) ) )).

fof(ca_Cx2_substitution_1,axiom,(
    ! [A,B] :
      ( ( B = A
        & ca_Cx2(A) )
     => ca_Cx2(B) ) )).

fof(ca_Cx3xcomp_substitution_1,axiom,(
    ! [A,B] :
      ( ( B = A
        & ca_Cx3xcomp(A) )
     => ca_Cx3xcomp(B) ) )).

fof(axiom_6,axiom,(
    ! [X] :
      ( ~ ? [Y] : ra_Px2(X,Y)
    <=> ca_Cx2(X) ) )).

fof(axiom_7,axiom,(
    ! [X] :
      ( ca_Cx2xcomp(X)
    <=> ( ca_Cx3(X)
        & cp1(X) ) ) )).

fof(ca_Cx2xcomp_substitution_1,axiom,(
    ! [A,B] :
      ( ( A = B
        & ca_Cx2xcomp(A) )
     => ca_Cx2xcomp(B) ) )).

fof(ra_Px1_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( B = A
        & ra_Px1(C,A) )
     => ra_Px1(C,B) ) )).

fof(axiom_13,axiom,(
    ! [X,Y,Z] :
      ( Z = Y
     <= ( rf(X,Z)
        & rf(X,Y) ) ) )).

fof(axiom_11,axiom,(
    ! [X] :
      ( ca_Cx3xcomp(X)
    <=> ! [Y] :
          ( cp1(Y)
         <= rr(X,Y) ) ) )).

fof(ra_Px2_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( ra_Px2(C,A)
        & B = A )
     => ra_Px2(C,B) ) )).

fof(axiom_0,axiom,(
    ! [X] :
      ( ~ cowlNothing(X)
      & cowlThing(X) ) )).

fof(ra_Px1_substitution_1,axiom,(
    ! [A,B,C] :
      ( ra_Px1(B,C)
     <= ( B = A
        & ra_Px1(A,C) ) ) )).

fof(axiom_12,axiom,(
    ! [X] :
      ( ? [Y] :
          ( rr(X,Y)
          & ca_Ax4(Y) )
    <=> ca_Vx5(X) ) )).

fof(rinvF_substitution_2,axiom,(
    ! [A,B,C] :
      ( rinvF(C,B)
     <= ( rinvF(C,A)
        & A = B ) ) )).

fof(axiom_5,axiom,(
    ! [X] :
      ( ca_Ax4(X)
    <=> ( ! [Y] :
            ( rinvR(X,Y)
           => ca_Cx2(Y) )
        & cp1(X) ) ) )).

fof(axiom_15,axiom,(
    ! [X,Y] :
      ( rinvR(X,Y)
    <=> rr(Y,X) ) )).

fof(rinvR_substitution_1,axiom,(
    ! [A,B,C] :
      ( rinvR(B,C)
     <= ( rinvR(A,C)
        & A = B ) ) )).

fof(ca_Vx5_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Vx5(B)
     <= ( B = A
        & ca_Vx5(A) ) ) )).

fof(axiom_17,axiom,(
    cSatisfiable(i2003_11_14_17_16_36130) )).

fof(cp1_substitution_1,axiom,(
    ! [A,B] :
      ( cp1(B)
     <= ( cp1(A)
        & B = A ) ) )).

fof(ra_Px2_substitution_1,axiom,(
    ! [A,B,C] :
      ( ( ra_Px2(A,C)
        & B = A )
     => ra_Px2(B,C) ) )).

fof(axiom_2,axiom,(
    ! [X] :
      ( ( ? [Y] :
            ( rr(X,Y)
            & ca_Vx5(Y) )
        & cp1(X) )
    <=> cSatisfiable(X) ) )).

fof(axiom_16,axiom,(
    ! [X,Y,Z] :
      ( ( rr(Y,Z)
        & rr(X,Y) )
     => rr(X,Z) ) )).

fof(cowlThing_substitution_1,axiom,(
    ! [A,B] :
      ( cowlThing(B)
     <= ( cowlThing(A)
        & B = A ) ) )).

fof(xsd_integer_substitution_1,axiom,(
    ! [A,B] :
      ( xsd_integer(B)
     <= ( A = B
        & xsd_integer(A) ) ) )).

fof(axiom_3,axiom,(
    ! [X] :
      ( ~ ? [Y] : ra_Px1(X,Y)
    <=> cp1(X) ) )).

fof(cowlNothing_substitution_1,axiom,(
    ! [A,B] :
      ( ( cowlNothing(A)
        & A = B )
     => cowlNothing(B) ) )).

fof(rinvF_substitution_1,axiom,(
    ! [A,B,C] :
      ( rinvF(B,C)
     <= ( rinvF(A,C)
        & B = A ) ) )).

fof(cSatisfiable_substitution_1,axiom,(
    ! [A,B] :
      ( ( cSatisfiable(A)
        & B = A )
     => cSatisfiable(B) ) )).

fof(ra_Px3_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( A = B
        & ra_Px3(C,A) )
     => ra_Px3(C,B) ) )).

fof(axiom_4,axiom,(
    ! [X] :
      ( ? [Y0] : ra_Px1(X,Y0)
    <=> cp1xcomp(X) ) )).

