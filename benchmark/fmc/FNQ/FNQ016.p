fof(cSatisfiable_substitution_1,axiom,(
    ! [A,B] :
      ( cSatisfiable(B)
     <= ( cSatisfiable(A)
        & B = A ) ) )).

fof(ca_Cx3xcomp_substitution_1,axiom,(
    ! [A,B] :
      ( ( ca_Cx3xcomp(A)
        & A = B )
     => ca_Cx3xcomp(B) ) )).

fof(axiom_0,axiom,(
    ! [X] :
      ( cowlThing(X)
      & ~ cowlNothing(X) ) )).

fof(ra_Px2_substitution_1,axiom,(
    ! [A,B,C] :
      ( ( ra_Px2(A,C)
        & B = A )
     => ra_Px2(B,C) ) )).

fof(axiom_14,axiom,(
    ! [X,Y] :
      ( rinvF(X,Y)
    <=> rf(Y,X) ) )).

fof(axiom_17,axiom,(
    cSatisfiable(i2003_11_14_17_16_32989) )).

fof(ca_Cx2_substitution_1,axiom,(
    ! [A,B] :
      ( ( A = B
        & ca_Cx2(A) )
     => ca_Cx2(B) ) )).

fof(rinvF_substitution_1,axiom,(
    ! [A,B,C] :
      ( rinvF(B,C)
     <= ( rinvF(A,C)
        & A = B ) ) )).

fof(ra_Px3_substitution_1,axiom,(
    ! [A,B,C] :
      ( ra_Px3(B,C)
     <= ( A = B
        & ra_Px3(A,C) ) ) )).

fof(ca_Vx5_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Vx5(B)
     <= ( A = B
        & ca_Vx5(A) ) ) )).

fof(xsd_integer_substitution_1,axiom,(
    ! [A,B] :
      ( xsd_integer(B)
     <= ( xsd_integer(A)
        & A = B ) ) )).

fof(axiom_1,axiom,(
    ! [X] :
      ( ~ xsd_integer(X)
    <=> xsd_string(X) ) )).

fof(rinvR_substitution_1,axiom,(
    ! [A,B,C] :
      ( ( A = B
        & rinvR(A,C) )
     => rinvR(B,C) ) )).

fof(ca_Cx2xcomp_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Cx2xcomp(B)
     <= ( ca_Cx2xcomp(A)
        & B = A ) ) )).

fof(rr_substitution_1,axiom,(
    ! [A,B,C] :
      ( rr(B,C)
     <= ( rr(A,C)
        & A = B ) ) )).

fof(axiom_2,axiom,(
    ! [X] :
      ( cSatisfiable(X)
    <=> ( cp1(X)
        & ? [Y] :
            ( rr(X,Y)
            & ca_Vx5(Y) ) ) ) )).

fof(rf_substitution_1,axiom,(
    ! [A,B,C] :
      ( ( B = A
        & rf(A,C) )
     => rf(B,C) ) )).

fof(axiom_3,axiom,(
    ! [X] :
      ( cp1(X)
    <=> ~ ? [Y] : ra_Px1(X,Y) ) )).

fof(axiom_10,axiom,(
    ! [X] :
      ( ? [Y0] : ra_Px3(X,Y0)
    <=> ca_Cx3xcomp(X) ) )).

fof(axiom_4,axiom,(
    ! [X] :
      ( cp1xcomp(X)
    <=> ? [Y0] : ra_Px1(X,Y0) ) )).

fof(axiom_15,axiom,(
    ! [X,Y] :
      ( rinvR(X,Y)
    <=> rr(Y,X) ) )).

fof(ca_Ax4_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Ax4(B)
     <= ( ca_Ax4(A)
        & A = B ) ) )).

fof(axiom_11,axiom,(
    ! [X] :
      ( ca_Cx3xcomp(X)
    <=> ! [Y] :
          ( rr(X,Y)
         => cp1(Y) ) ) )).

fof(ra_Px1_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( ra_Px1(C,A)
        & B = A )
     => ra_Px1(C,B) ) )).

fof(ca_Cx3_substitution_1,axiom,(
    ! [A,B] :
      ( ca_Cx3(B)
     <= ( B = A
        & ca_Cx3(A) ) ) )).

fof(axiom_12,axiom,(
    ! [X] :
      ( ca_Vx5(X)
    <=> ? [Y] :
          ( ca_Ax4(Y)
          & rr(X,Y) ) ) )).

fof(rr_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( rr(C,A)
        & B = A )
     => rr(C,B) ) )).

fof(axiom_9,axiom,(
    ! [X] :
      ( ca_Cx3(X)
    <=> ~ ? [Y] : ra_Px3(X,Y) ) )).

fof(axiom_7,axiom,(
    ! [X] :
      ( ca_Cx2xcomp(X)
    <=> ( ca_Cx3(X)
        & cp1(X) ) ) )).

fof(axiom_13,axiom,(
    ! [X] :
      ( cowlThing(X)
     => ! [Y0,Y1] :
          ( Y0 = Y1
         <= ( rf(X,Y0)
            & rf(X,Y1) ) ) ) )).

fof(axiom_6,axiom,(
    ! [X] :
      ( ca_Cx2(X)
    <=> ~ ? [Y] : ra_Px2(X,Y) ) )).

fof(ra_Px2_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( A = B
        & ra_Px2(C,A) )
     => ra_Px2(C,B) ) )).

fof(cowlNothing_substitution_1,axiom,(
    ! [A,B] :
      ( ( cowlNothing(A)
        & A = B )
     => cowlNothing(B) ) )).

fof(axiom_5,axiom,(
    ! [X] :
      ( ca_Ax4(X)
    <=> ( cp1(X)
        & ! [Y] :
            ( ca_Cx2(Y)
           <= rinvR(X,Y) ) ) ) )).

fof(rinvF_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( A = B
        & rinvF(C,A) )
     => rinvF(C,B) ) )).

fof(rinvR_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( rinvR(C,A)
        & B = A )
     => rinvR(C,B) ) )).

fof(cowlThing_substitution_1,axiom,(
    ! [A,B] :
      ( cowlThing(B)
     <= ( cowlThing(A)
        & A = B ) ) )).

fof(cp1_substitution_1,axiom,(
    ! [A,B] :
      ( ( B = A
        & cp1(A) )
     => cp1(B) ) )).

fof(rf_substitution_2,axiom,(
    ! [A,B,C] :
      ( rf(C,B)
     <= ( B = A
        & rf(C,A) ) ) )).

fof(cp1xcomp_substitution_1,axiom,(
    ! [A,B] :
      ( cp1xcomp(B)
     <= ( A = B
        & cp1xcomp(A) ) ) )).

fof(axiom_16,axiom,(
    ! [X,Y,Z] :
      ( rr(X,Z)
     <= ( rr(X,Y)
        & rr(Y,Z) ) ) )).

fof(ra_Px3_substitution_2,axiom,(
    ! [A,B,C] :
      ( ( B = A
        & ra_Px3(C,A) )
     => ra_Px3(C,B) ) )).

fof(xsd_string_substitution_1,axiom,(
    ! [A,B] :
      ( ( B = A
        & xsd_string(A) )
     => xsd_string(B) ) )).

fof(ra_Px1_substitution_1,axiom,(
    ! [A,B,C] :
      ( ra_Px1(B,C)
     <= ( B = A
        & ra_Px1(A,C) ) ) )).

fof(axiom_8,axiom,(
    ! [X] :
      ( ? [Y0] : ra_Px2(X,Y0)
    <=> ca_Cx2xcomp(X) ) )).

