fof(mDefSSum,definition,(
    ! [W0,W1] :
      ( ( aSet0(W0)
        & aSet0(W1) )
     => ! [W2] :
          ( W2 = sdtpldt1(W0,W1)
        <=> ( aSet0(W2)
            & ! [W3] :
                ( aElementOf0(W3,W2)
              <=> ? [W4,W5] :
                    ( aElementOf0(W4,W0)
                    & aElementOf0(W5,W1)
                    & sdtpldt0(W4,W5) = W3 ) ) ) ) ) )).

fof(mDefSInt,definition,(
    ! [W0,W1] :
      ( ( aSet0(W0)
        & aSet0(W1) )
     => ! [W2] :
          ( W2 = sdtasasdt0(W0,W1)
        <=> ( aSet0(W2)
            & ! [W3] :
                ( aElementOf0(W3,W2)
              <=> ( aElementOf0(W3,W0)
                  & aElementOf0(W3,W1) ) ) ) ) ) )).

fof(mDefIdeal,definition,(
    ! [W0] :
      ( aIdeal0(W0)
    <=> ( aSet0(W0)
        & ! [W1] :
            ( aElementOf0(W1,W0)
           => ( ! [W2] :
                  ( aElementOf0(W2,W0)
                 => aElementOf0(sdtpldt0(W1,W2),W0) )
              & ! [W2] :
                  ( aElement0(W2)
                 => aElementOf0(sdtasdt0(W2,W1),W0) ) ) ) ) ) )).

fof(mDefMod,definition,(
    ! [W0,W1,W2] :
      ( ( aElement0(W0)
        & aElement0(W1)
        & aIdeal0(W2) )
     => ( sdteqdtlpzmzozddtrp0(W0,W1,W2)
      <=> aElementOf0(sdtpldt0(W0,smndt0(W1)),W2) ) ) )).

fof(mDefDiv,definition,(
    ! [W0,W1] :
      ( ( aElement0(W0)
        & aElement0(W1) )
     => ( doDivides0(W0,W1)
      <=> ? [W2] :
            ( aElement0(W2)
            & sdtasdt0(W0,W2) = W1 ) ) ) )).

fof(mDefDvs,definition,(
    ! [W0] :
      ( aElement0(W0)
     => ! [W1] :
          ( aDivisorOf0(W1,W0)
        <=> ( aElement0(W1)
            & doDivides0(W1,W0) ) ) ) )).

fof(mDefGCD,definition,(
    ! [W0,W1] :
      ( ( aElement0(W0)
        & aElement0(W1) )
     => ! [W2] :
          ( aGcdOfAnd0(W2,W0,W1)
        <=> ( aDivisorOf0(W2,W0)
            & aDivisorOf0(W2,W1)
            & ! [W3] :
                ( ( aDivisorOf0(W3,W0)
                  & aDivisorOf0(W3,W1) )
               => doDivides0(W3,W2) ) ) ) ) )).

fof(mDefRel,definition,(
    ! [W0,W1] :
      ( ( aElement0(W0)
        & aElement0(W1) )
     => ( misRelativelyPrime0(W0,W1)
      <=> aGcdOfAnd0(sz10,W0,W1) ) ) )).

fof(mDefPrIdeal,definition,(
    ! [W0] :
      ( aElement0(W0)
     => ! [W1] :
          ( W1 = slsdtgt0(W0)
        <=> ( aSet0(W1)
            & ! [W2] :
                ( aElementOf0(W2,W1)
              <=> ? [W3] :
                    ( aElement0(W3)
                    & sdtasdt0(W0,W3) = W2 ) ) ) ) ) )).

fof(mMulAsso,axiom,(
    ! [W0,W1,W2] :
      ( sdtasdt0(sdtasdt0(W0,W1),W2) = sdtasdt0(W0,sdtasdt0(W1,W2))
     <= ( aElement0(W1)
        & aElement0(W2)
        & aElement0(W0) ) ) )).

fof(mSortsB_02,axiom,(
    ! [W0,W1] :
      ( aElement0(sdtasdt0(W0,W1))
     <= ( aElement0(W1)
        & aElement0(W0) ) ) )).

fof(mAddInvr,axiom,(
    ! [W0] :
      ( aElement0(W0)
     => ( sdtpldt0(W0,smndt0(W0)) = sz00
        & sdtpldt0(smndt0(W0),W0) = sz00 ) ) )).

fof(m__2228,hypothesis,(
    ? [W0] :
      ( sz00 != W0
      & aElementOf0(W0,sdtpldt1(slsdtgt0(xa),slsdtgt0(xb))) ) )).

fof(mMulMnOne,axiom,(
    ! [W0] :
      ( aElement0(W0)
     => ( smndt0(W0) = sdtasdt0(smndt0(sz10),W0)
        & sdtasdt0(W0,smndt0(sz10)) = smndt0(W0) ) ) )).

fof(mCancel,axiom,(
    ! [W0,W1] :
      ( ( aElement0(W0)
        & aElement0(W1) )
     => ( sz00 = sdtasdt0(W0,W1)
       => ( sz00 = W0
          | W1 = sz00 ) ) ) )).

fof(m__,conjecture,(
    ? [W0] :
      ( aElementOf0(W0,xI)
      & sz00 != W0
      & ! [W1] :
          ( ( sz00 != W1
            & aElementOf0(W1,xI) )
         => ~ iLess0(sbrdtbr0(W1),sbrdtbr0(W0)) ) ) )).

fof(mEucSort,axiom,(
    ! [W0] :
      ( ( aElement0(W0)
        & W0 != sz00 )
     => aNaturalNumber0(sbrdtbr0(W0)) ) )).

fof(mAddComm,axiom,(
    ! [W0,W1] :
      ( sdtpldt0(W0,W1) = sdtpldt0(W1,W0)
     <= ( aElement0(W1)
        & aElement0(W0) ) ) )).

fof(mMulComm,axiom,(
    ! [W0,W1] :
      ( sdtasdt0(W1,W0) = sdtasdt0(W0,W1)
     <= ( aElement0(W0)
        & aElement0(W1) ) ) )).

fof(mIdeSum,axiom,(
    ! [W0,W1] :
      ( aIdeal0(sdtpldt1(W0,W1))
     <= ( aIdeal0(W1)
        & aIdeal0(W0) ) ) )).

fof(mNatLess,axiom,(
    ! [W0,W1] :
      ( ( aNaturalNumber0(W0)
        & aNaturalNumber0(W1) )
     => ( iLess0(W0,W1)
       => $true ) ) )).

fof(mUnNeZr,axiom,(
    sz00 != sz10 )).

fof(mNatSort,axiom,(
    ! [W0] :
      ( aNaturalNumber0(W0)
     => $true ) )).

fof(mAMDistr,axiom,(
    ! [W0,W1,W2] :
      ( ( aElement0(W2)
        & aElement0(W1)
        & aElement0(W0) )
     => ( sdtasdt0(W0,sdtpldt0(W1,W2)) = sdtpldt0(sdtasdt0(W0,W1),sdtasdt0(W0,W2))
        & sdtasdt0(sdtpldt0(W1,W2),W0) = sdtpldt0(sdtasdt0(W1,W0),sdtasdt0(W2,W0)) ) ) )).

fof(mSortsU,axiom,(
    ! [W0] :
      ( aElement0(W0)
     => aElement0(smndt0(W0)) ) )).

fof(mPrIdeal,axiom,(
    ! [W0] :
      ( aElement0(W0)
     => aIdeal0(slsdtgt0(W0)) ) )).

fof(m__2110,hypothesis,
    ( xb != sz00
    | sz00 != xa )).

fof(mDivision,axiom,(
    ! [W0,W1] :
      ( ? [W2,W3] :
          ( aElement0(W3)
          & W0 = sdtpldt0(sdtasdt0(W2,W1),W3)
          & ( iLess0(sbrdtbr0(W3),sbrdtbr0(W1))
           <= sz00 != W3 )
          & aElement0(W2) )
     <= ( aElement0(W1)
        & W1 != sz00
        & aElement0(W0) ) ) )).

fof(mElmSort,axiom,(
    ! [W0] :
      ( $true
     <= aElement0(W0) ) )).

fof(m__2174,hypothesis,
    ( xI = sdtpldt1(slsdtgt0(xa),slsdtgt0(xb))
    & aIdeal0(xI) )).

fof(mMulUnit,axiom,(
    ! [W0] :
      ( ( W0 = sdtasdt0(sz10,W0)
        & sdtasdt0(W0,sz10) = W0 )
     <= aElement0(W0) ) )).

fof(m__2203,hypothesis,
    ( aElementOf0(xa,slsdtgt0(xa))
    & aElementOf0(sz00,slsdtgt0(xb))
    & aElementOf0(xb,slsdtgt0(xb))
    & aElementOf0(sz00,slsdtgt0(xa)) )).

fof(mMulZero,axiom,(
    ! [W0] :
      ( ( sdtasdt0(sz00,W0) = sz00
        & sz00 = sdtasdt0(W0,sz00) )
     <= aElement0(W0) ) )).

fof(mSortsB,axiom,(
    ! [W0,W1] :
      ( aElement0(sdtpldt0(W0,W1))
     <= ( aElement0(W1)
        & aElement0(W0) ) ) )).

fof(mIdeInt,axiom,(
    ! [W0,W1] :
      ( aIdeal0(sdtasasdt0(W0,W1))
     <= ( aIdeal0(W0)
        & aIdeal0(W1) ) ) )).

fof(mSetEq,axiom,(
    ! [W0,W1] :
      ( ( ( ! [W2] :
              ( aElementOf0(W2,W0)
             <= aElementOf0(W2,W1) )
          & ! [W2] :
              ( aElementOf0(W2,W0)
             => aElementOf0(W2,W1) ) )
       => W0 = W1 )
     <= ( aSet0(W0)
        & aSet0(W1) ) ) )).

fof(mSortsC,axiom,(
    aElement0(sz00) )).

fof(mSortsC_01,axiom,(
    aElement0(sz10) )).

fof(m__2091,hypothesis,
    ( aElement0(xb)
    & aElement0(xa) )).

fof(m__2129,hypothesis,(
    aGcdOfAnd0(xc,xa,xb) )).

fof(mEOfElem,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aElementOf0(W1,W0)
         => aElement0(W1) ) ) )).

fof(mAddZero,axiom,(
    ! [W0] :
      ( ( sdtpldt0(sz00,W0) = W0
        & sdtpldt0(W0,sz00) = W0 )
     <= aElement0(W0) ) )).

fof(mChineseRemainder,axiom,(
    ! [W0,W1] :
      ( ( ! [W2] :
            ( aElement0(W2)
           => aElementOf0(W2,sdtpldt1(W0,W1)) )
       => ! [W2,W3] :
            ( ( aElement0(W3)
              & aElement0(W2) )
           => ? [W4] :
                ( sdteqdtlpzmzozddtrp0(W4,W2,W0)
                & sdteqdtlpzmzozddtrp0(W4,W3,W1)
                & aElement0(W4) ) ) )
     <= ( aIdeal0(W0)
        & aIdeal0(W1) ) ) )).

fof(mSetSort,axiom,(
    ! [W0] :
      ( $true
     <= aSet0(W0) ) )).

fof(mAddAsso,axiom,(
    ! [W0,W1,W2] :
      ( sdtpldt0(sdtpldt0(W0,W1),W2) = sdtpldt0(W0,sdtpldt0(W1,W2))
     <= ( aElement0(W0)
        & aElement0(W1)
        & aElement0(W2) ) ) )).

