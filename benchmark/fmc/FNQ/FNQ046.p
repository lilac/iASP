fof(mDivisor,definition,(
    ! [W0] :
      ( aInteger0(W0)
     => ! [W1] :
          ( aDivisorOf0(W1,W0)
        <=> ( aInteger0(W1)
            & W1 != sz00
            & ? [W2] :
                ( aInteger0(W2)
                & sdtasdt0(W1,W2) = W0 ) ) ) ) )).

fof(mEquMod,definition,(
    ! [W0,W1,W2] :
      ( ( aInteger0(W0)
        & aInteger0(W1)
        & aInteger0(W2)
        & W2 != sz00 )
     => ( sdteqdtlpzmzozddtrp0(W0,W1,W2)
      <=> aDivisorOf0(W2,sdtpldt0(W0,smndt0(W1))) ) ) )).

fof(mSubset,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
        <=> ( aSet0(W1)
            & ! [W2] :
                ( aElementOf0(W2,W1)
               => aElementOf0(W2,W0) ) ) ) ) )).

fof(mUnion,definition,(
    ! [W0,W1] :
      ( ( aSubsetOf0(W0,cS1395)
        & aSubsetOf0(W1,cS1395) )
     => ! [W2] :
          ( W2 = sdtbsmnsldt0(W0,W1)
        <=> ( aSet0(W2)
            & ! [W3] :
                ( aElementOf0(W3,W2)
              <=> ( aInteger0(W3)
                  & ( aElementOf0(W3,W0)
                    | aElementOf0(W3,W1) ) ) ) ) ) ) )).

fof(mIntersection,definition,(
    ! [W0,W1] :
      ( ( aSubsetOf0(W0,cS1395)
        & aSubsetOf0(W1,cS1395) )
     => ! [W2] :
          ( W2 = sdtslmnbsdt0(W0,W1)
        <=> ( aSet0(W2)
            & ! [W3] :
                ( aElementOf0(W3,W2)
              <=> ( aInteger0(W3)
                  & aElementOf0(W3,W0)
                  & aElementOf0(W3,W1) ) ) ) ) ) )).

fof(mUnionSet,definition,(
    ! [W0] :
      ( ( aSet0(W0)
        & ! [W1] :
            ( aElementOf0(W1,W0)
           => aSubsetOf0(W1,cS1395) ) )
     => ! [W1] :
          ( W1 = sbsmnsldt0(W0)
        <=> ( aSet0(W1)
            & ! [W2] :
                ( aElementOf0(W2,W1)
              <=> ( aInteger0(W2)
                  & ? [W3] :
                      ( aElementOf0(W3,W0)
                      & aElementOf0(W2,W3) ) ) ) ) ) ) )).

fof(mComplement,definition,(
    ! [W0] :
      ( aSubsetOf0(W0,cS1395)
     => ! [W1] :
          ( W1 = stldt0(W0)
        <=> ( aSet0(W1)
            & ! [W2] :
                ( aElementOf0(W2,W1)
              <=> ( aInteger0(W2)
                  & ~ aElementOf0(W2,W0) ) ) ) ) ) )).

fof(mArSeq,definition,(
    ! [W0,W1] :
      ( ( aInteger0(W0)
        & aInteger0(W1)
        & W1 != sz00 )
     => ! [W2] :
          ( W2 = szAzrzSzezqlpdtcmdtrp0(W0,W1)
        <=> ( aSet0(W2)
            & ! [W3] :
                ( aElementOf0(W3,W2)
              <=> ( aInteger0(W3)
                  & sdteqdtlpzmzozddtrp0(W3,W0,W1) ) ) ) ) ) )).

fof(mOpen,definition,(
    ! [W0] :
      ( aSubsetOf0(W0,cS1395)
     => ( isOpen0(W0)
      <=> ! [W1] :
            ( aElementOf0(W1,W0)
           => ? [W2] :
                ( aInteger0(W2)
                & W2 != sz00
                & aSubsetOf0(szAzrzSzezqlpdtcmdtrp0(W1,W2),W0) ) ) ) ) )).

fof(mClosed,definition,(
    ! [W0] :
      ( aSubsetOf0(W0,cS1395)
     => ( isClosed0(W0)
      <=> isOpen0(stldt0(W0)) ) ) )).

fof(mInterOpen,axiom,(
    ! [W0,W1] :
      ( isOpen0(sdtslmnbsdt0(W0,W1))
     <= ( aSubsetOf0(W0,cS1395)
        & isOpen0(W1)
        & isOpen0(W0)
        & aSubsetOf0(W1,cS1395) ) ) )).

fof(mSets,axiom,(
    ! [W0] :
      ( $true
     <= aSet0(W0) ) )).

fof(mAddAsso,axiom,(
    ! [W0,W1,W2] :
      ( sdtpldt0(W0,sdtpldt0(W1,W2)) = sdtpldt0(sdtpldt0(W0,W1),W2)
     <= ( aInteger0(W1)
        & aInteger0(W2)
        & aInteger0(W0) ) ) )).

fof(mIntNeg,axiom,(
    ! [W0] :
      ( aInteger0(smndt0(W0))
     <= aInteger0(W0) ) )).

fof(mIntPlus,axiom,(
    ! [W0,W1] :
      ( aInteger0(sdtpldt0(W0,W1))
     <= ( aInteger0(W0)
        & aInteger0(W1) ) ) )).

fof(mMulAsso,axiom,(
    ! [W0,W1,W2] :
      ( ( aInteger0(W0)
        & aInteger0(W2)
        & aInteger0(W1) )
     => sdtasdt0(W0,sdtasdt0(W1,W2)) = sdtasdt0(sdtasdt0(W0,W1),W2) ) )).

fof(mMulMinOne,axiom,(
    ! [W0] :
      ( ( smndt0(W0) = sdtasdt0(W0,smndt0(sz10))
        & sdtasdt0(smndt0(sz10),W0) = smndt0(W0) )
     <= aInteger0(W0) ) )).

fof(mPrime,axiom,(
    ! [W0] :
      ( ( isPrime0(W0)
       => $true )
     <= ( W0 != sz00
        & aInteger0(W0) ) ) )).

fof(mZeroDiv,axiom,(
    ! [W0,W1] :
      ( ( sdtasdt0(W0,W1) = sz00
       => ( W1 = sz00
          | sz00 = W0 ) )
     <= ( aInteger0(W0)
        & aInteger0(W1) ) ) )).

fof(mMulZero,axiom,(
    ! [W0] :
      ( ( sdtasdt0(sz00,W0) = sz00
        & sdtasdt0(W0,sz00) = sz00 )
     <= aInteger0(W0) ) )).

fof(mAddComm,axiom,(
    ! [W0,W1] :
      ( ( aInteger0(W1)
        & aInteger0(W0) )
     => sdtpldt0(W0,W1) = sdtpldt0(W1,W0) ) )).

fof(m__,conjecture,(
    isClosed0(sdtbsmnsldt0(xA,xB)) )).

fof(mElements,axiom,(
    ! [W0] :
      ( ! [W1] :
          ( $true
         <= aElementOf0(W1,W0) )
     <= aSet0(W0) ) )).

fof(mIntZero,axiom,(
    aInteger0(sz00) )).

fof(mDistrib,axiom,(
    ! [W0,W1,W2] :
      ( ( sdtasdt0(W0,sdtpldt0(W1,W2)) = sdtpldt0(sdtasdt0(W0,W1),sdtasdt0(W0,W2))
        & sdtasdt0(sdtpldt0(W0,W1),W2) = sdtpldt0(sdtasdt0(W0,W2),sdtasdt0(W1,W2)) )
     <= ( aInteger0(W2)
        & aInteger0(W1)
        & aInteger0(W0) ) ) )).

fof(mEquModMul,axiom,(
    ! [W0,W1,W2,W3] :
      ( ( ( sdteqdtlpzmzozddtrp0(W0,W1,W3)
          & sdteqdtlpzmzozddtrp0(W0,W1,W2) )
       <= sdteqdtlpzmzozddtrp0(W0,W1,sdtasdt0(W2,W3)) )
     <= ( sz00 != W2
        & W3 != sz00
        & aInteger0(W3)
        & aInteger0(W2)
        & aInteger0(W1)
        & aInteger0(W0) ) ) )).

fof(mIntOne,axiom,(
    aInteger0(sz10) )).

fof(mEquModSym,axiom,(
    ! [W0,W1,W2] :
      ( ( sdteqdtlpzmzozddtrp0(W0,W1,W2)
       => sdteqdtlpzmzozddtrp0(W1,W0,W2) )
     <= ( aInteger0(W2)
        & W2 != sz00
        & aInteger0(W1)
        & aInteger0(W0) ) ) )).

fof(mPrimeDivisor,axiom,(
    ! [W0] :
      ( aInteger0(W0)
     => ( ( W0 != sz10
          & W0 != smndt0(sz10) )
      <=> ? [W1] :
            ( isPrime0(W1)
            & aDivisorOf0(W1,W0) ) ) ) )).

fof(mIntMult,axiom,(
    ! [W0,W1] :
      ( ( aInteger0(W0)
        & aInteger0(W1) )
     => aInteger0(sdtasdt0(W0,W1)) ) )).

fof(mIntegers,axiom,(
    ! [W0] :
      ( $true
     <= aInteger0(W0) ) )).

fof(mAddZero,axiom,(
    ! [W0] :
      ( aInteger0(W0)
     => ( W0 = sdtpldt0(W0,sz00)
        & W0 = sdtpldt0(sz00,W0) ) ) )).

fof(mUnionOpen,axiom,(
    ! [W0] :
      ( ( ! [W1] :
            ( ( aSubsetOf0(W1,cS1395)
              & isOpen0(W1) )
           <= aElementOf0(W1,W0) )
        & aSet0(W0) )
     => isOpen0(sbsmnsldt0(W0)) ) )).

fof(mFinSet,axiom,(
    ! [W0] :
      ( ( isFinite0(W0)
       => $true )
     <= aSet0(W0) ) )).

fof(mEquModTrn,axiom,(
    ! [W0,W1,W2,W3] :
      ( ( sdteqdtlpzmzozddtrp0(W0,W3,W2)
       <= ( sdteqdtlpzmzozddtrp0(W1,W3,W2)
          & sdteqdtlpzmzozddtrp0(W0,W1,W2) ) )
     <= ( aInteger0(W2)
        & W2 != sz00
        & aInteger0(W3)
        & aInteger0(W1)
        & aInteger0(W0) ) ) )).

fof(m__1826,hypothesis,
    ( aSubsetOf0(xA,cS1395)
    & aSubsetOf0(xB,cS1395)
    & isClosed0(xB)
    & isClosed0(xA) )).

fof(mEquModRef,axiom,(
    ! [W0,W1] :
      ( ( aInteger0(W0)
        & W1 != sz00
        & aInteger0(W1) )
     => sdteqdtlpzmzozddtrp0(W0,W0,W1) ) )).

fof(mMulOne,axiom,(
    ! [W0] :
      ( ( sdtasdt0(sz10,W0) = W0
        & W0 = sdtasdt0(W0,sz10) )
     <= aInteger0(W0) ) )).

fof(mMulComm,axiom,(
    ! [W0,W1] :
      ( sdtasdt0(W0,W1) = sdtasdt0(W1,W0)
     <= ( aInteger0(W1)
        & aInteger0(W0) ) ) )).

fof(mAddNeg,axiom,(
    ! [W0] :
      ( ( sz00 = sdtpldt0(smndt0(W0),W0)
        & sz00 = sdtpldt0(W0,smndt0(W0)) )
     <= aInteger0(W0) ) )).

