fof(mDefEmpty,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ( isEmpty0(W0)
      <=> ~ ? [W1] : aElementOf0(W1,W0) ) ) )).

fof(mDefSub,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
        <=> ( aSet0(W1)
            & ! [W2] :
                ( aElementOf0(W2,W1)
               => aElementOf0(W2,W0) ) ) ) ) )).

fof(mDefLB,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2] :
              ( aLowerBoundOfIn0(W2,W1,W0)
            <=> ( aElementOf0(W2,W0)
                & ! [W3] :
                    ( aElementOf0(W3,W1)
                   => sdtlseqdt0(W2,W3) ) ) ) ) ) )).

fof(mDefUB,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2] :
              ( aUpperBoundOfIn0(W2,W1,W0)
            <=> ( aElementOf0(W2,W0)
                & ! [W3] :
                    ( aElementOf0(W3,W1)
                   => sdtlseqdt0(W3,W2) ) ) ) ) ) )).

fof(mDefInf,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2] :
              ( aInfimumOfIn0(W2,W1,W0)
            <=> ( aElementOf0(W2,W0)
                & aLowerBoundOfIn0(W2,W1,W0)
                & ! [W3] :
                    ( aLowerBoundOfIn0(W3,W1,W0)
                   => sdtlseqdt0(W3,W2) ) ) ) ) ) )).

fof(mDefSup,definition,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2] :
              ( aSupremumOfIn0(W2,W1,W0)
            <=> ( aElementOf0(W2,W0)
                & aUpperBoundOfIn0(W2,W1,W0)
                & ! [W3] :
                    ( aUpperBoundOfIn0(W3,W1,W0)
                   => sdtlseqdt0(W2,W3) ) ) ) ) ) )).

fof(mDefCLat,definition,(
    ! [W0] :
      ( aCompleteLattice0(W0)
    <=> ( aSet0(W0)
        & ! [W1] :
            ( aSubsetOf0(W1,W0)
           => ? [W2] :
                ( aInfimumOfIn0(W2,W1,W0)
                & ? [W3] : aSupremumOfIn0(W3,W1,W0) ) ) ) ) )).

fof(mDefDom,definition,(
    ! [W0,W1] :
      ( ( aFunction0(W0)
        & aSet0(W1) )
     => ( isOn0(W0,W1)
      <=> ( szDzozmdt0(W0) = szRzazndt0(W0)
          & szRzazndt0(W0) = W1 ) ) ) )).

fof(mDefFix,definition,(
    ! [W0] :
      ( aFunction0(W0)
     => ! [W1] :
          ( aFixedPointOf0(W1,W0)
        <=> ( aElementOf0(W1,szDzozmdt0(W0))
            & sdtlpdtrp0(W0,W1) = W1 ) ) ) )).

fof(mDefMonot,definition,(
    ! [W0] :
      ( aFunction0(W0)
     => ( isMonotone0(W0)
      <=> ! [W1,W2] :
            ( ( aElementOf0(W1,szDzozmdt0(W0))
              & aElementOf0(W2,szDzozmdt0(W0)) )
           => ( sdtlseqdt0(W1,W2)
             => sdtlseqdt0(sdtlpdtrp0(W0,W1),sdtlpdtrp0(W0,W2)) ) ) ) ) )).

fof(mEOfElem,axiom,(
    ! [W0] :
      ( ! [W1] :
          ( aElement0(W1)
         <= aElementOf0(W1,W0) )
     <= aSet0(W0) ) )).

fof(mConMap,axiom,(
    ! [W0] :
      ( $true
     <= aFunction0(W0) ) )).

fof(m__,conjecture,(
    aCompleteLattice0(xS) )).

fof(mDomSort,axiom,(
    ! [W0] :
      ( aSet0(szDzozmdt0(W0))
     <= aFunction0(W0) ) )).

fof(mLessRel,axiom,(
    ! [W0,W1] :
      ( ( aElement0(W1)
        & aElement0(W0) )
     => ( $true
       <= sdtlseqdt0(W0,W1) ) ) )).

fof(mASymm,axiom,(
    ! [W0,W1] :
      ( ( ( sdtlseqdt0(W1,W0)
          & sdtlseqdt0(W0,W1) )
       => W1 = W0 )
     <= ( aElement0(W1)
        & aElement0(W0) ) ) )).

fof(mElmSort,axiom,(
    ! [W0] :
      ( $true
     <= aElement0(W0) ) )).

fof(mImgSort,axiom,(
    ! [W0] :
      ( aFunction0(W0)
     => ! [W1] :
          ( aElementOf0(W1,szDzozmdt0(W0))
         => aElementOf0(sdtlpdtrp0(W0,W1),szRzazndt0(W0)) ) ) )).

fof(m__1123,hypothesis,
    ( aCompleteLattice0(xU)
    & isMonotone0(xf)
    & isOn0(xf,xU)
    & aFunction0(xf) )).

fof(mTrans,axiom,(
    ! [W0,W1,W2] :
      ( ( aElement0(W1)
        & aElement0(W2)
        & aElement0(W0) )
     => ( ( sdtlseqdt0(W0,W1)
          & sdtlseqdt0(W1,W2) )
       => sdtlseqdt0(W0,W2) ) ) )).

fof(mSetSort,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => $true ) )).

fof(mRanSort,axiom,(
    ! [W0] :
      ( aFunction0(W0)
     => aSet0(szRzazndt0(W0)) ) )).

fof(mARefl,axiom,(
    ! [W0] :
      ( sdtlseqdt0(W0,W0)
     <= aElement0(W0) ) )).

fof(mInfUn,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2,W3] :
              ( ( aInfimumOfIn0(W2,W1,W0)
                & aInfimumOfIn0(W3,W1,W0) )
             => W3 = W2 ) ) ) )).

fof(mSupUn,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( ! [W2,W3] :
              ( W3 = W2
             <= ( aSupremumOfIn0(W2,W1,W0)
                & aSupremumOfIn0(W3,W1,W0) ) )
         <= aSubsetOf0(W1,W0) ) ) )).

fof(m__1144,hypothesis,(
    xS = cS1142(xf) )).

