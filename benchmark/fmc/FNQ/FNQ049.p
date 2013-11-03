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

fof(mSetSort,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => $true ) )).

fof(mImgSort,axiom,(
    ! [W0] :
      ( ! [W1] :
          ( aElementOf0(W1,szDzozmdt0(W0))
         => aElementOf0(sdtlpdtrp0(W0,W1),szRzazndt0(W0)) )
     <= aFunction0(W0) ) )).

fof(m__1261,hypothesis,(
    aInfimumOfIn0(xp,xP,xU) )).

fof(mSupUn,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2,W3] :
              ( ( aSupremumOfIn0(W3,W1,W0)
                & aSupremumOfIn0(W2,W1,W0) )
             => W2 = W3 ) ) ) )).

fof(mElmSort,axiom,(
    ! [W0] :
      ( $true
     <= aElement0(W0) ) )).

fof(mTrans,axiom,(
    ! [W0,W1,W2] :
      ( ( ( sdtlseqdt0(W1,W2)
          & sdtlseqdt0(W0,W1) )
       => sdtlseqdt0(W0,W2) )
     <= ( aElement0(W2)
        & aElement0(W1)
        & aElement0(W0) ) ) )).

fof(m__,conjecture,
    ( aLowerBoundOfIn0(sdtlpdtrp0(xf,xp),xP,xU)
    & aUpperBoundOfIn0(sdtlpdtrp0(xf,xp),xT,xU) )).

fof(mASymm,axiom,(
    ! [W0,W1] :
      ( ( ( sdtlseqdt0(W0,W1)
          & sdtlseqdt0(W1,W0) )
       => W1 = W0 )
     <= ( aElement0(W0)
        & aElement0(W1) ) ) )).

fof(m__1144,hypothesis,(
    cS1142(xf) = xS )).

fof(mDomSort,axiom,(
    ! [W0] :
      ( aFunction0(W0)
     => aSet0(szDzozmdt0(W0)) ) )).

fof(mConMap,axiom,(
    ! [W0] :
      ( $true
     <= aFunction0(W0) ) )).

fof(mARefl,axiom,(
    ! [W0] :
      ( aElement0(W0)
     => sdtlseqdt0(W0,W0) ) )).

fof(mInfUn,axiom,(
    ! [W0] :
      ( aSet0(W0)
     => ! [W1] :
          ( aSubsetOf0(W1,W0)
         => ! [W2,W3] :
              ( ( aInfimumOfIn0(W2,W1,W0)
                & aInfimumOfIn0(W3,W1,W0) )
             => W3 = W2 ) ) ) )).

fof(mLessRel,axiom,(
    ! [W0,W1] :
      ( ( sdtlseqdt0(W0,W1)
       => $true )
     <= ( aElement0(W0)
        & aElement0(W1) ) ) )).

fof(m__1244,hypothesis,(
    xP = cS1241(xU,xf,xT) )).

fof(mRanSort,axiom,(
    ! [W0] :
      ( aSet0(szRzazndt0(W0))
     <= aFunction0(W0) ) )).

fof(m__1173,hypothesis,(
    aSubsetOf0(xT,xS) )).

fof(m__1123,hypothesis,
    ( aFunction0(xf)
    & isMonotone0(xf)
    & isOn0(xf,xU)
    & aCompleteLattice0(xU) )).

fof(mEOfElem,axiom,(
    ! [W0] :
      ( ! [W1] :
          ( aElementOf0(W1,W0)
         => aElement0(W1) )
     <= aSet0(W0) ) )).

