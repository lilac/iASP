fof(axiom_770,axiom,(
    ! [X] :
      ~ ( iOnionTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_536,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iMargherita(X) ) )).

fof(axiom_297,axiom,(
    ! [X] :
      ( iSauceTopping(X)
     => iPizzaTopping(X) ) )).

fof(axiom_52,axiom,(
    ! [X] :
      ( iCapricciosa(X)
     => ? [Y] :
          ( iPeperonataTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_69,axiom,(
    ! [X] :
      ( iCheeseyPizza(X)
     => abstractDomain(X) ) )).

fof(axiom_567,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iVeneziana(X) ) )).

fof(axiom_357,axiom,(
    ! [X] :
      ( iTomatoTopping(X)
     => ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_550,axiom,(
    ! [X] :
      ~ ( iLeekTopping(X)
        & iPepperTopping(X) ) )).

fof(axiom_610,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iCajun(X) ) )).

fof(axiom_250,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPizzaBase(X) ) )).

fof(axiom_144,axiom,(
    ! [X] :
      ( iSpiciness(X)
     <= iHot(X) ) )).

fof(axiom_799,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_212,axiom,(
    ! [X] :
      ( iNutTopping(X)
     => ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) ) ) )).

fof(axiom_381,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iSultanaTopping(Y)
          & ihasTopping(X,Y) )
     <= iVeneziana(X) ) )).

fof(axiom_44,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iCajunSpiceTopping(X) ) )).

fof(axiom_351,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iThinAndCrispyPizza(X) ) )).

fof(axiom_408,axiom,(
    ! [X,Y] :
      ( iPizzaBase(X)
     <= iisBaseOf(X,Y) ) )).

fof(axiom_389,axiom,(
    ! [X,Y] :
      ( ihasBase(X,Y)
     => iPizzaBase(Y) ) )).

fof(axiom_828,axiom,(
    ! [X] :
      ~ ( iMushroomTopping(X)
        & iOliveTopping(X) ) )).

fof(axiom_571,axiom,(
    ! [X] :
      ~ ( iTomatoTopping(X)
        & iGarlicTopping(X) ) )).

fof(axiom_565,axiom,(
    ! [X] :
      ~ ( iLeekTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_22,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iPeperoniSausageTopping(Y) )
     <= iAmericanHot(X) ) )).

fof(axiom_738,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iParmense(X) ) )).

fof(axiom_163,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMushroomTopping(Y)
          & ihasTopping(X,Y) )
     <= iLaReine(X) ) )).

fof(axiom_149,axiom,(
    ! [X] :
      ( iHotSpicedBeefTopping(X)
     => iMeatTopping(X) ) )).

fof(axiom_449,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_492,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_710,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iCajun(X) ) )).

fof(axiom_636,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iRosa(X) ) )).

fof(axiom_704,axiom,(
    ! [X] :
      ~ ( iPrawnsTopping(X)
        & iMixedSeafoodTopping(X) ) )).

fof(axiom_720,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iSoho(X) ) )).

fof(axiom_111,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iFruttiDiMare(X) ) )).

fof(axiom_170,axiom,(
    ! [X] :
      ( iMargherita(X)
     => abstractDomain(X) ) )).

fof(axiom_742,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iAmerican(X) ) )).

fof(axiom_553,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_236,axiom,(
    ! [X] :
      ( iPeperonataTopping(X)
     => ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMedium(Y) ) ) )).

fof(axiom_323,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iSoho(X) ) )).

fof(axiom_605,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iParmense(X) ) )).

fof(axiom_223,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iParmense(X) ) )).

fof(axiom_613,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iLaReine(X) ) )).

fof(axiom_499,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_239,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMedium(Y) )
     <= iPeperoniSausageTopping(X) ) )).

fof(axiom_622,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iFiorentina(X) ) )).

fof(axiom_542,axiom,(
    ! [X] :
      ~ ( iFishTopping(X)
        & iCheeseTopping(X) ) )).

fof(axiom_457,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iFourSeasons(X) ) )).

fof(axiom_706,axiom,(
    ! [X] :
      ~ ( iHerbSpiceTopping(X)
        & iMeatTopping(X) ) )).

fof(axiom_746,axiom,(
    ! [X] :
      ~ ( iNutTopping(X)
        & iCheeseTopping(X) ) )).

fof(axiom_192,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iMushroom(X) ) )).

fof(axiom_428,axiom,(
    iowlThing(iGermany) )).

fof(axiom_291,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ( iGorgonzolaTopping(Y)
              | iTomatoTopping(Y)
              | iMozzarellaTopping(Y) )
           <= ihasTopping(X,Y) )
        & abstractDomain(X) )
     <= iRosa(X) ) )).

fof(axiom_422,axiom,(
    iowlThing(iAmerica) )).

fof(axiom_290,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iRosa(X) ) )).

fof(axiom_478,axiom,(
    ! [X] :
      ~ ( iGorgonzolaTopping(X)
        & iMozzarellaTopping(X) ) )).

fof(axiom_134,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iGorgonzolaTopping(X) ) )).

fof(axiom_671,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iSoho(X) ) )).

fof(axiom_234,axiom,(
    ! [X] :
      ( iPeperonataTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_252,axiom,(
    ! [X] :
      ( iPizzaTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_353,axiom,(
    ! [X] :
      ( iTobascoPepperSauce(X)
     => abstractDomain(X) ) )).

fof(axiom_135,axiom,(
    ! [X] :
      ( iGorgonzolaTopping(X)
     => iCheeseTopping(X) ) )).

fof(axiom_626,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iCapricciosa(X) ) )).

fof(axiom_48,axiom,(
    ! [X] :
      ( iCaperTopping(X)
     => ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_568,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iLeekTopping(X) ) )).

fof(axiom_651,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_101,axiom,(
    ! [X] :
      ( iFourSeasons(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iCaperTopping(Y) ) ) )).

fof(axiom_727,axiom,(
    ! [X] :
      ~ ( iPeperoniSausageTopping(X)
        & iChickenTopping(X) ) )).

fof(axiom_516,axiom,(
    ! [X] :
      ~ ( iVegetableTopping(X)
        & iMeatTopping(X) ) )).

fof(axiom_119,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMedium(Y) )
     <= iGarlicTopping(X) ) )).

fof(axiom_87,axiom,(
    ! [X] :
      ( iFiorentina(X)
     => iNamedPizza(X) ) )).

fof(axiom_7,axiom,(
    ! [X] : ~ iowlNothing(X) )).

fof(axiom_198,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     => abstractDomain(X) ) )).

fof(axiom_58,axiom,(
    ! [X] :
      ( iCapricciosa(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_456,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_106,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iPeperoniSausageTopping(Y)
          & ihasTopping(X,Y) )
     <= iFourSeasons(X) ) )).

fof(axiom_464,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iLaReine(X) ) )).

fof(axiom_572,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iMushroom(X) ) )).

fof(axiom_38,axiom,(
    ! [X] :
      ( iCajun(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) ) ) )).

fof(axiom_668,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_733,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_679,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iAmerican(X) ) )).

fof(axiom_690,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iCapricciosa(X) ) )).

fof(axiom_367,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iVegetarianPizzaEquivalent1(X) ) )).

fof(axiom_487,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iMushroom(X) ) )).

fof(axiom_467,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iFiorentina(X) ) )).

fof(axiom_785,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iPepperTopping(X) ) )).

fof(axiom_28,axiom,(
    ! [X] :
      ( iAnchoviesTopping(X)
     => iFishTopping(X) ) )).

fof(axiom_14,axiom,(
    ! [X] :
      ( iAmerican(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ( iPeperoniSausageTopping(Y)
              | iMozzarellaTopping(Y)
              | iTomatoTopping(Y) )
           <= ihasTopping(X,Y) ) ) ) )).

fof(axiom_244,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iPetitPoisTopping(X) ) )).

fof(axiom_161,axiom,(
    ! [X] :
      ( iLaReine(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iOliveTopping(Y) ) ) )).

fof(axiom_820,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iGiardiniera(X) ) )).

fof(axiom_412,axiom,(
    ! [X,Y,Z] :
      ( iisIngredientOf(X,Z)
     <= ( iisIngredientOf(Y,Z)
        & iisIngredientOf(X,Y) ) ) )).

fof(axiom_728,axiom,(
    ! [X] :
      ~ ( iJalapenoPepperTopping(X)
        & iSweetPepperTopping(X) ) )).

fof(axiom_475,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iSoho(X) ) )).

fof(axiom_703,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iTomatoTopping(X) ) )).

fof(axiom_249,axiom,(
    ! [X] :
      ( iFood(X)
     <= iPizza(X) ) )).

fof(axiom_116,axiom,(
    ! [X] :
      ( ( abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => ( iMixedSeafoodTopping(Y)
              | iTomatoTopping(Y)
              | iGarlicTopping(Y) ) ) )
     <= iFruttiDiMare(X) ) )).

fof(axiom_45,axiom,(
    ! [X] :
      ( iCajunSpiceTopping(X)
     => iHerbSpiceTopping(X) ) )).

fof(axiom_641,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iCajun(X) ) )).

fof(axiom_23,axiom,(
    ! [X] :
      ( ihasCountryOfOrigin(X,iAmerica)
     <= iAmericanHot(X) ) )).

fof(axiom_112,axiom,(
    ! [X] :
      ( iFruttiDiMare(X)
     => iNamedPizza(X) ) )).

fof(axiom_159,axiom,(
    ! [X] :
      ( iLaReine(X)
     => abstractDomain(X) ) )).

fof(axiom_520,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_333,axiom,(
    ! [X] :
      ( iSpicyPizzaEquivalent(X)
     => abstractDomain(X) ) )).

fof(axiom_699,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iMushroom(X) ) )).

fof(axiom_744,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iCaprina(X) ) )).

fof(axiom_208,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) )
     <= iNapoletana(X) ) )).

fof(axiom_678,axiom,(
    ! [X] :
      ~ ( iFruitTopping(X)
        & iVegetableTopping(X) ) )).

fof(axiom_426,axiom,(
    iowlThing(iFrance) )).

fof(axiom_493,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iCapricciosa(X) ) )).

fof(axiom_444,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iCajun(X) ) )).

fof(axiom_70,axiom,(
    ! [X] :
      ( ( iPizza(X)
        & ? [Y] :
            ( ihasTopping(X,Y)
            & iCheeseTopping(Y) ) )
    <=> iCheeseyPizza(X) ) )).

fof(axiom_472,axiom,(
    ! [X] :
      ~ ( iTomatoTopping(X)
        & iLeekTopping(X) ) )).

fof(axiom_503,axiom,(
    ! [X] :
      ~ ( iHot(X)
        & iMild(X) ) )).

fof(axiom_709,axiom,(
    ! [X] :
      ~ ( iOliveTopping(X)
        & iPepperTopping(X) ) )).

fof(axiom_73,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iCheeseyVegetableTopping(X) ) )).

fof(axiom_625,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iCajun(X) ) )).

fof(axiom_695,axiom,(
    ! [X] :
      ~ ( iGarlicTopping(X)
        & iArtichokeTopping(X) ) )).

fof(axiom_815,axiom,(
    ! [X] :
      ~ ( iFruitTopping(X)
        & iCheeseTopping(X) ) )).

fof(axiom_293,axiom,(
    ! [X] :
      ( iRosemaryTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_591,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iVeneziana(X) ) )).

fof(axiom_653,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iRosa(X) ) )).

fof(axiom_796,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_317,axiom,(
    ! [X] :
      ( iSloppyGiuseppe(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => ( iOnionTopping(Y)
              | iHotSpicedBeefTopping(Y)
              | iGreenPepperTopping(Y)
              | iTomatoTopping(Y)
              | iMozzarellaTopping(Y) ) ) ) ) )).

fof(axiom_782,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iFourSeasons(X) ) )).

fof(axiom_280,axiom,(
    ! [X] :
      ( iRealItalianPizza(X)
    <=> ( iPizza(X)
        & ihasCountryOfOrigin(X,iItaly) ) ) )).

fof(axiom_313,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) )
     <= iSloppyGiuseppe(X) ) )).

fof(axiom_570,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iAmerican(X) ) )).

fof(axiom_826,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iAmericanHot(X) ) )).

fof(axiom_430,axiom,(
    iowlThing(iItaly) )).

fof(axiom_51,axiom,(
    ! [X] :
      ( iCapricciosa(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ( iHamTopping(Y)
              | iAnchoviesTopping(Y)
              | iOliveTopping(Y)
              | iPeperonataTopping(Y)
              | iCaperTopping(Y)
              | iTomatoTopping(Y)
              | iMozzarellaTopping(Y) )
           <= ihasTopping(X,Y) ) ) ) )).

fof(axiom_747,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_737,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iParmense(X) ) )).

fof(axiom_138,axiom,(
    ! [X] :
      ( iGreenPepperTopping(X)
     => iPepperTopping(X) ) )).

fof(axiom_748,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iLaReine(X) ) )).

fof(axiom_524,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iCapricciosa(X) ) )).

fof(axiom_766,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iRosa(X) ) )).

fof(axiom_806,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_482,axiom,(
    ! [X] :
      ~ ( iNonVegetarianPizza(X)
        & iVegetarianPizza(X) ) )).

fof(axiom_580,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_321,axiom,(
    ! [X] :
      ( iSoho(X)
     => ? [Y] :
          ( iOliveTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_440,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_731,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iVeneziana(X) ) )).

fof(axiom_413,axiom,(
    ! [X,Y] :
      ( iisIngredientOf(X,Y)
     => iFood(X) ) )).

fof(axiom_539,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iLeekTopping(X) ) )).

fof(axiom_648,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iAmericanHot(X) ) )).

fof(axiom_2,axiom,(
    ? [X] : dataDomain(X) )).

fof(axiom_540,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iFiorentina(X) ) )).

fof(axiom_603,axiom,(
    ! [X] :
      ~ ( iMeatTopping(X)
        & iFishTopping(X) ) )).

fof(axiom_468,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iCapricciosa(X) ) )).

fof(axiom_669,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_544,axiom,(
    ! [X] :
      ~ ( iJalapenoPepperTopping(X)
        & iGreenPepperTopping(X) ) )).

fof(axiom_366,axiom,(
    ! [X] :
      ( ( abstractDomain(X)
        & ~ ? [Y] :
              ( iMeatTopping(Y)
              & ihasTopping(X,Y) )
        & abstractDomain(X)
        & ~ ? [Y] :
              ( iFishTopping(Y)
              & ihasTopping(X,Y) )
        & iPizza(X) )
    <=> iVegetarianPizza(X) ) )).

fof(axiom_633,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iCaprina(X) ) )).

fof(axiom_120,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iGiardiniera(X) ) )).

fof(axiom_817,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_77,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iCountry(X) ) )).

fof(axiom_403,axiom,(
    ! [X,Y] :
      ( iPizzaTopping(Y)
     <= ihasTopping(X,Y) ) )).

fof(axiom_560,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_348,axiom,(
    ! [X] :
      ( iPepperTopping(X)
     <= iSweetPepperTopping(X) ) )).

fof(axiom_662,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iCapricciosa(X) ) )).

fof(axiom_344,axiom,(
    ! [X] :
      ( iSundriedTomatoTopping(X)
     => ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) ) ) )).

fof(axiom_788,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iParmense(X) ) )).

fof(axiom_62,axiom,(
    ! [X] :
      ( iCaprina(X)
     => ? [Y] :
          ( iGoatsCheeseTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_85,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iParmesanTopping(Y) )
     <= iFiorentina(X) ) )).

fof(axiom_231,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iParmesanTopping(X) ) )).

fof(axiom_37,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iTobascoPepperSauce(Y)
          & ihasTopping(X,Y) )
     <= iCajun(X) ) )).

fof(axiom_131,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iGoatsCheeseTopping(X) ) )).

fof(axiom_207,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iAnchoviesTopping(Y)
          & ihasTopping(X,Y) )
     <= iNapoletana(X) ) )).

fof(axiom_141,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iHerbSpiceTopping(X) ) )).

fof(axiom_795,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iGiardiniera(X) ) )).

fof(axiom_342,axiom,(
    ! [X] :
      ( iSultanaTopping(X)
     => iFruitTopping(X) ) )).

fof(axiom_530,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_532,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iLaReine(X) ) )).

fof(axiom_0,axiom,(
    ! [X] :
      ( abstractDomain(X)
      | dataDomain(X) ) )).

fof(axiom_12,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) )
     <= iAmerican(X) ) )).

fof(axiom_179,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iMedium(X) ) )).

fof(axiom_88,axiom,(
    ! [X] :
      ( iFiorentina(X)
     => ? [Y] :
          ( iGarlicTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_477,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_205,axiom,(
    ! [X] :
      ( iNapoletana(X)
     => iNamedPizza(X) ) )).

fof(axiom_438,axiom,(
    ! [X] :
      ~ ( iJalapenoPepperTopping(X)
        & iPeperonataTopping(X) ) )).

fof(axiom_579,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iGiardiniera(X) ) )).

fof(axiom_195,axiom,(
    ! [X] :
      ( iMushroomTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_612,axiom,(
    ! [X] :
      ~ ( iAsparagusTopping(X)
        & iLeekTopping(X) ) )).

fof(axiom_30,axiom,(
    ! [X] :
      ( iArtichokeTopping(X)
     => ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_772,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iSoho(X) ) )).

fof(axiom_139,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iHamTopping(X) ) )).

fof(axiom_721,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iParmense(X) ) )).

fof(axiom_634,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iLaReine(X) ) )).

fof(axiom_597,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iSiciliana(X) ) )).

fof(axiom_273,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) )
     <= iPrinceCarlo(X) ) )).

fof(axiom_556,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iAmericanHot(X) ) )).

fof(axiom_647,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iRosa(X) ) )).

fof(axiom_608,axiom,(
    ! [X] :
      ~ ( iDomainConcept(X)
        & iValuePartition(X) ) )).

fof(axiom_371,axiom,(
    ! [X] :
      ( iVegetarianTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_78,axiom,(
    ! [X] :
      ( iCountry(X)
    <=> ( ( X = iGermany
          | iAmerica = X
          | iEngland = X
          | iItaly = X
          | iFrance = X )
        & iDomainConcept(X) ) ) )).

fof(axiom_55,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iHamTopping(Y)
          & ihasTopping(X,Y) )
     <= iCapricciosa(X) ) )).

fof(axiom_630,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iRosa(X) ) )).

fof(axiom_226,axiom,(
    ! [X] :
      ( iParmense(X)
     => ? [Y] :
          ( iHamTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_425,axiom,(
    iCountry(iFrance) )).

fof(axiom_227,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iParmense(X) ) )).

fof(axiom_676,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iNapoletana(X) ) )).

fof(axiom_93,axiom,(
    ! [X] :
      ( iFishTopping(X)
     => iPizzaTopping(X) ) )).

fof(axiom_331,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iSpicyPizza(X) ) )).

fof(axiom_50,axiom,(
    ! [X] :
      ( iCapricciosa(X)
     => abstractDomain(X) ) )).

fof(axiom_521,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iCaprina(X) ) )).

fof(axiom_588,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iSiciliana(X) ) )).

fof(axiom_105,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iOliveTopping(Y)
          & ihasTopping(X,Y) )
     <= iFourSeasons(X) ) )).

fof(axiom_64,axiom,(
    ! [X] :
      ( iCaprina(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => ( iGoatsCheeseTopping(Y)
              | iSundriedTomatoTopping(Y)
              | iMozzarellaTopping(Y)
              | iTomatoTopping(Y) ) ) ) ) )).

fof(axiom_237,axiom,(
    ! [X] :
      ( iPeperoniSausageTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_594,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_343,axiom,(
    ! [X] :
      ( iSundriedTomatoTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_84,axiom,(
    ! [X] :
      ( iFiorentina(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ( iParmesanTopping(Y)
              | iMozzarellaTopping(Y)
              | iTomatoTopping(Y)
              | iSpinachTopping(Y)
              | iGarlicTopping(Y)
              | iOliveTopping(Y) )
           <= ihasTopping(X,Y) ) ) ) )).

fof(axiom_286,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMedium(Y) )
     <= iRocketTopping(X) ) )).

fof(axiom_18,axiom,(
    ! [X] :
      ( iAmericanHot(X)
     => abstractDomain(X) ) )).

fof(axiom_262,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iChickenTopping(Y) ) ) )).

fof(axiom_535,axiom,(
    ! [X] :
      ~ ( iAsparagusTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_525,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_319,axiom,(
    ! [X] :
      ( iSoho(X)
     => abstractDomain(X) ) )).

fof(axiom_406,axiom,(
    ! [X,Y,Z] :
      ( Y = Z
     <= ( iisBaseOf(X,Z)
        & iisBaseOf(X,Y) ) ) )).

fof(axiom_378,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iOliveTopping(Y) ) ) )).

fof(axiom_363,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iVegetableTopping(X) ) )).

fof(axiom_538,axiom,(
    ! [X] :
      ~ ( iPeperonataTopping(X)
        & iSweetPepperTopping(X) ) )).

fof(axiom_680,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iGarlicTopping(X) ) )).

fof(axiom_451,axiom,(
    ! [X] :
      ~ ( iTomatoTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_74,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iChickenTopping(X) ) )).

fof(axiom_557,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iVeneziana(X) ) )).

fof(axiom_183,axiom,(
    ! [X] :
      ( iMixedSeafoodTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_345,axiom,(
    ! [X] :
      ( iTomatoTopping(X)
     <= iSundriedTomatoTopping(X) ) )).

fof(axiom_421,axiom,(
    iCountry(iAmerica) )).

fof(axiom_435,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iRosa(X) ) )).

fof(axiom_763,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iParmense(X) ) )).

fof(axiom_469,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_789,axiom,(
    ! [X] :
      ~ ( iFishTopping(X)
        & iSauceTopping(X) ) )).

fof(axiom_429,axiom,(
    iCountry(iItaly) )).

fof(axiom_488,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iSoho(X) ) )).

fof(axiom_642,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iParmense(X) ) )).

fof(axiom_315,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iOnionTopping(Y)
          & ihasTopping(X,Y) )
     <= iSloppyGiuseppe(X) ) )).

fof(axiom_266,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPrinceCarlo(X) ) )).

fof(axiom_734,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iGiardiniera(X) ) )).

fof(axiom_574,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_640,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_446,axiom,(
    ! [X] :
      ~ ( iOliveTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_670,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iParmense(X) ) )).

fof(axiom_743,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iLaReine(X) ) )).

fof(axiom_263,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => ? [Y] :
          ( iSweetPepperTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_509,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iFiorentina(X) ) )).

fof(axiom_251,axiom,(
    ! [X] :
      ( iPizzaBase(X)
     => iFood(X) ) )).

fof(axiom_722,axiom,(
    ! [X] :
      ~ ( iLeekTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_792,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iSoho(X) ) )).

fof(axiom_309,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iSlicedTomatoTopping(X) ) )).

fof(axiom_650,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iFourSeasons(X) ) )).

fof(axiom_607,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iRosa(X) ) )).

fof(axiom_108,axiom,(
    ! [X] :
      ( iFourSeasons(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_296,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iSauceTopping(X) ) )).

fof(axiom_314,axiom,(
    ! [X] :
      ( iSloppyGiuseppe(X)
     => iNamedPizza(X) ) )).

fof(axiom_147,axiom,(
    ! [X] :
      ( iHotGreenPepperTopping(X)
     => iGreenPepperTopping(X) ) )).

fof(axiom_682,axiom,(
    ! [X] :
      ~ ( iTomatoTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_169,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iLeekTopping(X) ) )).

fof(axiom_452,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_285,axiom,(
    ! [X] :
      ( iRocketTopping(X)
     => iVegetableTopping(X) ) )).

fof(axiom_168,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) )
     <= iLeekTopping(X) ) )).

fof(axiom_462,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iCapricciosa(X) ) )).

fof(axiom_794,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iMargherita(X) ) )).

fof(axiom_674,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_203,axiom,(
    ! [X] :
      ( iNapoletana(X)
     => ( ! [Y] :
            ( ( iAnchoviesTopping(Y)
              | iTomatoTopping(Y)
              | iCaperTopping(Y)
              | iMozzarellaTopping(Y)
              | iOliveTopping(Y) )
           <= ihasTopping(X,Y) )
        & abstractDomain(X) ) ) )).

fof(axiom_377,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iVeneziana(X) ) )).

fof(axiom_707,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iMushroom(X) ) )).

fof(axiom_761,axiom,(
    ! [X] :
      ~ ( iFruitTopping(X)
        & iHerbSpiceTopping(X) ) )).

fof(axiom_137,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iGreenPepperTopping(X) ) )).

fof(axiom_417,axiom,(
    ! [X,Y,Z] :
      ( ( iisToppingOf(X,Y)
        & iisToppingOf(X,Z) )
     => Z = Y ) )).

fof(axiom_3,axiom,(
    ! [X] :
      ~ ( dataDomain(X)
        & abstractDomain(X) ) )).

fof(axiom_511,axiom,(
    ! [X] :
      ~ ( iHotSpicedBeefTopping(X)
        & iChickenTopping(X) ) )).

fof(axiom_563,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iSiciliana(X) ) )).

fof(axiom_222,axiom,(
    ! [X] :
      ( iHamTopping(X)
     <= iParmaHamTopping(X) ) )).

fof(axiom_692,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_466,axiom,(
    ! [X] :
      ~ ( iOliveTopping(X)
        & iLeekTopping(X) ) )).

fof(axiom_254,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPolloAdAstra(X) ) )).

fof(axiom_42,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iPrawnsTopping(Y)
          & ihasTopping(X,Y) )
     <= iCajun(X) ) )).

fof(axiom_666,axiom,(
    ! [X] :
      ~ ( iSweetPepperTopping(X)
        & iGreenPepperTopping(X) ) )).

fof(axiom_225,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iParmesanTopping(Y) )
     <= iParmense(X) ) )).

fof(axiom_667,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iFiorentina(X) ) )).

fof(axiom_294,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iRosemaryTopping(X) ) )).

fof(axiom_504,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iLaReine(X) ) )).

fof(axiom_644,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_115,axiom,(
    ! [X] :
      ( iFruttiDiMare(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_218,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iOnionTopping(X) ) )).

fof(axiom_164,axiom,(
    ! [X] :
      ( iLaReine(X)
     => ? [Y] :
          ( iHamTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_832,axiom,(
    ! [X,Y] :
      ( ihasTopping(X,Y)
     => ihasIngredient(X,Y) ) )).

fof(axiom_172,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) )
     <= iMargherita(X) ) )).

fof(axiom_696,axiom,(
    ! [X] :
      ~ ( iSauceTopping(X)
        & iFruitTopping(X) ) )).

fof(axiom_576,axiom,(
    ! [X] :
      ~ ( iHerbSpiceTopping(X)
        & iFishTopping(X) ) )).

fof(axiom_473,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_180,axiom,(
    ! [X] :
      ( iMedium(X)
     => iSpiciness(X) ) )).

fof(axiom_453,axiom,(
    ! [X] :
      ~ ( iMushroomTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_129,axiom,(
    ! [X] :
      ( iGiardiniera(X)
     => ? [Y] :
          ( iLeekTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_777,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_56,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iCapricciosa(X) ) )).

fof(axiom_798,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iAmericanHot(X) ) )).

fof(axiom_63,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) )
     <= iCaprina(X) ) )).

fof(axiom_10,axiom,(
    ! [X] :
      ( ~ ( xsd_string(X)
          & xsd_integer(X) )
     <= dataDomain(X) ) )).

fof(axiom_270,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iRosemaryTopping(Y) )
     <= iPrinceCarlo(X) ) )).

fof(axiom_601,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iAmericanHot(X) ) )).

fof(axiom_541,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_174,axiom,(
    ! [X] :
      ( iMargherita(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_94,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iFood(X) ) )).

fof(axiom_575,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iCaprina(X) ) )).

fof(axiom_304,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iSiciliana(X) ) )).

fof(axiom_301,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iArtichokeTopping(Y) )
     <= iSiciliana(X) ) )).

fof(axiom_490,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iVeneziana(X) ) )).

fof(axiom_578,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iMargherita(X) ) )).

fof(axiom_672,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iOliveTopping(X) ) )).

fof(axiom_673,axiom,(
    ! [X] :
      ~ ( iOliveTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_143,axiom,(
    ! [X] :
      ( iHot(X)
     => abstractDomain(X) ) )).

fof(axiom_187,axiom,(
    ! [X] :
      ( iMozzarellaTopping(X)
     => iCheeseTopping(X) ) )).

fof(axiom_235,axiom,(
    ! [X] :
      ( iPepperTopping(X)
     <= iPeperonataTopping(X) ) )).

fof(axiom_238,axiom,(
    ! [X] :
      ( iMeatTopping(X)
     <= iPeperoniSausageTopping(X) ) )).

fof(axiom_708,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iSoho(X) ) )).

fof(axiom_445,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_813,axiom,(
    ! [X] :
      ~ ( iOnionTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_65,axiom,(
    ! [X] :
      ( iCaprina(X)
     => iNamedPizza(X) ) )).

fof(axiom_107,axiom,(
    ! [X] :
      ( iFourSeasons(X)
     => ? [Y] :
          ( iAnchoviesTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_685,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iAmericanHot(X) ) )).

fof(axiom_122,axiom,(
    ! [X] :
      ( iGiardiniera(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iPeperonataTopping(Y) ) ) )).

fof(axiom_281,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( iThinAndCrispyBase(Y)
           <= ihasBase(X,Y) )
        & abstractDomain(X) )
     <= iRealItalianPizza(X) ) )).

fof(axiom_480,axiom,(
    ! [X] :
      ~ ( iMushroomTopping(X)
        & iSpinachTopping(X) ) )).

fof(axiom_91,axiom,(
    ! [X] :
      ( iFishTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_182,axiom,(
    ! [X] :
      ( iMild(X)
     => iSpiciness(X) ) )).

fof(axiom_16,axiom,(
    ! [X] :
      ( ihasCountryOfOrigin(X,iAmerica)
     <= iAmerican(X) ) )).

fof(axiom_295,axiom,(
    ! [X] :
      ( iRosemaryTopping(X)
     => iHerbSpiceTopping(X) ) )).

fof(axiom_356,axiom,(
    ! [X] :
      ( iTomatoTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_89,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iSpinachTopping(Y) )
     <= iFiorentina(X) ) )).

fof(axiom_43,axiom,(
    ! [X] :
      ( iCajun(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_443,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iGiardiniera(X) ) )).

fof(axiom_779,axiom,(
    ! [X] :
      ~ ( iPizza(X)
        & iPizzaTopping(X) ) )).

fof(axiom_759,axiom,(
    ! [X] :
      ~ ( iAsparagusTopping(X)
        & iTomatoTopping(X) ) )).

fof(axiom_730,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_582,axiom,(
    ! [X] :
      ~ ( iAnchoviesTopping(X)
        & iPrawnsTopping(X) ) )).

fof(axiom_402,axiom,(
    ! [X,Y] :
      ( iPizza(X)
     <= ihasTopping(X,Y) ) )).

fof(axiom_455,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iGiardiniera(X) ) )).

fof(axiom_764,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iMushroom(X) ) )).

fof(axiom_376,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) )
     <= iVeneziana(X) ) )).

fof(axiom_243,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iPetitPoisTopping(X) ) )).

fof(axiom_506,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iMargherita(X) ) )).

fof(axiom_4,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iowlThing(X) ) )).

fof(axiom_615,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iAmericanHot(X) ) )).

fof(axiom_800,axiom,(
    ! [X] :
      ~ ( iPeperonataTopping(X)
        & iGreenPepperTopping(X) ) )).

fof(axiom_54,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) )
     <= iCapricciosa(X) ) )).

fof(axiom_41,axiom,(
    ! [X] :
      ( iCajun(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iOnionTopping(Y) ) ) )).

fof(axiom_329,axiom,(
    ! [X] :
      ( iSpiciness(X)
    <=> ( iHot(X)
        | iMild(X)
        | iMedium(X) ) ) )).

fof(axiom_410,axiom,(
    ! [X,Y] :
      ( iisBaseOf(X,Y)
    <=> ihasBase(Y,X) ) )).

fof(axiom_824,axiom,(
    ! [X] :
      ~ ( iFourCheesesTopping(X)
        & iParmesanTopping(X) ) )).

fof(axiom_26,axiom,(
    ! [X] :
      ( iAmericanHot(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iJalapenoPepperTopping(Y) ) ) )).

fof(axiom_600,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iMargherita(X) ) )).

fof(axiom_751,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iPepperTopping(X) ) )).

fof(axiom_694,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iMargherita(X) ) )).

fof(axiom_71,axiom,(
    ! [X] :
      ( iCheeseyVegetableTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_617,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iSiciliana(X) ) )).

fof(axiom_561,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_96,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iFourCheesesTopping(X) ) )).

fof(axiom_336,axiom,(
    ! [X] :
      ( ( iPizzaTopping(X)
        & ? [Y] :
            ( ihasSpiciness(X,Y)
            & iHot(Y) ) )
    <=> iSpicyTopping(X) ) )).

fof(axiom_665,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iGiardiniera(X) ) )).

fof(axiom_753,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_762,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iAmerican(X) ) )).

fof(axiom_368,axiom,(
    ! [X] :
      ( ( iPizza(X)
        & abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => iVegetarianTopping(Y) ) )
    <=> iVegetarianPizzaEquivalent1(X) ) )).

fof(axiom_171,axiom,(
    ! [X] :
      ( ( abstractDomain(X)
        & ! [Y] :
            ( ( iTomatoTopping(Y)
              | iMozzarellaTopping(Y) )
           <= ihasTopping(X,Y) ) )
     <= iMargherita(X) ) )).

fof(axiom_431,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iCapricciosa(X) ) )).

fof(axiom_739,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iRocketTopping(X) ) )).

fof(axiom_232,axiom,(
    ! [X] :
      ( iParmesanTopping(X)
     => ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_595,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iCajun(X) ) )).

fof(axiom_157,axiom,(
    ! [X] :
      ( iJalapenoPepperTopping(X)
     => ? [Y] :
          ( ihasSpiciness(X,Y)
          & iHot(Y) ) ) )).

fof(axiom_20,axiom,(
    ! [X] :
      ( iAmericanHot(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) ) ) )).

fof(axiom_479,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iVeneziana(X) ) )).

fof(axiom_396,axiom,(
    ! [X,Y] :
      ( iisIngredientOf(Y,X)
    <=> ihasIngredient(X,Y) ) )).

fof(axiom_655,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_773,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iLaReine(X) ) )).

fof(axiom_409,axiom,(
    ! [X,Y] :
      ( iPizza(Y)
     <= iisBaseOf(X,Y) ) )).

fof(axiom_471,axiom,(
    ! [X] :
      ~ ( iMedium(X)
        & iMild(X) ) )).

fof(axiom_9,axiom,(
    ! [X] :
      ( dataDomain(X)
     <= xsd_integer(X) ) )).

fof(axiom_80,axiom,(
    ! [X] :
      ( iPizzaBase(X)
     <= iDeepPanBase(X) ) )).

fof(axiom_66,axiom,(
    ! [X] :
      ( iCaprina(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_427,axiom,(
    iCountry(iGermany) )).

fof(axiom_307,axiom,(
    ! [X] :
      ( iSiciliana(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iAnchoviesTopping(Y) ) ) )).

fof(axiom_513,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_596,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iParmense(X) ) )).

fof(axiom_328,axiom,(
    ! [X] :
      ( iSpiciness(X)
     => abstractDomain(X) ) )).

fof(axiom_485,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iCaprina(X) ) )).

fof(axiom_186,axiom,(
    ! [X] :
      ( iMozzarellaTopping(X)
     => ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) ) ) )).

fof(axiom_241,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iPepperTopping(X) ) )).

fof(axiom_283,axiom,(
    ! [X] :
      ( iOnionTopping(X)
     <= iRedOnionTopping(X) ) )).

fof(axiom_312,axiom,(
    ! [X] :
      ( iSloppyGiuseppe(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iGreenPepperTopping(Y) ) ) )).

fof(axiom_379,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ihasCountryOfOrigin(X,iItaly) ) )).

fof(axiom_551,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_522,axiom,(
    ! [X] :
      ~ ( iCaperTopping(X)
        & iSpinachTopping(X) ) )).

fof(axiom_825,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iSiciliana(X) ) )).

fof(axiom_725,axiom,(
    ! [X] :
      ~ ( iMozzarellaTopping(X)
        & iParmesanTopping(X) ) )).

fof(axiom_700,axiom,(
    ! [X] :
      ~ ( iMozzarellaTopping(X)
        & iFourCheesesTopping(X) ) )).

fof(axiom_276,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iQuattroFormaggi(X) ) )).

fof(axiom_502,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iFourSeasons(X) ) )).

fof(axiom_104,axiom,(
    ! [X] :
      ( iFourSeasons(X)
     => iNamedPizza(X) ) )).

fof(axiom_303,axiom,(
    ! [X] :
      ( iSiciliana(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iOliveTopping(Y) ) ) )).

fof(axiom_271,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iLeekTopping(Y) )
     <= iPrinceCarlo(X) ) )).

fof(axiom_810,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iRosa(X) ) )).

fof(axiom_19,axiom,(
    ! [X] :
      ( iAmericanHot(X)
     => ( ! [Y] :
            ( ihasTopping(X,Y)
           => ( iPeperoniSausageTopping(Y)
              | iMozzarellaTopping(Y)
              | iHotGreenPepperTopping(Y)
              | iTomatoTopping(Y)
              | iJalapenoPepperTopping(Y) ) )
        & abstractDomain(X) ) ) )).

fof(axiom_369,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iVegetarianPizzaEquivalent2(X) ) )).

fof(axiom_35,axiom,(
    ! [X] :
      ( iCajun(X)
     => abstractDomain(X) ) )).

fof(axiom_326,axiom,(
    ! [X] :
      ( iSoho(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_373,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => abstractDomain(X) ) )).

fof(axiom_31,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iArtichokeTopping(X) ) )).

fof(axiom_224,axiom,(
    ! [X] :
      ( iParmense(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_305,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iGarlicTopping(Y)
          & ihasTopping(X,Y) )
     <= iSiciliana(X) ) )).

fof(axiom_68,axiom,(
    ! [X] :
      ( iCheeseTopping(X)
     => iPizzaTopping(X) ) )).

fof(axiom_21,axiom,(
    ! [X] :
      ( iAmericanHot(X)
     => iNamedPizza(X) ) )).

fof(axiom_133,axiom,(
    ! [X] :
      ( iCheeseTopping(X)
     <= iGoatsCheeseTopping(X) ) )).

fof(axiom_405,axiom,(
    ! [X,Y] :
      ( ( abstractDomain(X)
        & abstractDomain(Y) )
     <= iisBaseOf(X,Y) ) )).

fof(axiom_776,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iArtichokeTopping(X) ) )).

fof(axiom_433,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iVeneziana(X) ) )).

fof(axiom_778,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iVeneziana(X) ) )).

fof(axiom_118,axiom,(
    ! [X] :
      ( iGarlicTopping(X)
     => iVegetableTopping(X) ) )).

fof(axiom_162,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iLaReine(X) ) )).

fof(axiom_543,axiom,(
    ! [X] :
      ~ ( iHotSpicedBeefTopping(X)
        & iPeperoniSausageTopping(X) ) )).

fof(axiom_736,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iSiciliana(X) ) )).

fof(axiom_624,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iSiciliana(X) ) )).

fof(axiom_808,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iAmericanHot(X) ) )).

fof(axiom_418,axiom,(
    ! [X,Y] :
      ( iisToppingOf(X,Y)
     => iPizzaTopping(X) ) )).

fof(axiom_284,axiom,(
    ! [X] :
      ( iRocketTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_217,axiom,(
    ! [X] :
      ( iOnionTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_459,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iParmense(X) ) )).

fof(axiom_658,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iAmerican(X) ) )).

fof(axiom_247,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPizza(X) ) )).

fof(axiom_330,axiom,(
    ! [X] :
      ( iValuePartition(X)
     <= iSpiciness(X) ) )).

fof(axiom_340,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iSultanaTopping(X) ) )).

fof(axiom_716,axiom,(
    ! [X] :
      ~ ( iDeepPanBase(X)
        & iThinAndCrispyBase(X) ) )).

fof(axiom_791,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iCajun(X) ) )).

fof(axiom_76,axiom,(
    ! [X] :
      ( iChickenTopping(X)
     => iMeatTopping(X) ) )).

fof(axiom_619,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iMargherita(X) ) )).

fof(axiom_155,axiom,(
    ! [X] :
      ( iInterestingPizza(X)
    <=> ( ? [Y0,Y1,Y2] :
            ( ihasTopping(X,Y0)
            & ihasTopping(X,Y2)
            & Y2 != Y0
            & Y1 != Y2
            & Y1 != Y0
            & ihasTopping(X,Y1) )
        & iPizza(X) ) ) )).

fof(axiom_510,axiom,(
    ! [X] :
      ~ ( iMushroomTopping(X)
        & iArtichokeTopping(X) ) )).

fof(axiom_537,axiom,(
    ! [X] :
      ~ ( iGarlicTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_165,axiom,(
    ! [X] :
      ( iLaReine(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ( iOliveTopping(Y)
              | iMushroomTopping(Y)
              | iTomatoTopping(Y)
              | iMozzarellaTopping(Y)
              | iHamTopping(Y) )
           <= ihasTopping(X,Y) ) ) ) )).

fof(axiom_245,axiom,(
    ! [X] :
      ( iPineKernels(X)
     => abstractDomain(X) ) )).

fof(axiom_635,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iMargherita(X) ) )).

fof(axiom_508,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iNapoletana(X) ) )).

fof(axiom_98,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) )
     <= iFourCheesesTopping(X) ) )).

fof(axiom_178,axiom,(
    ! [X] :
      ( ( ? [Y] :
            ( ihasTopping(X,Y)
            & iMeatTopping(Y) )
        & iPizza(X) )
    <=> iMeatyPizza(X) ) )).

fof(axiom_341,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMedium(Y) )
     <= iSultanaTopping(X) ) )).

fof(axiom_320,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) )
     <= iSoho(X) ) )).

fof(axiom_632,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_173,axiom,(
    ! [X] :
      ( iMargherita(X)
     => iNamedPizza(X) ) )).

fof(axiom_548,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_53,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iCaperTopping(Y)
          & ihasTopping(X,Y) )
     <= iCapricciosa(X) ) )).

fof(axiom_566,axiom,(
    ! [X] :
      ~ ( iSundriedTomatoTopping(X)
        & iSlicedTomatoTopping(X) ) )).

fof(axiom_529,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_127,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMushroomTopping(Y)
          & ihasTopping(X,Y) )
     <= iGiardiniera(X) ) )).

fof(axiom_702,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iNapoletana(X) ) )).

fof(axiom_221,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) )
     <= iParmaHamTopping(X) ) )).

fof(axiom_24,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iHotGreenPepperTopping(Y) )
     <= iAmericanHot(X) ) )).

fof(axiom_267,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) )
     <= iPrinceCarlo(X) ) )).

fof(axiom_494,axiom,(
    ! [X] :
      ~ ( iTomatoTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_390,axiom,(
    ! [X,Y] :
      ( ihasBase(X,Y)
    <=> iisBaseOf(Y,X) ) )).

fof(axiom_577,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iParmense(X) ) )).

fof(axiom_278,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) )
     <= iQuattroFormaggi(X) ) )).

fof(axiom_257,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => iNamedPizza(X) ) )).

fof(axiom_306,axiom,(
    ! [X] :
      ( iSiciliana(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_498,axiom,(
    ! [X] :
      ~ ( iVegetableTopping(X)
        & iHerbSpiceTopping(X) ) )).

fof(axiom_497,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iSiciliana(X) ) )).

fof(axiom_802,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iAmerican(X) ) )).

fof(axiom_620,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iAmericanHot(X) ) )).

fof(axiom_308,axiom,(
    ! [X] :
      ( iSlicedTomatoTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_496,axiom,(
    ! [X] :
      ~ ( iFourCheesesTopping(X)
        & iGorgonzolaTopping(X) ) )).

fof(axiom_255,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) ) ) )).

fof(axiom_394,axiom,(
    ! [X,Y] :
      ( iFood(X)
     <= ihasIngredient(X,Y) ) )).

fof(axiom_39,axiom,(
    ! [X] :
      ( iCajun(X)
     => iNamedPizza(X) ) )).

fof(axiom_175,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iMeatTopping(X) ) )).

fof(axiom_268,axiom,(
    ! [X] :
      ( iPrinceCarlo(X)
     => iNamedPizza(X) ) )).

fof(axiom_391,axiom,(
    ! [X,Y] :
      ( ( abstractDomain(X)
        & abstractDomain(Y) )
     <= ihasCountryOfOrigin(X,Y) ) )).

fof(axiom_592,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iMargherita(X) ) )).

fof(axiom_375,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ? [Y] :
          ( iCaperTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_166,axiom,(
    ! [X] :
      ( iLaReine(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_675,axiom,(
    ! [X] :
      ~ ( iCheeseTopping(X)
        & iMeatTopping(X) ) )).

fof(axiom_193,axiom,(
    ! [X] :
      ( iMushroom(X)
     => ? [Y] :
          ( iMushroomTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_47,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iCaperTopping(X) ) )).

fof(axiom_416,axiom,(
    ! [X,Y] :
      ( iisToppingOf(X,Y)
     => ( abstractDomain(Y)
        & abstractDomain(X) ) ) )).

fof(axiom_790,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iAmerican(X) ) )).

fof(axiom_454,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iCajun(X) ) )).

fof(axiom_749,axiom,(
    ! [X] :
      ~ ( iFishTopping(X)
        & iFruitTopping(X) ) )).

fof(axiom_447,axiom,(
    ! [X] :
      ~ ( iFruitTopping(X)
        & iMeatTopping(X) ) )).

fof(axiom_604,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_90,axiom,(
    ! [X] :
      ( iFiorentina(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_404,axiom,(
    ! [X,Y] :
      ( iisToppingOf(Y,X)
    <=> ihasTopping(X,Y) ) )).

fof(axiom_25,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) )
     <= iAmericanHot(X) ) )).

fof(axiom_754,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_545,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iGiardiniera(X) ) )).

fof(axiom_712,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iTomatoTopping(X) ) )).

fof(axiom_200,axiom,(
    ! [X] :
      ( iNapoletana(X)
     => abstractDomain(X) ) )).

fof(axiom_775,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_359,axiom,(
    ! [X] :
      ( iUnclosedPizza(X)
     => abstractDomain(X) ) )).

fof(axiom_621,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iMushroom(X) ) )).

fof(axiom_370,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ( iVegetableTopping(Y)
              | iFruitTopping(Y)
              | iSauceTopping(Y)
              | iHerbSpiceTopping(Y)
              | iNutTopping(Y)
              | iCheeseTopping(Y) )
           <= ihasTopping(X,Y) )
        & abstractDomain(X)
        & iPizza(X) )
    <=> iVegetarianPizzaEquivalent2(X) ) )).

fof(axiom_724,axiom,(
    ! [X] :
      ~ ( iPetitPoisTopping(X)
        & iTomatoTopping(X) ) )).

fof(axiom_786,axiom,(
    ! [X] :
      ~ ( iMushroomTopping(X)
        & iGarlicTopping(X) ) )).

fof(axiom_154,axiom,(
    ! [X] :
      ( iInterestingPizza(X)
     => abstractDomain(X) ) )).

fof(axiom_689,axiom,(
    ! [X] :
      ~ ( iLeekTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_780,axiom,(
    ! [X] :
      ~ ( iQuattroFormaggi(X)
        & iSiciliana(X) ) )).

fof(axiom_211,axiom,(
    ! [X] :
      ( iNutTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_99,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iFourSeasons(X) ) )).

fof(axiom_6,axiom,(
    ! [X] :
      ( abstractDomain(X)
     => iowlThing(X) ) )).

fof(axiom_130,axiom,(
    ! [X] :
      ( iGiardiniera(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_809,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iMargherita(X) ) )).

fof(axiom_219,axiom,(
    ! [X] :
      ( iOnionTopping(X)
     => ? [Y] :
          ( iMedium(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_627,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iSiciliana(X) ) )).

fof(axiom_769,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_61,axiom,(
    ! [X] :
      ( iCaprina(X)
     => ? [Y] :
          ( iSundriedTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_531,axiom,(
    ! [X] :
      ~ ( iSauceTopping(X)
        & iMeatTopping(X) ) )).

fof(axiom_349,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iThinAndCrispyBase(X) ) )).

fof(axiom_385,axiom,(
    ! [X,Y] :
      ( ( abstractDomain(Y)
        & abstractDomain(X) )
     <= ihasBase(X,Y) ) )).

fof(axiom_256,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => ? [Y] :
          ( iCajunSpiceTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_783,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iFourSeasons(X) ) )).

fof(axiom_814,axiom,(
    ! [X] :
      ~ ( iMeatTopping(X)
        & iNutTopping(X) ) )).

fof(axiom_623,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iNapoletana(X) ) )).

fof(axiom_823,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iFiorentina(X) ) )).

fof(axiom_528,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iParmense(X) ) )).

fof(axiom_638,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iCapricciosa(X) ) )).

fof(axiom_831,axiom,(
    ! [X,Y] :
      ( ihasIngredient(X,Y)
     <= ihasBase(X,Y) ) )).

fof(axiom_299,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ihasTopping(X,Y)
           => ( iHamTopping(Y)
              | iOliveTopping(Y)
              | iGarlicTopping(Y)
              | iTomatoTopping(Y)
              | iArtichokeTopping(Y)
              | iMozzarellaTopping(Y)
              | iAnchoviesTopping(Y) ) )
        & abstractDomain(X) )
     <= iSiciliana(X) ) )).

fof(axiom_156,axiom,(
    ! [X] :
      ( iJalapenoPepperTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_434,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iMushroom(X) ) )).

fof(axiom_399,axiom,(
    ! [X,Y] :
      ( iSpiciness(Y)
     <= ihasSpiciness(X,Y) ) )).

fof(axiom_698,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iOliveTopping(X) ) )).

fof(axiom_152,axiom,(
    ! [X] :
      ( iIceCream(X)
     => ? [Y] :
          ( iFruitTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_659,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iCajun(X) ) )).

fof(axiom_564,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iFourSeasons(X) ) )).

fof(axiom_726,axiom,(
    ! [X] :
      ~ ( iTomatoTopping(X)
        & iArtichokeTopping(X) ) )).

fof(axiom_202,axiom,(
    ! [X] :
      ( iNapoletana(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_300,axiom,(
    ! [X] :
      ( iSiciliana(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_167,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iLeekTopping(X) ) )).

fof(axiom_489,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iFourSeasons(X) ) )).

fof(axiom_691,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_242,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPetitPoisTopping(X) ) )).

fof(axiom_72,axiom,(
    ! [X] :
      ( iCheeseyVegetableTopping(X)
     => iCheeseTopping(X) ) )).

fof(axiom_804,axiom,(
    ! [X] :
      ~ ( iGorgonzolaTopping(X)
        & iGoatsCheeseTopping(X) ) )).

fof(axiom_420,axiom,(
    ! [X,Y] :
      ( iisToppingOf(X,Y)
    <=> ihasTopping(Y,X) ) )).

fof(axiom_585,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iCajun(X) ) )).

fof(axiom_474,axiom,(
    ! [X] :
      ~ ( iFishTopping(X)
        & iVegetableTopping(X) ) )).

fof(axiom_439,axiom,(
    ! [X] :
      ~ ( iPetitPoisTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_639,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iCapricciosa(X) ) )).

fof(axiom_681,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iTomatoTopping(X) ) )).

fof(axiom_424,axiom,(
    iowlThing(iEngland) )).

fof(axiom_311,axiom,(
    ! [X] :
      ( iSloppyGiuseppe(X)
     => abstractDomain(X) ) )).

fof(axiom_32,axiom,(
    ! [X] :
      ( iAsparagusTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_558,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_771,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_599,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iVeneziana(X) ) )).

fof(axiom_741,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iNapoletana(X) ) )).

fof(axiom_693,axiom,(
    ! [X] :
      ~ ( iGarlicTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_346,axiom,(
    ! [X] :
      ( iSweetPepperTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_214,axiom,(
    ! [X] :
      ( iOliveTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_611,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iVeneziana(X) ) )).

fof(axiom_547,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iLaReine(X) ) )).

fof(axiom_393,axiom,(
    ! [X,Y,Z] :
      ( ihasIngredient(X,Z)
     <= ( ihasIngredient(X,Y)
        & ihasIngredient(Y,Z) ) ) )).

fof(axiom_302,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iHamTopping(Y)
          & ihasTopping(X,Y) )
     <= iSiciliana(X) ) )).

fof(axiom_768,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iMargherita(X) ) )).

fof(axiom_322,axiom,(
    ! [X] :
      ( iSoho(X)
     => ? [Y] :
          ( iParmesanTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_338,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iSpinachTopping(X) ) )).

fof(axiom_79,axiom,(
    ! [X] :
      ( iDeepPanBase(X)
     => abstractDomain(X) ) )).

fof(axiom_507,axiom,(
    ! [X] :
      ~ ( iPetitPoisTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_546,axiom,(
    ! [X] :
      ~ ( iPetitPoisTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_5,axiom,(
    ! [X] :
      ( iowlNothing(X)
     => abstractDomain(X) ) )).

fof(axiom_787,axiom,(
    ! [X] :
      ~ ( iNutTopping(X)
        & iHerbSpiceTopping(X) ) )).

fof(axiom_177,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iMeatyPizza(X) ) )).

fof(axiom_277,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ( iTomatoTopping(Y)
              | iFourCheesesTopping(Y) )
           <= ihasTopping(X,Y) )
        & abstractDomain(X) )
     <= iQuattroFormaggi(X) ) )).

fof(axiom_514,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iNapoletana(X) ) )).

fof(axiom_83,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) )
     <= iFiorentina(X) ) )).

fof(axiom_123,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iPetitPoisTopping(Y)
          & ihasTopping(X,Y) )
     <= iGiardiniera(X) ) )).

fof(axiom_661,axiom,(
    ! [X] :
      ~ ( iHotSpicedBeefTopping(X)
        & iHamTopping(X) ) )).

fof(axiom_656,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iFiorentina(X) ) )).

fof(axiom_436,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iCajun(X) ) )).

fof(axiom_199,axiom,(
    ! [X] :
      ( iPizza(X)
     <= iNamedPizza(X) ) )).

fof(axiom_324,axiom,(
    ! [X] :
      ( iSoho(X)
     => ? [Y] :
          ( iGarlicTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_361,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iUnclosedPizza(X) ) )).

fof(axiom_364,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     => iPizzaTopping(X) ) )).

fof(axiom_767,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iCaprina(X) ) )).

fof(axiom_158,axiom,(
    ! [X] :
      ( iJalapenoPepperTopping(X)
     => iPepperTopping(X) ) )).

fof(axiom_646,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_327,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iRocketTopping(Y) )
     <= iSoho(X) ) )).

fof(axiom_637,axiom,(
    ! [X] :
      ~ ( iCajunSpiceTopping(X)
        & iRosemaryTopping(X) ) )).

fof(axiom_196,axiom,(
    ! [X] :
      ( iMushroomTopping(X)
     => ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_801,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iMargherita(X) ) )).

fof(axiom_121,axiom,(
    ! [X] :
      ( ( abstractDomain(X)
        & ! [Y] :
            ( ( iPetitPoisTopping(Y)
              | iOliveTopping(Y)
              | iPeperonataTopping(Y)
              | iLeekTopping(Y)
              | iMushroomTopping(Y)
              | iTomatoTopping(Y)
              | iMozzarellaTopping(Y)
              | iSlicedTomatoTopping(Y) )
           <= ihasTopping(X,Y) ) )
     <= iGiardiniera(X) ) )).

fof(axiom_683,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_629,axiom,(
    ! [X] :
      ~ ( iGarlicTopping(X)
        & iRocketTopping(X) ) )).

fof(axiom_350,axiom,(
    ! [X] :
      ( iPizzaBase(X)
     <= iThinAndCrispyBase(X) ) )).

fof(axiom_128,axiom,(
    ! [X] :
      ( iGiardiniera(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iOliveTopping(Y) ) ) )).

fof(axiom_518,axiom,(
    ! [X] :
      ~ ( iOnionTopping(X)
        & iLeekTopping(X) ) )).

fof(axiom_414,axiom,(
    ! [X,Y] :
      ( iisIngredientOf(X,Y)
     => iFood(Y) ) )).

fof(axiom_829,axiom,(
    ! [X,Y] :
      ( iisIngredientOf(X,Y)
     <= iisToppingOf(X,Y) ) )).

fof(axiom_711,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iAmericanHot(X) ) )).

fof(axiom_153,axiom,(
    ! [X] :
      ( iFood(X)
     <= iIceCream(X) ) )).

fof(axiom_265,axiom,(
    ! [X] :
      ( iPrawnsTopping(X)
     => iFishTopping(X) ) )).

fof(axiom_148,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iHotSpicedBeefTopping(X) ) )).

fof(axiom_318,axiom,(
    ! [X] :
      ( iSloppyGiuseppe(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_631,axiom,(
    ! [X] :
      ~ ( iOnionTopping(X)
        & iGarlicTopping(X) ) )).

fof(axiom_562,axiom,(
    ! [X] :
      ~ ( iGoatsCheeseTopping(X)
        & iMozzarellaTopping(X) ) )).

fof(axiom_523,axiom,(
    ! [X] :
      ~ ( iParmesanTopping(X)
        & iGorgonzolaTopping(X) ) )).

fof(axiom_723,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iRosa(X) ) )).

fof(axiom_606,axiom,(
    ! [X] :
      ~ ( iVegetableTopping(X)
        & iNutTopping(X) ) )).

fof(axiom_461,axiom,(
    ! [X] :
      ~ ( iCheeseTopping(X)
        & iVegetableTopping(X) ) )).

fof(axiom_590,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iFourSeasons(X) ) )).

fof(axiom_384,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iowlThing(X) ) )).

fof(axiom_248,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iPizzaBase(Y)
          & ihasBase(X,Y) )
     <= iPizza(X) ) )).

fof(axiom_583,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_833,axiom,
    ( iFrance != iGermany
    & iAmerica != iFrance
    & iAmerica != iEngland
    & iAmerica != iGermany
    & iEngland != iGermany
    & iItaly != iAmerica
    & iItaly != iEngland
    & iGermany != iItaly
    & iEngland != iFrance
    & iItaly != iFrance )).

fof(axiom_411,axiom,(
    ! [X,Y] :
      ( ( abstractDomain(X)
        & abstractDomain(Y) )
     <= iisIngredientOf(X,Y) ) )).

fof(axiom_279,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iRealItalianPizza(X) ) )).

fof(axiom_586,axiom,(
    ! [X] :
      ~ ( iCaperTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_209,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iNonVegetarianPizza(X) ) )).

fof(axiom_40,axiom,(
    ! [X] :
      ( ( abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => ( iOnionTopping(Y)
              | iPrawnsTopping(Y)
              | iTobascoPepperSauce(Y)
              | iMozzarellaTopping(Y)
              | iTomatoTopping(Y)
              | iPeperonataTopping(Y) ) ) )
     <= iCajun(X) ) )).

fof(axiom_618,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iRosa(X) ) )).

fof(axiom_830,axiom,(
    ! [X,Y] :
      ( iisIngredientOf(X,Y)
     <= iisBaseOf(X,Y) ) )).

fof(axiom_201,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iCaperTopping(Y) )
     <= iNapoletana(X) ) )).

fof(axiom_355,axiom,(
    ! [X] :
      ( iTobascoPepperSauce(X)
     => iSauceTopping(X) ) )).

fof(axiom_382,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_581,axiom,(
    ! [X] :
      ~ ( iMixedSeafoodTopping(X)
        & iAnchoviesTopping(X) ) )).

fof(axiom_190,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ( iMozzarellaTopping(Y)
              | iTomatoTopping(Y)
              | iMushroomTopping(Y) )
           <= ihasTopping(X,Y) )
        & abstractDomain(X) )
     <= iMushroom(X) ) )).

fof(axiom_614,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iCajun(X) ) )).

fof(axiom_8,axiom,(
    ! [X] :
      ( xsd_string(X)
     => dataDomain(X) ) )).

fof(axiom_645,axiom,(
    ! [X] :
      ~ ( iCheeseTopping(X)
        & iHerbSpiceTopping(X) ) )).

fof(axiom_598,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iSiciliana(X) ) )).

fof(axiom_740,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_347,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) )
     <= iSweetPepperTopping(X) ) )).

fof(axiom_484,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iSoho(X) ) )).

fof(axiom_57,axiom,(
    ! [X] :
      ( iCapricciosa(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iOliveTopping(Y) ) ) )).

fof(axiom_102,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) )
     <= iFourSeasons(X) ) )).

fof(axiom_145,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iHotGreenPepperTopping(X) ) )).

fof(axiom_228,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iAsparagusTopping(Y)
          & ihasTopping(X,Y) )
     <= iParmense(X) ) )).

fof(axiom_124,axiom,(
    ! [X] :
      ( iGiardiniera(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_533,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iNapoletana(X) ) )).

fof(axiom_15,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iPeperoniSausageTopping(Y)
          & ihasTopping(X,Y) )
     <= iAmerican(X) ) )).

fof(axiom_125,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iSlicedTomatoTopping(Y)
          & ihasTopping(X,Y) )
     <= iGiardiniera(X) ) )).

fof(axiom_60,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iCaprina(X) ) )).

fof(axiom_476,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iCapricciosa(X) ) )).

fof(axiom_441,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_677,axiom,(
    ! [X] :
      ~ ( iGoatsCheeseTopping(X)
        & iFourCheesesTopping(X) ) )).

fof(axiom_146,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iHot(Y) )
     <= iHotGreenPepperTopping(X) ) )).

fof(axiom_465,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iMushroom(X) ) )).

fof(axiom_819,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_687,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iFiorentina(X) ) )).

fof(axiom_204,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iOliveTopping(Y) )
     <= iNapoletana(X) ) )).

fof(axiom_750,axiom,(
    ! [X] :
      ~ ( iAsparagusTopping(X)
        & iOliveTopping(X) ) )).

fof(axiom_185,axiom,(
    ! [X] :
      ( iMozzarellaTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_261,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) )
     <= iPolloAdAstra(X) ) )).

fof(axiom_92,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) )
     <= iFishTopping(X) ) )).

fof(axiom_181,axiom,(
    ! [X] :
      ( iMild(X)
     => abstractDomain(X) ) )).

fof(axiom_549,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iParmense(X) ) )).

fof(axiom_275,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iFourCheesesTopping(Y) )
     <= iQuattroFormaggi(X) ) )).

fof(axiom_213,axiom,(
    ! [X] :
      ( iPizzaTopping(X)
     <= iNutTopping(X) ) )).

fof(axiom_46,axiom,(
    ! [X] :
      ( iCajunSpiceTopping(X)
     => ? [Y] :
          ( ihasSpiciness(X,Y)
          & iHot(Y) ) ) )).

fof(axiom_663,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iFiorentina(X) ) )).

fof(axiom_233,axiom,(
    ! [X] :
      ( iParmesanTopping(X)
     => iCheeseTopping(X) ) )).

fof(axiom_95,axiom,(
    ! [X] :
      ( iFood(X)
     => iDomainConcept(X) ) )).

fof(axiom_505,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iCajun(X) ) )).

fof(axiom_298,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iSiciliana(X) ) )).

fof(axiom_649,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iAmericanHot(X) ) )).

fof(axiom_188,axiom,(
    ! [X] :
      ( ihasCountryOfOrigin(X,iItaly)
     <= iMozzarellaTopping(X) ) )).

fof(axiom_365,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iVegetarianPizza(X) ) )).

fof(axiom_609,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iUnclosedPizza(X) ) )).

fof(axiom_470,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iRosa(X) ) )).

fof(axiom_229,axiom,(
    ! [X] :
      ( iParmense(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ( iParmesanTopping(Y)
              | iHamTopping(Y)
              | iTomatoTopping(Y)
              | iAsparagusTopping(Y)
              | iMozzarellaTopping(Y) )
           <= ihasTopping(X,Y) ) ) ) )).

fof(axiom_160,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iMozzarellaTopping(Y) )
     <= iLaReine(X) ) )).

fof(axiom_288,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iGorgonzolaTopping(Y) )
     <= iRosa(X) ) )).

fof(axiom_392,axiom,(
    ! [X,Y] :
      ( ihasIngredient(X,Y)
     => ( abstractDomain(Y)
        & abstractDomain(X) ) ) )).

fof(axiom_415,axiom,(
    ! [X,Y] :
      ( iisIngredientOf(X,Y)
    <=> ihasIngredient(Y,X) ) )).

fof(axiom_652,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iCaprina(X) ) )).

fof(axiom_500,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iCaperTopping(X) ) )).

fof(axiom_818,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_797,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iFiorentina(X) ) )).

fof(axiom_807,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iArtichokeTopping(X) ) )).

fof(axiom_332,axiom,(
    ! [X] :
      ( ( ? [Y] :
            ( ihasTopping(X,Y)
            & iSpicyTopping(Y) )
        & iPizza(X) )
    <=> iSpicyPizza(X) ) )).

fof(axiom_757,axiom,(
    ! [X] :
      ~ ( iVegetableTopping(X)
        & iSauceTopping(X) ) )).

fof(axiom_352,axiom,(
    ! [X] :
      ( iThinAndCrispyPizza(X)
    <=> ( iPizza(X)
        & ! [Y] :
            ( ihasBase(X,Y)
           => iThinAndCrispyBase(Y) )
        & abstractDomain(X) ) ) )).

fof(axiom_515,axiom,(
    ! [X] :
      ~ ( iHerbSpiceTopping(X)
        & iSauceTopping(X) ) )).

fof(axiom_316,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iHotSpicedBeefTopping(Y)
          & ihasTopping(X,Y) )
     <= iSloppyGiuseppe(X) ) )).

fof(axiom_664,axiom,(
    ! [X] :
      ~ ( iCaperTopping(X)
        & iMushroomTopping(X) ) )).

fof(axiom_811,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iFourSeasons(X) ) )).

fof(axiom_81,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iDomainConcept(X) ) )).

fof(axiom_765,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iFiorentina(X) ) )).

fof(axiom_150,axiom,(
    ! [X] :
      ( iHotSpicedBeefTopping(X)
     => ? [Y] :
          ( iHot(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_821,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iPepperTopping(X) ) )).

fof(axiom_259,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => ? [Y] :
          ( iRedOnionTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_589,axiom,(
    ! [X] :
      ~ ( iVeneziana(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_657,axiom,(
    ! [X] :
      ~ ( iPizzaTopping(X)
        & iIceCream(X) ) )).

fof(axiom_519,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_527,axiom,(
    ! [X] :
      ~ ( iFruitTopping(X)
        & iNutTopping(X) ) )).

fof(axiom_36,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iPeperonataTopping(Y) )
     <= iCajun(X) ) )).

fof(axiom_354,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iHot(Y) )
     <= iTobascoPepperSauce(X) ) )).

fof(axiom_272,axiom,(
    ! [X] :
      ( ( abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => ( iParmesanTopping(Y)
              | iMozzarellaTopping(Y)
              | iTomatoTopping(Y)
              | iLeekTopping(Y)
              | iRosemaryTopping(Y) ) ) )
     <= iPrinceCarlo(X) ) )).

fof(axiom_398,axiom,(
    ! [X,Y,Z] :
      ( Z = Y
     <= ( ihasSpiciness(X,Z)
        & ihasSpiciness(X,Y) ) ) )).

fof(axiom_587,axiom,(
    ! [X] :
      ~ ( iHamTopping(X)
        & iChickenTopping(X) ) )).

fof(axiom_360,axiom,(
    ! [X] :
      ( iUnclosedPizza(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_460,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iRosa(X) ) )).

fof(axiom_569,axiom,(
    ! [X] :
      ~ ( iIceCream(X)
        & iPizza(X) ) )).

fof(axiom_491,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iNapoletana(X) ) )).

fof(axiom_97,axiom,(
    ! [X] :
      ( iCheeseTopping(X)
     <= iFourCheesesTopping(X) ) )).

fof(axiom_729,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iCapricciosa(X) ) )).

fof(axiom_684,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iMargherita(X) ) )).

fof(axiom_686,axiom,(
    ! [X] :
      ~ ( iCajun(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_140,axiom,(
    ! [X] :
      ( iMeatTopping(X)
     <= iHamTopping(X) ) )).

fof(axiom_534,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_194,axiom,(
    ! [X] :
      ( iMushroom(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_758,axiom,(
    ! [X] :
      ~ ( iSloppyGiuseppe(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_555,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iAmerican(X) ) )).

fof(axiom_100,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ihasTopping(X,Y)
           => ( iOliveTopping(Y)
              | iCaperTopping(Y)
              | iMushroomTopping(Y)
              | iTomatoTopping(Y)
              | iMozzarellaTopping(Y)
              | iAnchoviesTopping(Y)
              | iPeperoniSausageTopping(Y) ) )
        & abstractDomain(X) )
     <= iFourSeasons(X) ) )).

fof(axiom_334,axiom,(
    ! [X] :
      ( ( iPizza(X)
        & ? [Y] :
            ( ihasTopping(X,Y)
            & iPizzaTopping(Y)
            & ? [Z] :
                ( iHot(Z)
                & ihasSpiciness(Y,Z) ) ) )
    <=> iSpicyPizzaEquivalent(X) ) )).

fof(axiom_387,axiom,(
    ! [X,Y,Z] :
      ( ( ihasBase(Z,X)
        & ihasBase(Y,X) )
     => Z = Y ) )).

fof(axiom_383,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ( abstractDomain(X)
        & ! [Y] :
            ( ihasTopping(X,Y)
           => ( iPineKernels(Y)
              | iOliveTopping(Y)
              | iTomatoTopping(Y)
              | iCaperTopping(Y)
              | iMozzarellaTopping(Y)
              | iSultanaTopping(Y)
              | iOnionTopping(Y) ) ) ) ) )).

fof(axiom_784,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iFiorentina(X) ) )).

fof(axiom_793,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iMargherita(X) ) )).

fof(axiom_287,axiom,(
    ! [X] :
      ( iRosa(X)
     => abstractDomain(X) ) )).

fof(axiom_717,axiom,(
    ! [X] :
      ~ ( iGarlicTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_282,axiom,(
    ! [X] :
      ( iRedOnionTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_483,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iSiciliana(X) ) )).

fof(axiom_27,axiom,(
    ! [X] :
      ( iAnchoviesTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_732,axiom,(
    ! [X] :
      ~ ( iPizzaBase(X)
        & iPizza(X) ) )).

fof(axiom_584,axiom,(
    ! [X] :
      ~ ( iFourSeasons(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_29,axiom,(
    ! [X] :
      ( iArtichokeTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_774,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iAmerican(X) ) )).

fof(axiom_49,axiom,(
    ! [X] :
      ( iCaperTopping(X)
     => iVegetableTopping(X) ) )).

fof(axiom_701,axiom,(
    ! [X] :
      ~ ( iFishTopping(X)
        & iNutTopping(X) ) )).

fof(axiom_1,axiom,(
    ? [X] : abstractDomain(X) )).

fof(axiom_660,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iGarlicTopping(X) ) )).

fof(axiom_75,axiom,(
    ! [X] :
      ( iChickenTopping(X)
     => ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) ) ) )).

fof(axiom_215,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasSpiciness(X,Y)
          & iMild(Y) )
     <= iOliveTopping(X) ) )).

fof(axiom_517,axiom,(
    ! [X] :
      ~ ( iLeekTopping(X)
        & iRocketTopping(X) ) )).

fof(axiom_752,axiom,(
    ! [X] :
      ~ ( iParmense(X)
        & iFourSeasons(X) ) )).

fof(axiom_442,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iMushroom(X) ) )).

fof(axiom_386,axiom,(
    ! [X,Y,Z] :
      ( ( ihasBase(X,Y)
        & ihasBase(X,Z) )
     => Z = Y ) )).

fof(axiom_216,axiom,(
    ! [X] :
      ( iOliveTopping(X)
     => iVegetableTopping(X) ) )).

fof(axiom_450,axiom,(
    ! [X] :
      ~ ( iHamTopping(X)
        & iPeperoniSausageTopping(X) ) )).

fof(axiom_132,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iGoatsCheeseTopping(X) ) )).

fof(axiom_114,axiom,(
    ! [X] :
      ( ? [Y] :
          ( ihasTopping(X,Y)
          & iGarlicTopping(Y) )
     <= iFruttiDiMare(X) ) )).

fof(axiom_335,axiom,(
    ! [X] :
      ( iSpicyTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_253,axiom,(
    ! [X] :
      ( iFood(X)
     <= iPizzaTopping(X) ) )).

fof(axiom_374,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iPineKernels(Y) ) ) )).

fof(axiom_714,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_397,axiom,(
    ! [X,Y] :
      ( ihasSpiciness(X,Y)
     => ( abstractDomain(X)
        & abstractDomain(Y) ) ) )).

fof(axiom_34,axiom,(
    ! [X] :
      ( iAsparagusTopping(X)
     => iVegetableTopping(X) ) )).

fof(axiom_17,axiom,(
    ! [X] :
      ( iAmerican(X)
     => ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_448,axiom,(
    ! [X] :
      ~ ( iMushroom(X)
        & iAmerican(X) ) )).

fof(axiom_136,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iGorgonzolaTopping(X) ) )).

fof(axiom_113,axiom,(
    ! [X] :
      ( iFruttiDiMare(X)
     => ? [Y] :
          ( iMixedSeafoodTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_67,axiom,(
    ! [X] :
      ( iCheeseTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_400,axiom,(
    ! [X,Y] :
      ( ihasTopping(X,Y)
     => ( abstractDomain(Y)
        & abstractDomain(X) ) ) )).

fof(axiom_745,axiom,(
    ! [X] :
      ~ ( iSoho(X)
        & iCajun(X) ) )).

fof(axiom_13,axiom,(
    ! [X] :
      ( iNamedPizza(X)
     <= iAmerican(X) ) )).

fof(axiom_258,axiom,(
    ! [X] :
      ( ( ! [Y] :
            ( ihasTopping(X,Y)
           => ( iChickenTopping(Y)
              | iGarlicTopping(Y)
              | iTomatoTopping(Y)
              | iCajunSpiceTopping(Y)
              | iMozzarellaTopping(Y)
              | iRedOnionTopping(Y)
              | iSweetPepperTopping(Y) ) )
        & abstractDomain(X) )
     <= iPolloAdAstra(X) ) )).

fof(axiom_526,axiom,(
    ! [X] :
      ~ ( iAmericanHot(X)
        & iSoho(X) ) )).

fof(axiom_718,axiom,(
    ! [X] :
      ~ ( iPizzaBase(X)
        & iIceCream(X) ) )).

fof(axiom_573,axiom,(
    ! [X] :
      ~ ( iSiciliana(X)
        & iLaReine(X) ) )).

fof(axiom_654,axiom,(
    ! [X] :
      ~ ( iMedium(X)
        & iHot(X) ) )).

fof(axiom_512,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_719,axiom,(
    ! [X] :
      ~ ( iArtichokeTopping(X)
        & iOliveTopping(X) ) )).

fof(axiom_86,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iOliveTopping(Y)
          & ihasTopping(X,Y) )
     <= iFiorentina(X) ) )).

fof(axiom_325,axiom,(
    ! [X] :
      ( iSoho(X)
     => ( ! [Y] :
            ( ( iOliveTopping(Y)
              | iMozzarellaTopping(Y)
              | iTomatoTopping(Y)
              | iRocketTopping(Y)
              | iGarlicTopping(Y)
              | iParmesanTopping(Y) )
           <= ihasTopping(X,Y) )
        & abstractDomain(X) ) ) )).

fof(axiom_688,axiom,(
    ! [X] :
      ~ ( iFruttiDiMare(X)
        & iFourSeasons(X) ) )).

fof(axiom_310,axiom,(
    ! [X] :
      ( iSlicedTomatoTopping(X)
     => iTomatoTopping(X) ) )).

fof(axiom_486,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iQuattroFormaggi(X) ) )).

fof(axiom_812,axiom,(
    ! [X] :
      ~ ( iRocketTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_602,axiom,(
    ! [X] :
      ~ ( iAmerican(X)
        & iSiciliana(X) ) )).

fof(axiom_697,axiom,(
    ! [X] :
      ~ ( iFiorentina(X)
        & iFruttiDiMare(X) ) )).

fof(axiom_358,axiom,(
    ! [X] :
      ( iTomatoTopping(X)
     => iVegetableTopping(X) ) )).

fof(axiom_362,axiom,(
    ! [X] :
      ( iValuePartition(X)
     => abstractDomain(X) ) )).

fof(axiom_705,axiom,(
    ! [X] :
      ~ ( iParmesanTopping(X)
        & iGoatsCheeseTopping(X) ) )).

fof(axiom_432,axiom,(
    ! [X] :
      ~ ( iLeekTopping(X)
        & iGarlicTopping(X) ) )).

fof(axiom_643,axiom,(
    ! [X] :
      ~ ( iCapricciosa(X)
        & iAmerican(X) ) )).

fof(axiom_339,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iSpinachTopping(X) ) )).

fof(axiom_289,axiom,(
    ! [X] :
      ( iRosa(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_230,axiom,(
    ! [X] :
      ( iParmense(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iTomatoTopping(Y) ) ) )).

fof(axiom_117,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iGarlicTopping(X) ) )).

fof(axiom_628,axiom,(
    ! [X] :
      ~ ( iSauceTopping(X)
        & iNutTopping(X) ) )).

fof(axiom_269,axiom,(
    ! [X] :
      ( iPrinceCarlo(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iParmesanTopping(Y) ) ) )).

fof(axiom_210,axiom,(
    ! [X] :
      ( ( iPizza(X)
        & ~ iVegetarianPizza(X)
        & abstractDomain(X) )
    <=> iNonVegetarianPizza(X) ) )).

fof(axiom_292,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iTomatoTopping(Y)
          & ihasTopping(X,Y) )
     <= iRosa(X) ) )).

fof(axiom_372,axiom,(
    ! [X] :
      ( iVegetarianTopping(X)
    <=> ( iPizzaTopping(X)
        & ( iNutTopping(X)
          | iHerbSpiceTopping(X)
          | iFruitTopping(X)
          | iSauceTopping(X)
          | iVegetableTopping(X)
          | iCheeseTopping(X) ) ) ) )).

fof(axiom_501,axiom,(
    ! [X] :
      ~ ( iCaperTopping(X)
        & iOnionTopping(X) ) )).

fof(axiom_554,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iPolloAdAstra(X) ) )).

fof(axiom_495,axiom,(
    ! [X] :
      ~ ( iPizzaBase(X)
        & iPizzaTopping(X) ) )).

fof(axiom_260,axiom,(
    ! [X] :
      ( iPolloAdAstra(X)
     => ? [Y] :
          ( iGarlicTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_735,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iRosa(X) ) )).

fof(axiom_463,axiom,(
    ! [X] :
      ~ ( iNapoletana(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_437,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iCapricciosa(X) ) )).

fof(axiom_423,axiom,(
    iCountry(iEngland) )).

fof(axiom_803,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iSloppyGiuseppe(X) ) )).

fof(axiom_189,axiom,(
    ! [X] :
      ( iMushroom(X)
     => abstractDomain(X) ) )).

fof(axiom_197,axiom,(
    ! [X] :
      ( iVegetableTopping(X)
     <= iMushroomTopping(X) ) )).

fof(axiom_401,axiom,(
    ! [X,Y,Z] :
      ( ( ihasTopping(Y,X)
        & ihasTopping(Z,X) )
     => Z = Y ) )).

fof(axiom_103,axiom,(
    ! [X] :
      ( iFourSeasons(X)
     => ? [Y] :
          ( ihasTopping(X,Y)
          & iMushroomTopping(Y) ) ) )).

fof(axiom_559,axiom,(
    ! [X] :
      ~ ( iGarlicTopping(X)
        & iOliveTopping(X) ) )).

fof(axiom_781,axiom,(
    ! [X] :
      ~ ( iMargherita(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_380,axiom,(
    ! [X] :
      ( iVeneziana(X)
     => ? [Y] :
          ( iOnionTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_755,axiom,(
    ! [X] :
      ~ ( iUnclosedPizza(X)
        & iPrinceCarlo(X) ) )).

fof(axiom_220,axiom,(
    ! [X] :
      ( iParmaHamTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_176,axiom,(
    ! [X] :
      ( iPizzaTopping(X)
     <= iMeatTopping(X) ) )).

fof(axiom_264,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPrawnsTopping(X) ) )).

fof(axiom_337,axiom,(
    ! [X] :
      ( iSpinachTopping(X)
     => abstractDomain(X) ) )).

fof(axiom_274,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iQuattroFormaggi(X) ) )).

fof(axiom_109,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iFruitTopping(X) ) )).

fof(axiom_184,axiom,(
    ! [X] :
      ( iMixedSeafoodTopping(X)
     => iFishTopping(X) ) )).

fof(axiom_82,axiom,(
    ! [X] :
      ( iFiorentina(X)
     => abstractDomain(X) ) )).

fof(axiom_407,axiom,(
    ! [X,Y,Z] :
      ( Y = Z
     <= ( iisBaseOf(Y,X)
        & iisBaseOf(Z,X) ) ) )).

fof(axiom_552,axiom,(
    ! [X] :
      ~ ( iGiardiniera(X)
        & iCaprina(X) ) )).

fof(axiom_419,axiom,(
    ! [X,Y] :
      ( iisToppingOf(X,Y)
     => iPizza(Y) ) )).

fof(axiom_246,axiom,(
    ! [X] :
      ( iPineKernels(X)
     => iNutTopping(X) ) )).

fof(axiom_395,axiom,(
    ! [X,Y] :
      ( iFood(Y)
     <= ihasIngredient(X,Y) ) )).

fof(axiom_11,axiom,(
    ! [X] :
      ( iAmerican(X)
     => abstractDomain(X) ) )).

fof(axiom_240,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iPepperTopping(X) ) )).

fof(axiom_816,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_142,axiom,(
    ! [X] :
      ( iPizzaTopping(X)
     <= iHerbSpiceTopping(X) ) )).

fof(axiom_827,axiom,(
    ! [X] :
      ~ ( iPepperTopping(X)
        & iAsparagusTopping(X) ) )).

fof(axiom_715,axiom,(
    ! [X] :
      ~ ( iOliveTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_126,axiom,(
    ! [X] :
      ( iGiardiniera(X)
     => iNamedPizza(X) ) )).

fof(axiom_481,axiom,(
    ! [X] :
      ~ ( iSauceTopping(X)
        & iCheeseTopping(X) ) )).

fof(axiom_110,axiom,(
    ! [X] :
      ( iPizzaTopping(X)
     <= iFruitTopping(X) ) )).

fof(axiom_388,axiom,(
    ! [X,Y] :
      ( ihasBase(X,Y)
     => iPizza(X) ) )).

fof(axiom_191,axiom,(
    ! [X] :
      ( iMushroom(X)
     => ? [Y] :
          ( iMozzarellaTopping(Y)
          & ihasTopping(X,Y) ) ) )).

fof(axiom_151,axiom,(
    ! [X] :
      ( abstractDomain(X)
     <= iIceCream(X) ) )).

fof(axiom_593,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iVeneziana(X) ) )).

fof(axiom_59,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iAnchoviesTopping(Y)
          & ihasTopping(X,Y) )
     <= iCapricciosa(X) ) )).

fof(axiom_760,axiom,(
    ! [X] :
      ~ ( iLaReine(X)
        & iFourSeasons(X) ) )).

fof(axiom_616,axiom,(
    ! [X] :
      ~ ( iOliveTopping(X)
        & iTomatoTopping(X) ) )).

fof(axiom_822,axiom,(
    ! [X] :
      ~ ( iCaprina(X)
        & iAmericanHot(X) ) )).

fof(axiom_458,axiom,(
    ! [X] :
      ~ ( iRosa(X)
        & iVeneziana(X) ) )).

fof(axiom_756,axiom,(
    ! [X] :
      ~ ( iPrinceCarlo(X)
        & iCaprina(X) ) )).

fof(axiom_33,axiom,(
    ! [X] :
      ( ? [Y] :
          ( iMild(Y)
          & ihasSpiciness(X,Y) )
     <= iAsparagusTopping(X) ) )).

fof(axiom_805,axiom,(
    ! [X] :
      ~ ( iSpinachTopping(X)
        & iPetitPoisTopping(X) ) )).

fof(axiom_206,axiom,(
    ! [X] :
      ( iNapoletana(X)
     => ihasCountryOfOrigin(X,iItaly) ) )).

fof(axiom_713,axiom,(
    ! [X] :
      ~ ( iPolloAdAstra(X)
        & iSoho(X) ) )).

