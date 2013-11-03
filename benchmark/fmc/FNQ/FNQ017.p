fof(mp_first_movers_negative_growth,axiom,(
    ! [E] :
      ( ( stable(E)
        & ? [T1] :
            ( in_environment(E,T1)
            & ! [T] :
                ( ( greater_or_equal(T,T1)
                  & subpopulations(first_movers,efficient_producers,E,T) )
               => greater(zero,growth_rate(first_movers,T)) ) )
        & environment(E) )
     => ? [T2] :
          ( ! [T] :
              ( ( greater_or_equal(T,T2)
                & subpopulations(first_movers,efficient_producers,E,T) )
             => greater(zero,growth_rate(first_movers,T)) )
          & greater(T2,appear(efficient_producers,E)) ) ) )).

fof(a3,hypothesis,(
    ! [E] :
      ( environment(E)
     => greater(appear(efficient_producers,e),appear(first_movers,E)) ) )).

fof(mp_contains_FM_and_EP,axiom,(
    ! [E,T] :
      ( subpopulations(first_movers,efficient_producers,E,T)
     <= ( in_environment(E,T)
        & greater(cardinality_at_time(first_movers,T),zero)
        & greater(cardinality_at_time(efficient_producers,T),zero)
        & environment(E) ) ) )).

fof(prove_t7,conjecture,(
    ! [E] :
      ( ? [To] :
          ( in_environment(E,To)
          & greater(To,appear(first_movers,E))
          & cardinality_at_time(first_movers,to) = zero )
     <= ( stable(E)
        & environment(E) ) ) )).

fof(mp_stable_efficient_producers,axiom,(
    ! [E] :
      ( in_environment(E,appear(efficient_producers,E))
     <= ( stable(E)
        & environment(E) ) ) )).

fof(mp_contracting_time,axiom,(
    ! [S,To] :
      ( ( finite_set(S)
        & contracts_from(To,S) )
     => ? [T2] :
          ( greater(T2,To)
          & zero = cardinality_at_time(s,t2) ) ) )).

fof(mp_stable_first_movers,axiom,(
    ! [E] :
      ( ( environment(E)
        & stable(E) )
     => in_environment(E,appear(first_movers,E)) ) )).

fof(mp_contracts_from,axiom,(
    ! [E,To] :
      ( contracts_from(To,first_movers)
     <= ( stable(E)
        & ! [T] :
            ( ( greater_or_equal(T,To)
              & greater(cardinality_at_time(first_movers,T),zero) )
           => greater(zero,growth_rate(first_movers,T)) )
        & in_environment(E,To)
        & environment(E) ) ) )).

fof(mp_times_in_environment,axiom,(
    ! [E,T1,T2] :
      ( ( greater(T1,T2)
        | T1 = T2
        | greater(T2,T1) )
     <= ( in_environment(E,T1)
        & in_environment(E,T2) ) ) )).

fof(mp_greater_or_equal,axiom,(
    ! [X,Y] :
      ( ( X = Y
        | greater(X,Y) )
    <=> greater_or_equal(X,Y) ) )).

fof(a5,hypothesis,(
    ! [E] :
      ( ( stable(E)
        & environment(E) )
     => ? [T] :
          ( greater_or_equal(T,equilibrium(E))
          & in_environment(E,T) ) ) )).

fof(mp_long_stable_environments,axiom,(
    ! [E,T1,T2] :
      ( in_environment(E,T2)
     <= ( stable(E)
        & in_environment(E,T1)
        & greater(T2,T1)
        & environment(E) ) ) )).

fof(l1,hypothesis,(
    ! [E] :
      ( ? [To] :
          ( in_environment(E,To)
          & ! [T] :
              ( ( subpopulations(first_movers,efficient_producers,E,T)
                & greater_or_equal(T,To) )
             => greater(growth_rate(efficient_producers,T),growth_rate(first_movers,T)) ) )
     <= ( environment(E)
        & stable(E) ) ) )).

fof(t6,hypothesis,(
    ! [E,T] :
      ( greater(cardinality_at_time(efficient_producers,T),zero)
     <= ( environment(E)
        & greater_or_equal(T,appear(efficient_producers,E))
        & in_environment(E,T) ) ) )).

fof(mp_greater_transitivity,axiom,(
    ! [X,Y,Z] :
      ( ( greater(X,Y)
        & greater(Y,Z) )
     => greater(X,Z) ) )).

fof(mp7_first_movers_exist,axiom,(
    finite_set(first_movers) )).

