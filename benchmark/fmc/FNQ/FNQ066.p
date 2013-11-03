include('Axioms/HAL001+0.ax').
fof(gamma_morphism,axiom,(
    morphism(gamma,d,e) )).

fof(h_surjection,hypothesis,(
    surjection(h) )).

fof(f_morphism,axiom,(
    morphism(f,a,d) )).

fof(delta_morphism,axiom,(
    morphism(delta,e,r) )).

fof(alpha_g_f_gamma_commute,axiom,(
    commute(alpha,g,f,gamma) )).

fof(beta_h_g_delta_commute,axiom,(
    commute(beta,h,g,delta) )).

fof(beta_morphism,axiom,(
    morphism(beta,b,c) )).

fof(alpha_injection,axiom,(
    injection(alpha) )).

fof(f_surjection,hypothesis,(
    surjection(f) )).

fof(delta_surjection,axiom,(
    surjection(delta) )).

fof(alpha_beta_exact,axiom,(
    exact(alpha,beta) )).

fof(lemma8,conjecture,(
    ! [E] :
      ( element(E,e)
     => ? [B1,E1,A] :
          ( element(B1,b)
          & element(E1,e)
          & element(A,a)
          & E1 = apply(gamma,apply(f,A))
          & E1 = apply(g,apply(alpha,A))
          & subtract(e,apply(g,B1),E) = E1 ) ) )).

fof(g_morphism,axiom,(
    morphism(g,b,e) )).

fof(gamma_delta_exact,axiom,(
    exact(gammma,delta) )).

fof(alpha_morphism,axiom,(
    morphism(alpha,a,b) )).

fof(beta_surjection,axiom,(
    surjection(beta) )).

fof(gamma_injection,axiom,(
    injection(gamma) )).

fof(lemma3,axiom,(
    ! [E] :
      ( ? [R,B1] :
          ( apply(delta,apply(g,B1)) = R
          & R = apply(h,apply(beta,B1))
          & element(B1,b)
          & apply(delta,E) = R
          & element(R,r) )
     <= element(E,e) ) )).

fof(h_morphism,axiom,(
    morphism(h,c,r) )).

