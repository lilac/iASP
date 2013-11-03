include('Axioms/HAL001+0.ax').
fof(alpha_morphism,axiom,(
    morphism(alpha,a,b) )).

fof(h_morphism,axiom,(
    morphism(h,c,r) )).

fof(beta_morphism,axiom,(
    morphism(beta,b,c) )).

fof(delta_surjection,axiom,(
    surjection(delta) )).

fof(h_surjection,hypothesis,(
    surjection(h) )).

fof(gamma_injection,axiom,(
    injection(gamma) )).

fof(gamma_morphism,axiom,(
    morphism(gamma,d,e) )).

fof(beta_h_g_delta_commute,axiom,(
    commute(beta,h,g,delta) )).

fof(gamma_delta_exact,axiom,(
    exact(gammma,delta) )).

fof(g_morphism,axiom,(
    morphism(g,b,e) )).

fof(alpha_beta_exact,axiom,(
    exact(alpha,beta) )).

fof(delta_morphism,axiom,(
    morphism(delta,e,r) )).

fof(f_surjection,hypothesis,(
    surjection(f) )).

fof(g_surjection,conjecture,(
    surjection(g) )).

fof(alpha_injection,axiom,(
    injection(alpha) )).

fof(beta_surjection,axiom,(
    surjection(beta) )).

fof(f_morphism,axiom,(
    morphism(f,a,d) )).

fof(alpha_g_f_gamma_commute,axiom,(
    commute(alpha,g,f,gamma) )).

