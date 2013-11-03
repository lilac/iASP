include('Axioms/MED001+0.ax').
fof(treatmentex_sub,conjecture,
    ( ( ! [X0] :
          ( ~ gt(n0,X0)
         => ( drugbg(X0)
            & drugsu(X0)
            & drugi(X0) ) )
      & bcapacityex(n0)
      & ! [X0] :
          ( conditionhyper(X0)
         <= gt(n0,X0) ) )
   => ! [X0] :
        ( ~ gt(n0,X0)
       => conditionnormo(X0) ) )).

