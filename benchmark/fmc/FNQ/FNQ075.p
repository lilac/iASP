include('Axioms/KLE001+0.ax').
include('Axioms/KLE001+4.ax').
include('Axioms/KLE001+6.ax').
include('Axioms/KLE001+7.ax').
fof(goals,conjecture,(
    ! [X0,X1] :
      ( ( addition(multiplication(X1,X0),multiplication(star(X0),star(X1))) = multiplication(star(X0),star(X1))
        & divergence(addition(X0,X1)) = zero )
     => multiplication(star(X0),star(X1)) = addition(star(addition(X0,X1)),multiplication(star(X0),star(X1))) ) )).

