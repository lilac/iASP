fof(encrypt_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(n1,X3),enc(n1,X5,X6,X7)))
     <= p(state(X0,X1),iknows(atoms(n1,X3),enc(n0,X5,X6,X7))) ) )).

fof(unwrap_hn1k1_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,n1,X2,X3,X4,X5),X6),iknows(X7,enc(n1,X9,X10,X11)))
     => p(state(h(n1,X0,n1,X2,X3,X4,n1),X6),iknows(X7,enc(n1,X9,X10,X11))) ) )).

fof(initial_state,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] : p(state(h(n1,n0,n0,n0,n0,n0,n1),h(n1,n0,n0,n0,n0,n0,n0)),iknows(atoms(n0,n0),enc(n0,n0,n0,n0))) )).

fof(set_attr_hn1k1_sensitive_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,X2,X3,n1,X5),X6),X7)
     <= p(state(h(n1,X0,X1,X2,X3,n0,X5),X6),X7) ) )).

fof(encrypt_hn1k1_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,n1,X3,X4,X5),X6),iknows(atoms(n1,X8),enc(n1,X10,X11,X12)))
     <= p(state(h(n1,X0,X1,n1,X3,X4,X5),X6),iknows(atoms(n1,X8),enc(n0,X10,X11,X12))) ) )).

fof(co1,conjecture,(
    ? [X,Y,X1,X2] : p(X,iknows(atoms(n1,X1),Y)) )).

fof(decrypt_hn1k2_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,X2,X3,n1,X5,X6)),iknows(atoms(X7,n0),enc(X9,X10,X11,n1)))
     => p(state(X0,h(n1,X1,X2,X3,n1,X5,X6)),iknows(atoms(X7,n1),enc(X9,X10,X11,n1))) ) )).

fof(set_attr_hn1k1_extractable_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n0),X6),X7)
     <= p(state(h(n1,X0,X1,X2,X3,X4,n1),X6),X7) ) )).

fof(decrypt_hn1k2_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,X2,X3,n1,X5,X6)),iknows(atoms(n1,X8),enc(X9,n1,X11,X12)))
     <= p(state(X0,h(n1,X1,X2,X3,n1,X5,X6)),iknows(atoms(n0,X8),enc(X9,n1,X11,X12))) ) )).

fof(set_attr_hn1k2_sensitive_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,X2,X3,X4,n0,X6)),X7)
     => p(state(X0,h(n1,X1,X2,X3,X4,n1,X6)),X7) ) )).

fof(wrap_hn1k1_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,n1,X1,X2,X3,X4,X5),h(n1,X6,X7,X8,X9,X10,n1)),iknows(X12,enc(X13,X14,n0,X16)))
     => p(state(h(n1,n1,X1,X2,X3,X4,X5),h(n1,X6,X7,X8,X9,X10,n1)),iknows(X12,enc(X13,X14,n1,X16))) ) )).

fof(encrypt_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(n1,n1),enc(X4,n0,X6,X7)))
     => p(state(X0,X1),iknows(atoms(n1,n1),enc(X4,n1,X6,X7))) ) )).

fof(decrypt_hn1k1_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,X2,n1,X4,X5),X6),iknows(atoms(n0,X8),enc(n1,X10,X11,X12)))
     => p(state(h(n1,X0,X1,X2,n1,X4,X5),X6),iknows(atoms(n1,X8),enc(n1,X10,X11,X12))) ) )).

fof(decrypt_hn1k1_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,X2,n1,X4,X5),X6),iknows(atoms(X7,n0),enc(X9,X10,n1,X12)))
     => p(state(h(n1,X0,X1,X2,n1,X4,X5),X6),iknows(atoms(X7,n1),enc(X9,X10,n1,X12))) ) )).

fof(intruder_decrypt_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(X2,n1),enc(X4,X5,X6,n1)))
     <= p(state(X0,X1),iknows(atoms(X2,n1),enc(X4,X5,X6,n1))) ) )).

fof(set_attr_hn1k1_decrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,n0,X1,X2,n1,X4,X5),X6),X7)
     <= p(state(h(n1,n0,X1,X2,n0,X4,X5),X6),X7) ) )).

fof(encrypt_hn1k2_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,X2,n1,X4,X5,X6)),iknows(atoms(n1,X8),enc(X9,n1,X11,X12)))
     <= p(state(X0,h(n1,X1,X2,n1,X4,X5,X6)),iknows(atoms(n1,X8),enc(X9,n0,X11,X12))) ) )).

fof(set_attr_hn1k2_encrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,n0,n0,n1,X4,X5,X6)),X7)
     <= p(state(X0,h(n1,n0,n0,n0,X4,X5,X6)),X7) ) )).

fof(encrypt_hn1k2_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,X2,n1,X4,X5,X6)),iknows(atoms(X7,n1),enc(X9,X10,X11,n1)))
     <= p(state(X0,h(n1,X1,X2,n1,X4,X5,X6)),iknows(atoms(X7,n1),enc(X9,X10,X11,n0))) ) )).

fof(encrypt_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(X2,n1),enc(X4,X5,X6,n0)))
     => p(state(X0,X1),iknows(atoms(X2,n1),enc(X4,X5,X6,n1))) ) )).

fof(unwrap_hn1k2_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n0,X0,X1,X2,X3,X4,X5),h(n1,X6,n1,X8,X9,X10,X11)),iknows(X12,enc(X13,n1,X15,X16)))
     => p(state(h(n1,X0,X1,X2,X3,X4,n1),h(n1,X6,n1,X8,X9,X10,X11)),iknows(X12,enc(X13,n1,X15,X16))) ) )).

fof(intruder_decrypt_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(n1,X3),enc(n1,X5,X6,X7)))
     => p(state(X0,X1),iknows(atoms(n1,X3),enc(n1,X5,X6,X7))) ) )).

fof(intruder_decrypt_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(n1,n0),enc(X4,X5,n1,X7)))
     => p(state(X0,X1),iknows(atoms(X2,n1),enc(X4,X5,n1,X7))) ) )).

fof(intruder_decrypt_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(n0,n1),enc(X4,n1,X6,X7)))
     => p(state(X0,X1),iknows(atoms(n1,X3),enc(X4,n1,X6,X7))) ) )).

fof(wrap_hn1k2_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),h(n1,n1,X7,X8,X9,X10,X11)),iknows(X12,enc(X13,n1,X15,X16)))
     <= p(state(h(n1,X0,X1,X2,X3,X4,n1),h(n1,n1,X7,X8,X9,X10,X11)),iknows(X12,enc(X13,n0,X15,X16))) ) )).

fof(encrypt_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,X1),iknows(atoms(n1,n1),enc(X4,X5,n1,X7)))
     <= p(state(X0,X1),iknows(atoms(n1,n1),enc(X4,X5,n0,X7))) ) )).

fof(set_attr_hn1k2_wrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,n0,n0,X3,n0,X5,X6)),X7)
     => p(state(X0,h(n1,n1,n0,X3,n0,X5,X6)),X7) ) )).

fof(wrap_hn1k1_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,n1,X1,X2,X3,X4,n1),X6),iknows(X7,enc(n0,X9,X10,X11)))
     => p(state(h(n1,n1,X1,X2,X3,X4,n1),X6),iknows(X7,enc(n1,X9,X10,X11))) ) )).

fof(unwrap_hn1k1_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,n1,X2,X3,X4,X5),h(n0,X6,X7,X8,X9,X10,X11)),iknows(X12,enc(X13,X14,n1,X16)))
     => p(state(h(n1,X0,n1,X2,X3,X4,X5),h(n1,X6,X7,X8,X9,X10,n1)),iknows(X12,enc(X13,X14,n1,X16))) ) )).

fof(set_attr_hn1k2_unwrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,n0,n0,n0,X4,X5,X6)),X7)
     => p(state(X0,h(n1,n0,n1,n0,X4,X5,X6)),X7) ) )).

fof(encrypt_hn1k1_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,X0,X1,n1,X3,X4,X5),X6),iknows(atoms(X7,n1),enc(X9,X10,n1,X12)))
     <= p(state(h(n1,X0,X1,n1,X3,X4,X5),X6),iknows(atoms(X7,n1),enc(X9,X10,n0,X12))) ) )).

fof(set_attr_hn1k2_extractable_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,X2,X3,X4,X5,n1)),X7)
     => p(state(X0,h(n1,X1,X2,X3,X4,X5,n0)),X7) ) )).

fof(set_attr_hn1k2_decrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,n0,X2,X3,n0,X5,X6)),X7)
     => p(state(X0,h(n1,n0,X2,X3,n1,X5,X6)),X7) ) )).

fof(domain_constraints,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(X0,X1,X2,X3,X4,X5,X6),h(X7,X8,X9,X10,X11,X12,X13)),iknows(atoms(X14,X15),enc(X16,X17,X18,X19)))
     => ( ( X0 = n1
        <~> X0 = n0 )
        & ( X2 = n0
        <~> n1 = X2 )
        & ( n1 = X11
        <~> X11 = n0 )
        & ( X12 = n0
        <~> X12 = n1 )
        & ( X14 = n1
        <~> X14 = n0 )
        & ( X19 = n1
        <~> X19 = n0 )
        & ( n1 = X18
        <~> n0 = X18 )
        & ( n0 = X17
        <~> X17 = n1 )
        & ( n1 = X16
        <~> X16 = n0 )
        & ( X15 = n0
        <~> n1 = X15 )
        & ( n1 = X13
        <~> n0 = X13 )
        & ( n1 = X10
        <~> X10 = n0 )
        & ( X9 = n0
        <~> X9 = n1 )
        & ( n0 = X8
        <~> X8 = n1 )
        & ( X7 = n1
        <~> n0 = X7 )
        & ( n0 = X6
        <~> X6 = n1 )
        & ( X5 = n1
        <~> n0 = X5 )
        & ( X4 = n1
        <~> X4 = n0 )
        & ( n0 = X3
        <~> n1 = X3 )
        & ( X1 = n0
        <~> X1 = n1 ) ) ) )).

fof(set_attr_hn1k1_encrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,n0,n0,n1,X3,X4,X5),X6),X7)
     <= p(state(h(n1,n0,n0,n0,X3,X4,X5),X6),X7) ) )).

fof(set_attr_hn1k1_unwrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,n0,n0,n0,X3,X4,X5),X6),X7)
     => p(state(h(n1,n0,n1,n0,X3,X4,X5),X6),X7) ) )).

fof(wrap_hn1k2_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,n1,X2,X3,X4,X5,n1)),iknows(X7,enc(X8,X9,X10,n0)))
     => p(state(X0,h(n1,n1,X2,X3,X4,X5,n1)),iknows(X7,enc(X8,X9,X10,n1))) ) )).

fof(set_attr_hn1k1_wrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(h(n1,n0,n0,X2,n0,X4,X5),X6),X7)
     => p(state(h(n1,n1,n0,X2,n0,X4,X5),X6),X7) ) )).

fof(unwrap_hn1k2_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20] :
      ( p(state(X0,h(n1,X1,n1,X3,X4,X5,X6)),iknows(X7,enc(X8,X9,X10,n1)))
     => p(state(X0,h(n1,X1,n1,X3,X4,X5,n1)),iknows(X7,enc(X8,X9,X10,n1))) ) )).

