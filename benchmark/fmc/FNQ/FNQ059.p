fof(wrap_hn1k2_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n1,X2,X3,X4,X5,n1),X7,X8),iknows(X9,enc(X10,X11,X12,n1)))
     <= p(state(X0,h(n1,n1,X2,X3,X4,X5,n1),X7,X8),iknows(X9,enc(X10,X11,X12,n0))) ) )).

fof(set_attr_hn1k2_encrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n0,n0,n0,X4,X5,X6),X7,X8),X9)
     => p(state(X0,h(n1,n0,n0,n1,X4,X5,X6),X7,X8),X9) ) )).

fof(encrypt_hn1k2_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,n1,X4,X5,X6),X7,X8),iknows(atoms(n1,X10),enc(X11,n1,X13,X14)))
     <= p(state(X0,h(n1,X1,X2,n1,X4,X5,X6),X7,X8),iknows(atoms(n1,X10),enc(X11,n0,X13,X14))) ) )).

fof(set_attr_hn2k1_wrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,n1,n0,X4,n0,X6,X7),X8),X9)
     <= p(state(X0,X1,h(n1,n0,n0,X4,n0,X6,X7),X8),X9) ) )).

fof(set_attr_hn2k1_sensitive_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,X4,X5,n1,X7),X8),X9)
     <= p(state(X0,X1,h(n1,X2,X3,X4,X5,n0,X7),X8),X9) ) )).

fof(unwrap_hn2k1_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,h(n1,X7,n1,X9,X10,X11,X12),X13),iknows(X14,enc(n1,X16,X17,X18)))
     <= p(state(h(n0,X0,X1,X2,X3,X4,X5),X6,h(n1,X7,n1,X9,X10,X11,X12),X13),iknows(X14,enc(n1,X16,X17,X18))) ) )).

fof(wrap_hn2k2_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,X7,h(n1,n1,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,n0,X17,X18)))
     => p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,X7,h(n1,n1,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(wrap_hn1k1_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n1,X1,X2,X3,X4,X5),X6,X7,h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,n0,X18)))
     => p(state(h(n1,n1,X1,X2,X3,X4,X5),X6,X7,h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(wrap_hn2k1_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,n1,X3,X4,X5,X6,X7),h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,n0,X18)))
     => p(state(X0,X1,h(n1,n1,X3,X4,X5,X6,X7),h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(wrap_hn1k2_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n1,X2,X3,X4,X5,X6),h(n1,X7,X8,X9,X10,X11,n1),X13),iknows(X14,enc(X15,n1,X17,X18)))
     <= p(state(X0,h(n1,n1,X2,X3,X4,X5,X6),h(n1,X7,X8,X9,X10,X11,n1),X13),iknows(X14,enc(X15,n0,X17,X18))) ) )).

fof(wrap_hn2k1_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),h(n1,n1,X8,X9,X10,X11,X12),X13),iknows(X14,enc(X15,X16,n0,X18)))
     => p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),h(n1,n1,X8,X9,X10,X11,X12),X13),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(wrap_hn2k2_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,n1,X4,X5,X6,X7,n1)),iknows(X9,enc(X10,X11,X12,n0)))
     => p(state(X0,X1,X2,h(n1,n1,X4,X5,X6,X7,n1)),iknows(X9,enc(X10,X11,X12,n1))) ) )).

fof(wrap_hn2k1_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,h(n1,n1,X8,X9,X10,X11,X12),X13),iknows(X14,enc(n0,X16,X17,X18)))
     => p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,h(n1,n1,X8,X9,X10,X11,X12),X13),iknows(X14,enc(n1,X16,X17,X18))) ) )).

fof(unwrap_hn1k1_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,n1,X2,X3,X4,X5),X6,h(n0,X7,X8,X9,X10,X11,X12),X13),iknows(X14,enc(n1,X16,X17,X18)))
     => p(state(h(n1,X0,n1,X2,X3,X4,X5),X6,h(n1,X7,X8,X9,X10,X11,n1),X13),iknows(X14,enc(n1,X16,X17,X18))) ) )).

fof(unwrap_hn1k1_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,n1,X2,X3,X4,n1),X6,X7,X8),iknows(X9,enc(n1,X11,X12,X13)))
     <= p(state(h(n1,X0,n1,X2,X3,X4,X5),X6,X7,X8),iknows(X9,enc(n1,X11,X12,X13))) ) )).

fof(set_attr_hn1k1_unwrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n0,n1,n0,X3,X4,X5),X6,X7,X8),X9)
     <= p(state(h(n1,n0,n0,n0,X3,X4,X5),X6,X7,X8),X9) ) )).

fof(encrypt_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(n1,n1),enc(X6,n1,X8,X9)))
     <= p(state(X0,X1,X2,X3),iknows(atoms(n1,n1),enc(X6,n0,X8,X9))) ) )).

fof(unwrap_hn2k2_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,n1,X5,X6,X7,X8)),iknows(X9,enc(X10,X11,X12,n1)))
     => p(state(X0,X1,X2,h(n1,X3,n1,X5,X6,X7,n1)),iknows(X9,enc(X10,X11,X12,n1))) ) )).

fof(unwrap_hn1k2_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),h(n1,X6,n1,X8,X9,X10,X11),X12,X13),iknows(X14,enc(X15,n1,X17,X18)))
     <= p(state(h(n0,X0,X1,X2,X3,X4,X5),h(n1,X6,n1,X8,X9,X10,X11),X12,X13),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(set_attr_hn2k1_encrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,n0,n0,n0,X5,X6,X7),X8),X9)
     => p(state(X0,X1,h(n1,n0,n0,n1,X5,X6,X7),X8),X9) ) )).

fof(intruder_decrypt_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(n1,X5),enc(n1,X7,X8,X9)))
     <= p(state(X0,X1,X2,X3),iknows(atoms(n1,X5),enc(n1,X7,X8,X9))) ) )).

fof(decrypt_hn2k1_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,X4,n1,X6,X7),X8),iknows(atoms(n1,X10),enc(n1,X12,X13,X14)))
     <= p(state(X0,X1,h(n1,X2,X3,X4,n1,X6,X7),X8),iknows(atoms(n0,X10),enc(n1,X12,X13,X14))) ) )).

fof(unwrap_hn2k2_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n0,X1,X2,X3,X4,X5,X6),X7,h(n1,X8,n1,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,X17,n1)))
     => p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),X7,h(n1,X8,n1,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,X17,n1))) ) )).

fof(set_attr_hn1k2_decrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n0,X2,X3,n0,X5,X6),X7,X8),X9)
     => p(state(X0,h(n1,n0,X2,X3,n1,X5,X6),X7,X8),X9) ) )).

fof(wrap_hn1k1_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n1,X1,X2,X3,X4,X5),X6,h(n1,X7,X8,X9,X10,X11,n1),X13),iknows(X14,enc(n1,X16,X17,X18)))
     <= p(state(h(n1,n1,X1,X2,X3,X4,X5),X6,h(n1,X7,X8,X9,X10,X11,n1),X13),iknows(X14,enc(n0,X16,X17,X18))) ) )).

fof(co1,conjecture,(
    ? [X,Y,X1,X2] : p(X,iknows(atoms(n1,X1),Y)) )).

fof(encrypt_hn2k2_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,X4,n1,X6,X7,X8)),iknows(atoms(X9,n1),enc(X11,X12,X13,n1)))
     <= p(state(X0,X1,X2,h(n1,X3,X4,n1,X6,X7,X8)),iknows(atoms(X9,n1),enc(X11,X12,X13,n0))) ) )).

fof(encrypt_hn1k1_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,n1,X3,X4,X5),X6,X7,X8),iknows(atoms(n1,X10),enc(n0,X12,X13,X14)))
     => p(state(h(n1,X0,X1,n1,X3,X4,X5),X6,X7,X8),iknows(atoms(n1,X10),enc(n1,X12,X13,X14))) ) )).

fof(unwrap_hn1k2_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,n1,X3,X4,X5,X6),h(n0,X7,X8,X9,X10,X11,X12),X13),iknows(X14,enc(X15,n1,X17,X18)))
     => p(state(X0,h(n1,X1,n1,X3,X4,X5,X6),h(n1,X7,X8,X9,X10,X11,n1),X13),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(decrypt_hn2k2_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,X4,X5,n1,X7,X8)),iknows(atoms(X9,n1),enc(X11,X12,X13,n1)))
     <= p(state(X0,X1,X2,h(n1,X3,X4,X5,n1,X7,X8)),iknows(atoms(X9,n0),enc(X11,X12,X13,n1))) ) )).

fof(set_attr_hn2k2_decrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,n0,X4,X5,n1,X7,X8)),X9)
     <= p(state(X0,X1,X2,h(n1,n0,X4,X5,n0,X7,X8)),X9) ) )).

fof(encrypt_hn1k1_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,n1,X3,X4,X5),X6,X7,X8),iknows(atoms(X9,n1),enc(X11,X12,n0,X14)))
     => p(state(h(n1,X0,X1,n1,X3,X4,X5),X6,X7,X8),iknows(atoms(X9,n1),enc(X11,X12,n1,X14))) ) )).

fof(set_attr_hn1k2_sensitive_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,X3,X4,n0,X6),X7,X8),X9)
     => p(state(X0,h(n1,X1,X2,X3,X4,n1,X6),X7,X8),X9) ) )).

fof(wrap_hn1k1_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n1,X1,X2,X3,X4,n1),X6,X7,X8),iknows(X9,enc(n1,X11,X12,X13)))
     <= p(state(h(n1,n1,X1,X2,X3,X4,n1),X6,X7,X8),iknows(X9,enc(n0,X11,X12,X13))) ) )).

fof(wrap_hn2k2_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),X7,h(n1,n1,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,X17,n1)))
     <= p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),X7,h(n1,n1,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,X17,n0))) ) )).

fof(intruder_decrypt_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(X4,n1),enc(X6,X7,X8,n1)))
     <= p(state(X0,X1,X2,X3),iknows(atoms(X4,n1),enc(X6,X7,X8,n1))) ) )).

fof(set_attr_hn2k1_unwrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,n0,n0,n0,X5,X6,X7),X8),X9)
     => p(state(X0,X1,h(n1,n0,n1,n0,X5,X6,X7),X8),X9) ) )).

fof(set_attr_hn1k1_wrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n0,n0,X2,n0,X4,X5),X6,X7,X8),X9)
     => p(state(h(n1,n1,n0,X2,n0,X4,X5),X6,X7,X8),X9) ) )).

fof(unwrap_hn2k1_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,n1,X4,X5,X6,n1),X8),iknows(X9,enc(n1,X11,X12,X13)))
     <= p(state(X0,X1,h(n1,X2,n1,X4,X5,X6,X7),X8),iknows(X9,enc(n1,X11,X12,X13))) ) )).

fof(initial_state,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] : p(state(h(n1,n0,n0,n0,n0,n0,n1),h(n1,n0,n0,n0,n0,n0,n0),h(n0,n0,n0,n0,n0,n0,n0),h(n0,n0,n0,n0,n0,n0,n0)),iknows(atoms(n0,n0),enc(n0,n0,n0,n0))) )).

fof(set_attr_hn1k1_decrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n0,X1,X2,n0,X4,X5),X6,X7,X8),X9)
     => p(state(h(n1,n0,X1,X2,n1,X4,X5),X6,X7,X8),X9) ) )).

fof(unwrap_hn1k1_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,n1,X2,X3,X4,X5),h(n0,X6,X7,X8,X9,X10,X11),X12,X13),iknows(X14,enc(X15,X16,n1,X18)))
     => p(state(h(n1,X0,n1,X2,X3,X4,X5),h(n1,X6,X7,X8,X9,X10,n1),X12,X13),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(set_attr_hn1k2_unwrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n0,n1,n0,X4,X5,X6),X7,X8),X9)
     <= p(state(X0,h(n1,n0,n0,n0,X4,X5,X6),X7,X8),X9) ) )).

fof(wrap_hn1k2_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n1,X2,X3,X4,X5,X6),X7,h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,X17,n0)))
     => p(state(X0,h(n1,n1,X2,X3,X4,X5,X6),X7,h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,X17,n1))) ) )).

fof(set_attr_hn1k2_wrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,n1,n0,X3,n0,X5,X6),X7,X8),X9)
     <= p(state(X0,h(n1,n0,n0,X3,n0,X5,X6),X7,X8),X9) ) )).

fof(set_attr_hn1k2_extractable_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,X3,X4,X5,n0),X7,X8),X9)
     <= p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),X7,X8),X9) ) )).

fof(unwrap_hn1k2_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,n1,X3,X4,X5,X6),X7,h(n0,X8,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,X17,n1)))
     => p(state(X0,h(n1,X1,n1,X3,X4,X5,X6),X7,h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,X17,n1))) ) )).

fof(unwrap_hn1k2_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,n1,X3,X4,X5,X6),X7,X8),iknows(X9,enc(X10,X11,X12,n1)))
     => p(state(X0,h(n1,X1,n1,X3,X4,X5,n1),X7,X8),iknows(X9,enc(X10,X11,X12,n1))) ) )).

fof(domain_constraints,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(X0,X1,X2,X3,X4,X5,X6),h(X7,X8,X9,X10,X11,X12,X13),h(X14,X15,X16,X17,X18,X19,X20),h(X21,X22,X23,X24,X25,X26,X27)),iknows(atoms(X28,X29),enc(X30,X31,X32,X33)))
     => ( ( n0 = X0
        <~> n1 = X0 )
        & ( n1 = X2
        <~> X2 = n0 )
        & ( X3 = n1
        <~> n0 = X3 )
        & ( n1 = X7
        <~> X7 = n0 )
        & ( X8 = n1
        <~> n0 = X8 )
        & ( n1 = X9
        <~> X9 = n0 )
        & ( n0 = X10
        <~> n1 = X10 )
        & ( n0 = X11
        <~> X11 = n1 )
        & ( X12 = n0
        <~> n1 = X12 )
        & ( X14 = n0
        <~> X14 = n1 )
        & ( n0 = X18
        <~> n1 = X18 )
        & ( n1 = X19
        <~> X19 = n0 )
        & ( X21 = n0
        <~> n1 = X21 )
        & ( n0 = X22
        <~> X22 = n1 )
        & ( X24 = n1
        <~> X24 = n0 )
        & ( X25 = n0
        <~> n1 = X25 )
        & ( X27 = n0
        <~> n1 = X27 )
        & ( n1 = X29
        <~> n0 = X29 )
        & ( n1 = X30
        <~> n0 = X30 )
        & ( n1 = X31
        <~> X31 = n0 )
        & ( X33 = n0
        <~> n1 = X33 )
        & ( n0 = X32
        <~> X32 = n1 )
        & ( X28 = n1
        <~> n0 = X28 )
        & ( n1 = X26
        <~> X26 = n0 )
        & ( n1 = X23
        <~> X23 = n0 )
        & ( n0 = X20
        <~> X20 = n1 )
        & ( X17 = n1
        <~> n0 = X17 )
        & ( X16 = n1
        <~> n0 = X16 )
        & ( n1 = X15
        <~> X15 = n0 )
        & ( n1 = X13
        <~> X13 = n0 )
        & ( n1 = X6
        <~> X6 = n0 )
        & ( X5 = n1
        <~> n0 = X5 )
        & ( X4 = n0
        <~> X4 = n1 )
        & ( X1 = n1
        <~> n0 = X1 ) ) ) )).

fof(set_attr_hn2k2_wrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,n1,n0,X5,n0,X7,X8)),X9)
     <= p(state(X0,X1,X2,h(n1,n0,n0,X5,n0,X7,X8)),X9) ) )).

fof(encrypt_hn1k2_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,n1,X4,X5,X6),X7,X8),iknows(atoms(X9,n1),enc(X11,X12,X13,n0)))
     => p(state(X0,h(n1,X1,X2,n1,X4,X5,X6),X7,X8),iknows(atoms(X9,n1),enc(X11,X12,X13,n1))) ) )).

fof(unwrap_hn1k1_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,n1,X2,X3,X4,X5),X6,X7,h(n0,X8,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,n1,X18)))
     => p(state(h(n1,X0,n1,X2,X3,X4,X5),X6,X7,h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(wrap_hn2k2_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,X4,X5,X6,n1),h(n1,n1,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,n0,X17,X18)))
     => p(state(X0,X1,h(n1,X2,X3,X4,X5,X6,n1),h(n1,n1,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(encrypt_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(n1,n1),enc(X6,X7,n0,X9)))
     => p(state(X0,X1,X2,X3),iknows(atoms(n1,n1),enc(X6,X7,n1,X9))) ) )).

fof(decrypt_hn1k2_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,X3,n1,X5,X6),X7,X8),iknows(atoms(X9,n0),enc(X11,X12,X13,n1)))
     => p(state(X0,h(n1,X1,X2,X3,n1,X5,X6),X7,X8),iknows(atoms(X9,n1),enc(X11,X12,X13,n1))) ) )).

fof(wrap_hn2k1_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,n1,X3,X4,X5,X6,n1),X8),iknows(X9,enc(n1,X11,X12,X13)))
     <= p(state(X0,X1,h(n1,n1,X3,X4,X5,X6,n1),X8),iknows(X9,enc(n0,X11,X12,X13))) ) )).

fof(set_attr_hn2k1_decrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,n0,X3,X4,n0,X6,X7),X8),X9)
     => p(state(X0,X1,h(n1,n0,X3,X4,n1,X6,X7),X8),X9) ) )).

fof(set_attr_hn2k2_extractable_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,X4,X5,X6,X7,n0)),X9)
     <= p(state(X0,X1,X2,h(n1,X3,X4,X5,X6,X7,n1)),X9) ) )).

fof(set_attr_hn1k1_extractable_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,X7,X8),X9)
     => p(state(h(n1,X0,X1,X2,X3,X4,n0),X6,X7,X8),X9) ) )).

fof(encrypt_hn2k1_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,n1,X5,X6,X7),X8),iknows(atoms(X9,n1),enc(X11,X12,n0,X14)))
     => p(state(X0,X1,h(n1,X2,X3,n1,X5,X6,X7),X8),iknows(atoms(X9,n1),enc(X11,X12,n1,X14))) ) )).

fof(set_attr_hn2k2_encrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,n0,n0,n1,X6,X7,X8)),X9)
     <= p(state(X0,X1,X2,h(n1,n0,n0,n0,X6,X7,X8)),X9) ) )).

fof(intruder_decrypt_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(n0,n1),enc(X6,n1,X8,X9)))
     => p(state(X0,X1,X2,X3),iknows(atoms(n1,X5),enc(X6,n1,X8,X9))) ) )).

fof(encrypt_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(n1,X5),enc(n0,X7,X8,X9)))
     => p(state(X0,X1,X2,X3),iknows(atoms(n1,X5),enc(n1,X7,X8,X9))) ) )).

fof(unwrap_hn2k2_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n0,X0,X1,X2,X3,X4,X5),X6,X7,h(n1,X8,n1,X10,X11,X12,X13)),iknows(X14,enc(X15,n1,X17,X18)))
     => p(state(h(n1,X0,X1,X2,X3,X4,n1),X6,X7,h(n1,X8,n1,X10,X11,X12,X13)),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(decrypt_hn1k2_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n1,X1,X2,X3,n1,X5,X6),X7,X8),iknows(atoms(n0,X10),enc(X11,n1,X13,X14)))
     => p(state(X0,h(n1,X1,X2,X3,n1,X5,X6),X7,X8),iknows(atoms(n1,X10),enc(X11,n1,X13,X14))) ) )).

fof(encrypt_k2Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(X4,n1),enc(X6,X7,X8,n1)))
     <= p(state(X0,X1,X2,X3),iknows(atoms(X4,n1),enc(X6,X7,X8,n0))) ) )).

fof(unwrap_hn2k1_hn2k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,n1,X4,X5,X6,X7),h(n0,X8,X9,X10,X11,X12,X13)),iknows(X14,enc(X15,X16,n1,X18)))
     => p(state(X0,X1,h(n1,X2,n1,X4,X5,X6,X7),h(n1,X8,X9,X10,X11,X12,n1)),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(set_attr_hn1k1_sensitive_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,n0,X5),X6,X7,X8),X9)
     => p(state(h(n1,X0,X1,X2,X3,n1,X5),X6,X7,X8),X9) ) )).

fof(set_attr_hn2k2_unwrap_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,n0,n1,n0,X6,X7,X8)),X9)
     <= p(state(X0,X1,X2,h(n1,n0,n0,n0,X6,X7,X8)),X9) ) )).

fof(unwrap_hn2k2_hn2k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n0,X2,X3,X4,X5,X6,X7),h(n1,X8,n1,X10,X11,X12,X13)),iknows(X14,enc(X15,n1,X17,X18)))
     => p(state(X0,X1,h(n1,X2,X3,X4,X5,X6,n1),h(n1,X8,n1,X10,X11,X12,X13)),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(intruder_decrypt_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,X3),iknows(atoms(n1,n0),enc(X6,X7,n1,X9)))
     => p(state(X0,X1,X2,X3),iknows(atoms(X4,n1),enc(X6,X7,n1,X9))) ) )).

fof(set_attr_hn1k1_encrypt_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n0,n0,n1,X3,X4,X5),X6,X7,X8),X9)
     <= p(state(h(n1,n0,n0,n0,X3,X4,X5),X6,X7,X8),X9) ) )).

fof(wrap_hn1k2_hn1k1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,X3,X4,n1),h(n1,n1,X7,X8,X9,X10,X11),X12,X13),iknows(X14,enc(X15,n0,X17,X18)))
     => p(state(h(n1,X0,X1,X2,X3,X4,n1),h(n1,n1,X7,X8,X9,X10,X11),X12,X13),iknows(X14,enc(X15,n1,X17,X18))) ) )).

fof(encrypt_hn2k1_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,n1,X5,X6,X7),X8),iknows(atoms(n1,X10),enc(n1,X12,X13,X14)))
     <= p(state(X0,X1,h(n1,X2,X3,n1,X5,X6,X7),X8),iknows(atoms(n1,X10),enc(n0,X12,X13,X14))) ) )).

fof(decrypt_hn1k1_k1Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,n1,X4,X5),X6,X7,X8),iknows(atoms(n0,X10),enc(n1,X12,X13,X14)))
     => p(state(h(n1,X0,X1,X2,n1,X4,X5),X6,X7,X8),iknows(atoms(n1,X10),enc(n1,X12,X13,X14))) ) )).

fof(wrap_hn1k1_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,n1,X1,X2,X3,X4,X5),h(n1,X6,X7,X8,X9,X10,n1),X12,X13),iknows(X14,enc(X15,X16,n0,X18)))
     => p(state(h(n1,n1,X1,X2,X3,X4,X5),h(n1,X6,X7,X8,X9,X10,n1),X12,X13),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(set_attr_hn2k1_extractable_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,X4,X5,X6,n0),X8),X9)
     <= p(state(X0,X1,h(n1,X2,X3,X4,X5,X6,n1),X8),X9) ) )).

fof(encrypt_hn2k2_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,X4,n1,X6,X7,X8)),iknows(atoms(n1,X10),enc(X11,n1,X13,X14)))
     <= p(state(X0,X1,X2,h(n1,X3,X4,n1,X6,X7,X8)),iknows(atoms(n1,X10),enc(X11,n0,X13,X14))) ) )).

fof(decrypt_hn2k2_k1Enck2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,X4,X5,n1,X7,X8)),iknows(atoms(n0,X10),enc(X11,n1,X13,X14)))
     => p(state(X0,X1,X2,h(n1,X3,X4,X5,n1,X7,X8)),iknows(atoms(n1,X10),enc(X11,n1,X13,X14))) ) )).

fof(unwrap_hn2k1_hn1k2_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,h(n0,X1,X2,X3,X4,X5,X6),h(n1,X7,n1,X9,X10,X11,X12),X13),iknows(X14,enc(X15,X16,n1,X18)))
     => p(state(X0,h(n1,X1,X2,X3,X4,X5,n1),h(n1,X7,n1,X9,X10,X11,X12),X13),iknows(X14,enc(X15,X16,n1,X18))) ) )).

fof(decrypt_hn1k1_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(h(n1,X0,X1,X2,n1,X4,X5),X6,X7,X8),iknows(atoms(X9,n0),enc(X11,X12,n1,X14)))
     => p(state(h(n1,X0,X1,X2,n1,X4,X5),X6,X7,X8),iknows(atoms(X9,n1),enc(X11,X12,n1,X14))) ) )).

fof(decrypt_hn2k1_k2Enck1_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,h(n1,X2,X3,X4,n1,X6,X7),X8),iknows(atoms(X9,n0),enc(X11,X12,n1,X14)))
     => p(state(X0,X1,h(n1,X2,X3,X4,n1,X6,X7),X8),iknows(atoms(X9,n1),enc(X11,X12,n1,X14))) ) )).

fof(set_attr_hn2k2_sensitive_command,axiom,(
    ! [X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,X34] :
      ( p(state(X0,X1,X2,h(n1,X3,X4,X5,X6,n0,X8)),X9)
     => p(state(X0,X1,X2,h(n1,X3,X4,X5,X6,n1,X8)),X9) ) )).

