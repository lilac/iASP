fof(co1,conjecture,(
    ~ ~ ( ( ? [X12] :
              ( actual_world(X12)
              & ? [X13,X14,X15,X16,X17,X18,X19,X20] :
                  ( of(X12,X15,X14)
                  & placename(X12,X15)
                  & chevy(X12,X16)
                  & old(X12,X16)
                  & event(X12,X18)
                  & barrel(X12,X18)
                  & down(X12,X18,X17)
                  & ! [X24] :
                      ( ( young(X12,X24)
                        & fellow(X12,X24) )
                     <= member(X12,X24,X19) )
                  & ! [X25] :
                      ( member(X12,X25,X20)
                     => ! [X26] :
                          ( ? [X27] :
                              ( agent(X12,X27,X26)
                              & patient(X12,X27,X25)
                              & present(X12,X27)
                              & nonreflexive(X12,X27)
                              & wear(X12,X27)
                              & event(X12,X27) )
                         <= member(X12,X26,X13) ) )
                  & group(X12,X20)
                  & ! [X28] :
                      ( ( coat(X12,X28)
                        & black(X12,X28)
                        & cheap(X12,X28) )
                     <= member(X12,X28,X20) )
                  & group(X12,X19)
                  & two(X12,X19)
                  & ! [X21] :
                      ( ? [X22,X23] :
                          ( state(X12,X22)
                          & be(X12,X22,X21,X23)
                          & in(X12,X23,X14) )
                     <= member(X12,X21,X19) )
                  & in(X12,X18,X14)
                  & present(X12,X18)
                  & agent(X12,X18,X16)
                  & lonely(X12,X17)
                  & street(X12,X17)
                  & dirty(X12,X16)
                  & white(X12,X16)
                  & hollywood_placename(X12,X15)
                  & city(X12,X14)
                  & frontseat(X12,X14)
                  & group(X12,X13) ) )
         <= ? [U] :
              ( actual_world(U)
              & ? [V,W,X,Y,Z,X1,X2,X3] :
                  ( group(U,V)
                  & frontseat(U,Y)
                  & of(U,X,W)
                  & city(U,W)
                  & hollywood_placename(U,X)
                  & placename(U,X)
                  & old(U,Y)
                  & present(U,X1)
                  & barrel(U,X1)
                  & in(U,X1,W)
                  & ! [X4] :
                      ( member(U,X4,X2)
                     => ? [X5,X6] :
                          ( state(U,X5)
                          & in(U,X6,Y)
                          & be(U,X5,X4,X6) ) )
                  & ! [X7] :
                      ( ( young(U,X7)
                        & fellow(U,X7) )
                     <= member(U,X7,X2) )
                  & ! [X8] :
                      ( ! [X9] :
                          ( member(U,X9,V)
                         => ? [X10] :
                              ( agent(U,X10,X9)
                              & patient(U,X10,X8)
                              & present(U,X10)
                              & nonreflexive(U,X10)
                              & wear(U,X10)
                              & event(U,X10) ) )
                     <= member(U,X8,X3) )
                  & ! [X11] :
                      ( ( cheap(U,X11)
                        & black(U,X11)
                        & coat(U,X11) )
                     <= member(U,X11,X3) )
                  & group(U,X3)
                  & group(U,X2)
                  & two(U,X2)
                  & down(U,X1,Z)
                  & agent(U,X1,Y)
                  & event(U,X1)
                  & lonely(U,Z)
                  & street(U,Z)
                  & dirty(U,Y)
                  & white(U,Y)
                  & chevy(U,Y) ) ) )
        & ( ? [U] :
              ( ? [V,W,X,Y,Z,X1,X2,X3] :
                  ( group(U,V)
                  & city(U,W)
                  & placename(U,X)
                  & chevy(U,Y)
                  & old(U,Y)
                  & street(U,Z)
                  & lonely(U,Z)
                  & event(U,X1)
                  & agent(U,X1,Y)
                  & present(U,X1)
                  & down(U,X1,Z)
                  & in(U,X1,W)
                  & ! [X4] :
                      ( member(U,X4,X2)
                     => ? [X5,X6] :
                          ( in(U,X6,Y)
                          & be(U,X5,X4,X6)
                          & state(U,X5) ) )
                  & two(U,X2)
                  & group(U,X2)
                  & ! [X7] :
                      ( member(U,X7,X2)
                     => ( fellow(U,X7)
                        & young(U,X7) ) )
                  & ! [X8] :
                      ( ! [X9] :
                          ( member(U,X9,V)
                         => ? [X10] :
                              ( agent(U,X10,X9)
                              & present(U,X10)
                              & wear(U,X10)
                              & nonreflexive(U,X10)
                              & patient(U,X10,X8)
                              & event(U,X10) ) )
                     <= member(U,X8,X3) )
                  & group(U,X3)
                  & ! [X11] :
                      ( ( black(U,X11)
                        & cheap(U,X11)
                        & coat(U,X11) )
                     <= member(U,X11,X3) )
                  & barrel(U,X1)
                  & dirty(U,Y)
                  & white(U,Y)
                  & hollywood_placename(U,X)
                  & of(U,X,W)
                  & frontseat(U,Y) )
              & actual_world(U) )
         <= ? [X12] :
              ( ? [X13,X14,X15,X16,X17,X18,X19,X20] :
                  ( group(X12,X13)
                  & frontseat(X12,X14)
                  & of(X12,X15,X14)
                  & city(X12,X14)
                  & white(X12,X16)
                  & dirty(X12,X16)
                  & old(X12,X16)
                  & street(X12,X17)
                  & event(X12,X18)
                  & agent(X12,X18,X16)
                  & present(X12,X18)
                  & barrel(X12,X18)
                  & ! [X21] :
                      ( ? [X22,X23] :
                          ( state(X12,X22)
                          & be(X12,X22,X21,X23)
                          & in(X12,X23,X14) )
                     <= member(X12,X21,X19) )
                  & group(X12,X20)
                  & ! [X28] :
                      ( member(X12,X28,X20)
                     => ( cheap(X12,X28)
                        & black(X12,X28)
                        & coat(X12,X28) ) )
                  & ! [X25] :
                      ( ! [X26] :
                          ( ? [X27] :
                              ( event(X12,X27)
                              & agent(X12,X27,X26)
                              & patient(X12,X27,X25)
                              & wear(X12,X27)
                              & nonreflexive(X12,X27)
                              & present(X12,X27) )
                         <= member(X12,X26,X13) )
                     <= member(X12,X25,X20) )
                  & ! [X24] :
                      ( ( young(X12,X24)
                        & fellow(X12,X24) )
                     <= member(X12,X24,X19) )
                  & group(X12,X19)
                  & two(X12,X19)
                  & in(X12,X18,X14)
                  & down(X12,X18,X17)
                  & lonely(X12,X17)
                  & chevy(X12,X16)
                  & placename(X12,X15)
                  & hollywood_placename(X12,X15) )
              & actual_world(X12) ) ) ) )).
