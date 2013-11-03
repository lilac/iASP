fof(co1,conjecture,(
    ~ ~ ( ( ? [X12] :
              ( actual_world(X12)
              & ? [X13,X14,X15,X16,X17,X18,X19,X20] :
                  ( group(X12,X13)
                  & frontseat(X12,X16)
                  & city(X12,X14)
                  & hollywood_placename(X12,X15)
                  & placename(X12,X15)
                  & chevy(X12,X16)
                  & white(X12,X16)
                  & old(X12,X16)
                  & agent(X12,X18,X16)
                  & present(X12,X18)
                  & barrel(X12,X18)
                  & down(X12,X18,X17)
                  & ! [X21] :
                      ( member(X12,X21,X19)
                     => ? [X22,X23] :
                          ( state(X12,X22)
                          & in(X12,X23,X16)
                          & be(X12,X22,X21,X23) ) )
                  & two(X12,X19)
                  & group(X12,X19)
                  & ! [X25] :
                      ( ! [X26] :
                          ( member(X12,X26,X13)
                         => ? [X27] :
                              ( patient(X12,X27,X25)
                              & wear(X12,X27)
                              & nonreflexive(X12,X27)
                              & present(X12,X27)
                              & agent(X12,X27,X26)
                              & event(X12,X27) ) )
                     <= member(X12,X25,X20) )
                  & group(X12,X20)
                  & ! [X28] :
                      ( member(X12,X28,X20)
                     => ( coat(X12,X28)
                        & cheap(X12,X28)
                        & black(X12,X28) ) )
                  & ! [X24] :
                      ( member(X12,X24,X19)
                     => ( fellow(X12,X24)
                        & young(X12,X24) ) )
                  & in(X12,X18,X14)
                  & event(X12,X18)
                  & lonely(X12,X17)
                  & street(X12,X17)
                  & dirty(X12,X16)
                  & of(X12,X15,X14) ) )
         <= ? [U] :
              ( actual_world(U)
              & ? [V,W,X,Y,Z,X1,X2,X3] :
                  ( group(U,V)
                  & chevy(U,Y)
                  & white(U,Y)
                  & old(U,Y)
                  & street(U,Z)
                  & lonely(U,Z)
                  & present(U,X1)
                  & down(U,X1,Z)
                  & in(U,X1,W)
                  & ! [X4] :
                      ( member(U,X4,X2)
                     => ? [X5,X6] :
                          ( state(U,X5)
                          & be(U,X5,X4,X6)
                          & in(U,X6,Z) ) )
                  & two(U,X2)
                  & ! [X7] :
                      ( member(U,X7,X2)
                     => ( young(U,X7)
                        & fellow(U,X7) ) )
                  & ! [X8] :
                      ( member(U,X8,X3)
                     => ! [X9] :
                          ( ? [X10] :
                              ( event(U,X10)
                              & nonreflexive(U,X10)
                              & wear(U,X10)
                              & present(U,X10)
                              & patient(U,X10,X8)
                              & agent(U,X10,X9) )
                         <= member(U,X9,V) ) )
                  & ! [X11] :
                      ( member(U,X11,X3)
                     => ( coat(U,X11)
                        & black(U,X11)
                        & cheap(U,X11) ) )
                  & group(U,X3)
                  & group(U,X2)
                  & barrel(U,X1)
                  & agent(U,X1,Y)
                  & event(U,X1)
                  & dirty(U,Y)
                  & placename(U,X)
                  & hollywood_placename(U,X)
                  & city(U,W)
                  & of(U,X,W)
                  & frontseat(U,Z) ) ) )
        & ( ? [U] :
              ( actual_world(U)
              & ? [V,W,X,Y,Z,X1,X2,X3] :
                  ( hollywood_placename(U,X)
                  & chevy(U,Y)
                  & dirty(U,Y)
                  & street(U,Z)
                  & lonely(U,Z)
                  & event(U,X1)
                  & agent(U,X1,Y)
                  & two(U,X2)
                  & ! [X7] :
                      ( ( young(U,X7)
                        & fellow(U,X7) )
                     <= member(U,X7,X2) )
                  & group(U,X3)
                  & ! [X11] :
                      ( ( coat(U,X11)
                        & cheap(U,X11)
                        & black(U,X11) )
                     <= member(U,X11,X3) )
                  & ! [X8] :
                      ( ! [X9] :
                          ( ? [X10] :
                              ( event(U,X10)
                              & patient(U,X10,X8)
                              & present(U,X10)
                              & nonreflexive(U,X10)
                              & wear(U,X10)
                              & agent(U,X10,X9) )
                         <= member(U,X9,V) )
                     <= member(U,X8,X3) )
                  & group(U,X2)
                  & ! [X4] :
                      ( ? [X5,X6] :
                          ( be(U,X5,X4,X6)
                          & in(U,X6,Z)
                          & state(U,X5) )
                     <= member(U,X4,X2) )
                  & in(U,X1,W)
                  & down(U,X1,Z)
                  & barrel(U,X1)
                  & present(U,X1)
                  & old(U,Y)
                  & white(U,Y)
                  & placename(U,X)
                  & city(U,W)
                  & of(U,X,W)
                  & frontseat(U,Z)
                  & group(U,V) ) )
         <= ? [X12] :
              ( ? [X13,X14,X15,X16,X17,X18,X19,X20] :
                  ( group(X12,X13)
                  & frontseat(X12,X16)
                  & placename(X12,X15)
                  & chevy(X12,X16)
                  & dirty(X12,X16)
                  & barrel(X12,X18)
                  & in(X12,X18,X14)
                  & ! [X21] :
                      ( member(X12,X21,X19)
                     => ? [X22,X23] :
                          ( be(X12,X22,X21,X23)
                          & in(X12,X23,X16)
                          & state(X12,X22) ) )
                  & group(X12,X19)
                  & ! [X24] :
                      ( ( fellow(X12,X24)
                        & young(X12,X24) )
                     <= member(X12,X24,X19) )
                  & ! [X28] :
                      ( ( cheap(X12,X28)
                        & black(X12,X28)
                        & coat(X12,X28) )
                     <= member(X12,X28,X20) )
                  & group(X12,X20)
                  & ! [X25] :
                      ( ! [X26] :
                          ( ? [X27] :
                              ( event(X12,X27)
                              & agent(X12,X27,X26)
                              & present(X12,X27)
                              & nonreflexive(X12,X27)
                              & wear(X12,X27)
                              & patient(X12,X27,X25) )
                         <= member(X12,X26,X13) )
                     <= member(X12,X25,X20) )
                  & two(X12,X19)
                  & down(X12,X18,X17)
                  & present(X12,X18)
                  & agent(X12,X18,X16)
                  & event(X12,X18)
                  & lonely(X12,X17)
                  & street(X12,X17)
                  & old(X12,X16)
                  & white(X12,X16)
                  & hollywood_placename(X12,X15)
                  & city(X12,X14)
                  & of(X12,X15,X14) )
              & actual_world(X12) ) ) ) )).
