fof(co1,conjecture,(
    ~ ~ ( ( ? [X4] :
              ( actual_world(X4)
              & ? [X5] :
                  ( ! [X12] :
                      ( member(X4,X12,X5)
                     => hamburger(X4,X12) )
                  & group(X4,X5)
                  & ! [X6] :
                      ( ? [X7] :
                          ( group(X4,X7)
                          & ! [X11] :
                              ( ( young(X4,X11)
                                & guy(X4,X11) )
                             <= member(X4,X11,X7) )
                          & three(X4,X7)
                          & ! [X8] :
                              ( member(X4,X8,X7)
                             => ? [X9,X10] :
                                  ( agent(X4,X10,X8)
                                  & sit(X4,X10)
                                  & at(X4,X10,X9)
                                  & with(X4,X10,X6)
                                  & present(X4,X10)
                                  & event(X4,X10)
                                  & table(X4,X9) ) ) )
                     <= member(X4,X6,X5) ) ) )
         <= ? [U] :
              ( ? [V,W] :
                  ( table(U,V)
                  & group(U,W)
                  & ! [X3] :
                      ( ( guy(U,X3)
                        & young(U,X3) )
                     <= member(U,X3,W) )
                  & three(U,W)
                  & ! [X] :
                      ( member(U,X,W)
                     => ? [Y] :
                          ( ! [X2] :
                              ( hamburger(U,X2)
                             <= member(U,X2,Y) )
                          & group(U,Y)
                          & ! [Z] :
                              ( ? [X1] :
                                  ( event(U,X1)
                                  & agent(U,X1,X)
                                  & at(U,X1,V)
                                  & with(U,X1,Z)
                                  & sit(U,X1)
                                  & present(U,X1) )
                             <= member(U,Z,Y) ) ) ) )
              & actual_world(U) ) )
        & ( ? [U] :
              ( ? [V,W] :
                  ( table(U,V)
                  & ! [X] :
                      ( ? [Y] :
                          ( ! [Z] :
                              ( ? [X1] :
                                  ( present(U,X1)
                                  & sit(U,X1)
                                  & at(U,X1,V)
                                  & with(U,X1,Z)
                                  & agent(U,X1,X)
                                  & event(U,X1) )
                             <= member(U,Z,Y) )
                          & group(U,Y)
                          & ! [X2] :
                              ( member(U,X2,Y)
                             => hamburger(U,X2) ) )
                     <= member(U,X,W) )
                  & three(U,W)
                  & ! [X3] :
                      ( ( young(U,X3)
                        & guy(U,X3) )
                     <= member(U,X3,W) )
                  & group(U,W) )
              & actual_world(U) )
         <= ? [X4] :
              ( actual_world(X4)
              & ? [X5] :
                  ( ! [X12] :
                      ( member(X4,X12,X5)
                     => hamburger(X4,X12) )
                  & group(X4,X5)
                  & ! [X6] :
                      ( member(X4,X6,X5)
                     => ? [X7] :
                          ( three(X4,X7)
                          & group(X4,X7)
                          & ! [X11] :
                              ( ( young(X4,X11)
                                & guy(X4,X11) )
                             <= member(X4,X11,X7) )
                          & ! [X8] :
                              ( ? [X9,X10] :
                                  ( event(X4,X10)
                                  & agent(X4,X10,X8)
                                  & with(X4,X10,X6)
                                  & at(X4,X10,X9)
                                  & sit(X4,X10)
                                  & present(X4,X10)
                                  & table(X4,X9) )
                             <= member(X4,X8,X7) ) ) ) ) ) ) ) )).

