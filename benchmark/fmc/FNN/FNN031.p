fof(co1,conjecture,(
    ~ ~ ( ( ? [X4] :
              ( ? [X5] :
                  ( ! [X6] :
                      ( member(X4,X6,X5)
                     => ? [X7] :
                          ( ! [X8] :
                              ( ? [X9,X10] :
                                  ( table(X4,X9)
                                  & event(X4,X10)
                                  & sit(X4,X10)
                                  & at(X4,X10,X9)
                                  & with(X4,X10,X6)
                                  & present(X4,X10)
                                  & agent(X4,X10,X8) )
                             <= member(X4,X8,X7) )
                          & ! [X11] :
                              ( member(X4,X11,X7)
                             => ( guy(X4,X11)
                                & young(X4,X11) ) )
                          & group(X4,X7)
                          & three(X4,X7) ) )
                  & group(X4,X5)
                  & ! [X12] :
                      ( hamburger(X4,X12)
                     <= member(X4,X12,X5) ) )
              & actual_world(X4) )
         <= ? [U] :
              ( ? [V] :
                  ( ! [W] :
                      ( member(U,W,V)
                     => ? [X,Y] :
                          ( table(U,X)
                          & ! [Z] :
                              ( member(U,Z,Y)
                             => ? [X1] :
                                  ( event(U,X1)
                                  & agent(U,X1,W)
                                  & present(U,X1)
                                  & sit(U,X1)
                                  & at(U,X1,X)
                                  & with(U,X1,Z) ) )
                          & ! [X2] :
                              ( hamburger(U,X2)
                             <= member(U,X2,Y) )
                          & group(U,Y) ) )
                  & group(U,V)
                  & ! [X3] :
                      ( ( young(U,X3)
                        & guy(U,X3) )
                     <= member(U,X3,V) )
                  & three(U,V) )
              & actual_world(U) ) )
        & ( ? [X4] :
              ( ? [X5] :
                  ( group(X4,X5)
                  & ! [X12] :
                      ( member(X4,X12,X5)
                     => hamburger(X4,X12) )
                  & ! [X6] :
                      ( member(X4,X6,X5)
                     => ? [X7] :
                          ( ! [X8] :
                              ( ? [X9,X10] :
                                  ( table(X4,X9)
                                  & event(X4,X10)
                                  & agent(X4,X10,X8)
                                  & present(X4,X10)
                                  & sit(X4,X10)
                                  & with(X4,X10,X6)
                                  & at(X4,X10,X9) )
                             <= member(X4,X8,X7) )
                          & three(X4,X7)
                          & ! [X11] :
                              ( member(X4,X11,X7)
                             => ( young(X4,X11)
                                & guy(X4,X11) ) )
                          & group(X4,X7) ) ) )
              & actual_world(X4) )
         => ? [U] :
              ( ? [V] :
                  ( ! [W] :
                      ( member(U,W,V)
                     => ? [X,Y] :
                          ( ! [Z] :
                              ( member(U,Z,Y)
                             => ? [X1] :
                                  ( present(U,X1)
                                  & at(U,X1,X)
                                  & with(U,X1,Z)
                                  & sit(U,X1)
                                  & agent(U,X1,W)
                                  & event(U,X1) ) )
                          & ! [X2] :
                              ( member(U,X2,Y)
                             => hamburger(U,X2) )
                          & group(U,Y)
                          & table(U,X) ) )
                  & three(U,V)
                  & ! [X3] :
                      ( member(U,X3,V)
                     => ( young(U,X3)
                        & guy(U,X3) ) )
                  & group(U,V) )
              & actual_world(U) ) ) ) )).

