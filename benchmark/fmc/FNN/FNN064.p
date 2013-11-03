fof(co1,conjecture,(
    ~ ~ ( ( ? [U] :
              ( ? [V] :
                  ( ! [X3] :
                      ( hamburger(U,X3)
                     <= member(U,X3,V) )
                  & group(U,V)
                  & ! [W] :
                      ( ? [X,Y] :
                          ( ! [X2] :
                              ( member(U,X2,Y)
                             => ( young(U,X2)
                                & guy(U,X2) ) )
                          & group(U,Y)
                          & three(U,Y)
                          & ! [Z] :
                              ( member(U,Z,Y)
                             => ? [X1] :
                                  ( event(U,X1)
                                  & sit(U,X1)
                                  & at(U,X1,X)
                                  & with(U,X1,W)
                                  & present(U,X1)
                                  & agent(U,X1,Z) ) )
                          & table(U,X) )
                     <= member(U,W,V) ) )
              & actual_world(U) )
         <= ? [X4] :
              ( ? [X5] :
                  ( ! [X12] :
                      ( member(X4,X12,X5)
                     => hamburger(X4,X12) )
                  & group(X4,X5)
                  & ! [X6] :
                      ( ? [X7] :
                          ( ! [X8] :
                              ( member(X4,X8,X7)
                             => ? [X9,X10] :
                                  ( agent(X4,X10,X8)
                                  & present(X4,X10)
                                  & sit(X4,X10)
                                  & at(X4,X10,X9)
                                  & with(X4,X10,X6)
                                  & event(X4,X10)
                                  & table(X4,X9) ) )
                          & ! [X11] :
                              ( member(X4,X11,X7)
                             => ( guy(X4,X11)
                                & young(X4,X11) ) )
                          & group(X4,X7)
                          & three(X4,X7) )
                     <= member(X4,X6,X5) ) )
              & actual_world(X4) ) )
        & ( ? [U] :
              ( ? [V] :
                  ( ! [W] :
                      ( ? [X,Y] :
                          ( ! [X2] :
                              ( member(U,X2,Y)
                             => ( guy(U,X2)
                                & young(U,X2) ) )
                          & group(U,Y)
                          & three(U,Y)
                          & ! [Z] :
                              ( ? [X1] :
                                  ( event(U,X1)
                                  & at(U,X1,X)
                                  & with(U,X1,W)
                                  & sit(U,X1)
                                  & present(U,X1)
                                  & agent(U,X1,Z) )
                             <= member(U,Z,Y) )
                          & table(U,X) )
                     <= member(U,W,V) )
                  & group(U,V)
                  & ! [X3] :
                      ( hamburger(U,X3)
                     <= member(U,X3,V) ) )
              & actual_world(U) )
         => ? [X4] :
              ( actual_world(X4)
              & ? [X5] :
                  ( ! [X6] :
                      ( ? [X7] :
                          ( ! [X8] :
                              ( member(X4,X8,X7)
                             => ? [X9,X10] :
                                  ( table(X4,X9)
                                  & present(X4,X10)
                                  & with(X4,X10,X6)
                                  & at(X4,X10,X9)
                                  & sit(X4,X10)
                                  & agent(X4,X10,X8)
                                  & event(X4,X10) ) )
                          & three(X4,X7)
                          & ! [X11] :
                              ( member(X4,X11,X7)
                             => ( guy(X4,X11)
                                & young(X4,X11) ) )
                          & group(X4,X7) )
                     <= member(X4,X6,X5) )
                  & group(X4,X5)
                  & ! [X12] :
                      ( hamburger(X4,X12)
                     <= member(X4,X12,X5) ) ) ) ) ) )).

