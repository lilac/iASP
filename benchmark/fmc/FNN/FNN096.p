fof(main,conjecture,(
    ~ ? [X] :
        ~ ( ! [Y] :
              ( ~ ! [X] :
                    ( ~ r1(Y,X)
                    | p3(X) )
              | ~ r1(X,Y) )
          | ! [Y] :
              ( ~ ! [X] :
                    ( p2(X)
                    | ~ r1(Y,X) )
              | ~ r1(X,Y) )
          | ! [Y] :
              ( p2(Y)
              | ~ r1(X,Y) )
          | ! [Y] :
              ( ~ ! [X] :
                    ( ~ r1(Y,X)
                    | p1(X) )
              | ~ r1(X,Y) )
          | ! [Y] :
              ( ~ r1(X,Y)
              | p1(Y) )
          | ~ ( ( ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( p1(Y)
                                | ! [X] :
                                    ( p3(X)
                                    | p2(X)
                                    | ! [Y] :
                                        ( p4(Y)
                                        | p3(Y)
                                        | p2(Y)
                                        | p1(Y)
                                        | ! [X] :
                                            ( ~ r1(Y,X)
                                            | p3(X)
                                            | ! [Y] :
                                                ( ~ r1(X,Y)
                                                | $false )
                                            | p1(X)
                                            | p2(X)
                                            | p4(X) )
                                        | ~ r1(X,Y) )
                                    | p1(X)
                                    | p4(X)
                                    | ~ r1(Y,X) )
                                | ~ r1(X,Y) )
                            | ~ ( ! [Y] :
                                    ( ~ r1(X,Y)
                                    | p2(Y)
                                    | p1(Y)
                                    | ! [X] :
                                        ( p4(X)
                                        | p3(X)
                                        | p2(X)
                                        | ! [Y] :
                                            ( ~ r1(X,Y)
                                            | p1(Y)
                                            | ! [X] :
                                                ( $false
                                                | ~ r1(Y,X) )
                                            | p2(Y)
                                            | p3(Y)
                                            | p4(Y) )
                                        | p1(X)
                                        | ~ r1(Y,X) )
                                    | p3(Y)
                                    | p4(Y) )
                                | p1(X) ) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | p4(X)
                          | p3(X)
                          | ! [Y] :
                              ( p4(Y)
                              | ! [X] :
                                  ( ~ r1(Y,X)
                                  | p3(X)
                                  | p1(X)
                                  | ! [Y] :
                                      ( ~ r1(X,Y)
                                      | $false )
                                  | p2(X)
                                  | p4(X) )
                              | p1(Y)
                              | p2(Y)
                              | p3(Y)
                              | ~ r1(X,Y) )
                          | p1(X)
                          | p2(X) )
                      | p1(Y) )
                | ! [Y] :
                    ( ~ r1(X,Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | p3(X)
                        | p1(X)
                        | ! [Y] :
                            ( p4(Y)
                            | p3(Y)
                            | p2(Y)
                            | p1(Y)
                            | ! [X] :
                                ( p1(X)
                                | ! [Y] :
                                    ( $false
                                    | ~ r1(X,Y) )
                                | p2(X)
                                | p3(X)
                                | p4(X)
                                | ~ r1(Y,X) )
                            | ~ r1(X,Y) )
                        | p2(X)
                        | p4(X) )
                    | p1(Y) ) )
              & ( ! [Y] :
                    ( ~ r1(X,Y)
                    | p4(Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | p4(X)
                        | p3(X)
                        | p1(X)
                        | ! [Y] :
                            ( p4(Y)
                            | p3(Y)
                            | p2(Y)
                            | ! [X] :
                                ( ~ r1(Y,X)
                                | $false )
                            | p1(Y)
                            | ~ r1(X,Y) )
                        | p2(X) )
                    | p1(Y)
                    | p2(Y)
                    | p3(Y) )
                | ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | p4(Y)
                      | p3(Y)
                      | p1(Y)
                      | ! [X] :
                          ( p4(X)
                          | p1(X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | p1(Y)
                              | ! [X] :
                                  ( $false
                                  | ~ r1(Y,X) )
                              | p2(Y)
                              | p3(Y)
                              | p4(Y) )
                          | p2(X)
                          | p3(X)
                          | ~ r1(Y,X) )
                      | ~ ! [X] :
                            ( ! [Y] :
                                ( p4(Y)
                                | p3(Y)
                                | p1(Y)
                                | ! [X] :
                                    ( ~ r1(Y,X)
                                    | p3(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | p3(Y)
                                        | ! [X] :
                                            ( ~ r1(Y,X)
                                            | $false )
                                        | p1(Y)
                                        | p2(Y)
                                        | p4(Y) )
                                    | p2(X)
                                    | p4(X) )
                                | p2(Y)
                                | ~ r1(X,Y) )
                            | ~ ( p3(X)
                                | p2(X)
                                | p1(X)
                                | ! [Y] :
                                    ( ! [X] :
                                        ( ~ r1(Y,X)
                                        | p4(X)
                                        | p3(X)
                                        | ! [Y] :
                                            ( ~ r1(X,Y)
                                            | $false )
                                        | p1(X)
                                        | p2(X) )
                                    | p1(Y)
                                    | p2(Y)
                                    | p3(Y)
                                    | p4(Y)
                                    | ~ r1(X,Y) )
                                | p4(X) )
                            | ~ r1(Y,X) )
                      | p2(Y) ) )
              & ( ~ ! [Y] :
                      ( p1(Y)
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ~ ( p1(X)
                                | ! [Y] :
                                    ( ! [X] :
                                        ( ~ r1(Y,X)
                                        | p4(X)
                                        | p3(X)
                                        | p2(X)
                                        | ! [Y] :
                                            ( $false
                                            | ~ r1(X,Y) )
                                        | p1(X) )
                                    | p1(Y)
                                    | p2(Y)
                                    | p3(Y)
                                    | p4(Y)
                                    | ~ r1(X,Y) ) )
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | ! [X] :
                                    ( p2(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | p4(Y)
                                        | p2(Y)
                                        | ! [X] :
                                            ( ~ r1(Y,X)
                                            | $false )
                                        | p1(Y)
                                        | p3(Y) )
                                    | p3(X)
                                    | p4(X)
                                    | ~ r1(Y,X) )
                                | p1(Y) ) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | p3(Y)
                              | ! [X] :
                                  ( ~ r1(Y,X)
                                  | $false )
                              | p1(Y)
                              | p2(Y)
                              | p4(Y) )
                          | p1(X)
                          | p2(X)
                          | p3(X)
                          | p4(X) )
                      | ~ r1(X,Y) )
                | ! [Y] :
                    ( ! [X] :
                        ( p4(X)
                        | p3(X)
                        | p2(X)
                        | p1(X)
                        | ! [Y] :
                            ( ~ r1(X,Y)
                            | p4(Y)
                            | p1(Y)
                            | ! [X] :
                                ( $false
                                | ~ r1(Y,X) )
                            | p2(Y)
                            | p3(Y) )
                        | ~ r1(Y,X) )
                    | p1(Y)
                    | ~ r1(X,Y) ) )
              & ( ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | p4(Y)
                      | p3(Y)
                      | p2(Y)
                      | p1(Y)
                      | ! [X] :
                          ( p4(X)
                          | p3(X)
                          | p1(X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | $false )
                          | p2(X)
                          | ~ r1(Y,X) )
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | p4(Y)
                                | p2(Y)
                                | p1(Y)
                                | ! [X] :
                                    ( ~ r1(Y,X)
                                    | p4(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | $false )
                                    | p2(X)
                                    | p3(X) )
                                | p3(Y) )
                            | ~ ( p4(X)
                                | p2(X)
                                | p1(X)
                                | ! [Y] :
                                    ( p3(Y)
                                    | ! [X] :
                                        ( $false
                                        | ~ r1(Y,X) )
                                    | p1(Y)
                                    | p2(Y)
                                    | p4(Y)
                                    | ~ r1(X,Y) )
                                | p3(X) ) ) )
                | ! [Y] :
                    ( ~ r1(X,Y)
                    | p4(Y)
                    | p2(Y)
                    | p1(Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | p4(X)
                        | p3(X)
                        | p1(X)
                        | ! [Y] :
                            ( ~ r1(X,Y)
                            | $false )
                        | p2(X) )
                    | p3(Y) ) )
              & ( ! [Y] :
                    ( p3(Y)
                    | p1(Y)
                    | ! [X] :
                        ( p4(X)
                        | p3(X)
                        | ! [Y] :
                            ( $false
                            | ~ r1(X,Y) )
                        | p1(X)
                        | p2(X)
                        | ~ r1(Y,X) )
                    | p2(Y)
                    | ~ r1(X,Y) )
                | ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | p2(Y)
                      | ! [X] :
                          ( p1(X)
                          | ! [Y] :
                              ( $false
                              | ~ r1(X,Y) )
                          | p2(X)
                          | p3(X)
                          | p4(X)
                          | ~ r1(Y,X) )
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( p3(Y)
                                | ! [X] :
                                    ( p3(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | $false )
                                    | p2(X)
                                    | p4(X)
                                    | ~ r1(Y,X) )
                                | p1(Y)
                                | p2(Y)
                                | ~ r1(X,Y) )
                            | ~ ( ! [Y] :
                                    ( p3(Y)
                                    | p1(Y)
                                    | ! [X] :
                                        ( ~ r1(Y,X)
                                        | $false )
                                    | p2(Y)
                                    | p4(Y)
                                    | ~ r1(X,Y) )
                                | p1(X)
                                | p2(X)
                                | p3(X) ) )
                      | p1(Y)
                      | p3(Y) ) )
              & ( ~ ! [Y] :
                      ( p2(Y)
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | ! [X] :
                                    ( $false
                                    | ~ r1(Y,X) )
                                | p1(Y)
                                | p2(Y)
                                | p3(Y) )
                            | ~ ( p2(X)
                                | p1(X)
                                | ! [Y] :
                                    ( $false
                                    | ~ r1(X,Y) )
                                | p3(X) ) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | $false )
                      | p1(Y)
                      | p3(Y)
                      | ~ r1(X,Y) )
                | ! [Y] :
                    ( p3(Y)
                    | p2(Y)
                    | p1(Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | $false )
                    | ~ r1(X,Y) ) )
              & ( ~ ! [Y] :
                      ( p1(Y)
                      | ! [X] :
                          ( $false
                          | ~ r1(Y,X) )
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ~ ( p2(X)
                                | p1(X)
                                | ! [Y] :
                                    ( ~ r1(X,Y)
                                    | $false ) )
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | p2(Y)
                                | p1(Y)
                                | ! [X] :
                                    ( ~ r1(Y,X)
                                    | $false ) ) )
                      | p2(Y)
                      | ~ r1(X,Y) )
                | ! [Y] :
                    ( p2(Y)
                    | p1(Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | $false )
                    | ~ r1(X,Y) ) )
              & ! [Y] :
                  ( ~ ! [X] :
                        ( ~ r1(Y,X)
                        | ~ ! [Y] :
                              ( ~ r1(X,Y)
                              | ! [X] :
                                  ( p2(X)
                                  | ~ r1(Y,X) )
                              | ~ p2(Y) )
                        | p2(X) )
                  | ! [X] :
                      ( ~ r1(Y,X)
                      | p2(X) )
                  | ~ r1(X,Y) )
              & ( ! [Y] :
                    ( ! [X] :
                        ( ~ r1(Y,X)
                        | $false )
                    | p1(Y)
                    | ~ r1(X,Y) )
                | ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ~ ( p1(X)
                                | ! [Y] :
                                    ( ~ r1(X,Y)
                                    | $false ) )
                            | ! [Y] :
                                ( p1(Y)
                                | ! [X] :
                                    ( $false
                                    | ~ r1(Y,X) )
                                | ~ r1(X,Y) ) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | $false )
                      | p1(Y) ) )
              & ( ~ ! [Y] :
                      ( p3(Y)
                      | p2(Y)
                      | ~ ! [X] :
                            ( ! [Y] :
                                ( ~ r1(X,Y)
                                | p4(Y)
                                | p3(Y)
                                | p2(Y)
                                | ! [X] :
                                    ( ~ r1(Y,X)
                                    | $false )
                                | p1(Y) )
                            | ~ ( p4(X)
                                | p3(X)
                                | p2(X)
                                | ! [Y] :
                                    ( ~ r1(X,Y)
                                    | $false )
                                | p1(X) )
                            | ~ r1(Y,X) )
                      | ! [X] :
                          ( $false
                          | ~ r1(Y,X) )
                      | p1(Y)
                      | p4(Y)
                      | ~ r1(X,Y) )
                | ! [Y] :
                    ( p4(Y)
                    | p3(Y)
                    | p1(Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | $false )
                    | p2(Y)
                    | ~ r1(X,Y) ) )
              & ( ~ ! [Y] :
                      ( ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | p1(Y)
                                | ! [X] :
                                    ( ~ r1(Y,X)
                                    | p2(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | $false )
                                    | p1(X)
                                    | p3(X)
                                    | p4(X) ) )
                            | ~ ( p1(X)
                                | ! [Y] :
                                    ( ! [X] :
                                        ( ~ r1(Y,X)
                                        | $false )
                                    | p1(Y)
                                    | p2(Y)
                                    | p3(Y)
                                    | p4(Y)
                                    | ~ r1(X,Y) ) ) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | p1(X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | $false )
                          | p2(X)
                          | p3(X)
                          | p4(X) )
                      | p1(Y)
                      | ~ r1(X,Y) )
                | ! [Y] :
                    ( p1(Y)
                    | ! [X] :
                        ( p3(X)
                        | p2(X)
                        | ! [Y] :
                            ( $false
                            | ~ r1(X,Y) )
                        | p1(X)
                        | p4(X)
                        | ~ r1(Y,X) )
                    | ~ r1(X,Y) ) )
              & ( ~ ! [Y] :
                      ( p1(Y)
                      | ! [X] :
                          ( p3(X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | $false )
                          | p1(X)
                          | p2(X)
                          | p4(X)
                          | ~ r1(Y,X) )
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ! [X] :
                                    ( p3(X)
                                    | p2(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | $false )
                                    | p4(X)
                                    | ~ r1(Y,X) )
                                | p1(Y)
                                | p2(Y)
                                | ~ r1(X,Y) )
                            | ~ ( p1(X)
                                | ! [Y] :
                                    ( p4(Y)
                                    | p3(Y)
                                    | p2(Y)
                                    | ! [X] :
                                        ( $false
                                        | ~ r1(Y,X) )
                                    | p1(Y)
                                    | ~ r1(X,Y) )
                                | p2(X) ) )
                      | p2(Y)
                      | ~ r1(X,Y) )
                | ! [Y] :
                    ( ~ r1(X,Y)
                    | p2(Y)
                    | p1(Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | p4(X)
                        | p2(X)
                        | ! [Y] :
                            ( ~ r1(X,Y)
                            | $false )
                        | p1(X)
                        | p3(X) ) ) )
              & ( ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | ! [X] :
                          ( p3(X)
                          | ! [Y] :
                              ( p3(Y)
                              | p1(Y)
                              | ! [X] :
                                  ( $false
                                  | ~ r1(Y,X) )
                              | p2(Y)
                              | p4(Y)
                              | ~ r1(X,Y) )
                          | p1(X)
                          | p2(X)
                          | p4(X)
                          | ~ r1(Y,X) )
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( p2(Y)
                                | p1(Y)
                                | ! [X] :
                                    ( p4(X)
                                    | p2(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | p4(Y)
                                        | p3(Y)
                                        | p1(Y)
                                        | ! [X] :
                                            ( $false
                                            | ~ r1(Y,X) )
                                        | p2(Y) )
                                    | p3(X)
                                    | ~ r1(Y,X) )
                                | ~ r1(X,Y) )
                            | ~ ( p1(X)
                                | ! [Y] :
                                    ( p4(Y)
                                    | p2(Y)
                                    | ! [X] :
                                        ( p4(X)
                                        | p3(X)
                                        | ! [Y] :
                                            ( $false
                                            | ~ r1(X,Y) )
                                        | p1(X)
                                        | p2(X)
                                        | ~ r1(Y,X) )
                                    | p1(Y)
                                    | p3(Y)
                                    | ~ r1(X,Y) )
                                | p2(X) ) )
                      | p1(Y)
                      | p2(Y) )
                | ! [Y] :
                    ( ~ r1(X,Y)
                    | ! [X] :
                        ( ~ r1(Y,X)
                        | p4(X)
                        | p1(X)
                        | ! [Y] :
                            ( ~ r1(X,Y)
                            | p3(Y)
                            | p1(Y)
                            | ! [X] :
                                ( ~ r1(Y,X)
                                | $false )
                            | p2(Y)
                            | p4(Y) )
                        | p2(X)
                        | p3(X) )
                    | p1(Y)
                    | p2(Y) ) )
              & ( ! [Y] :
                    ( ~ r1(X,Y)
                    | p3(Y)
                    | ! [X] :
                        ( p3(X)
                        | p1(X)
                        | ! [Y] :
                            ( p2(Y)
                            | p1(Y)
                            | ! [X] :
                                ( $false
                                | ~ r1(Y,X) )
                            | p3(Y)
                            | p4(Y)
                            | ~ r1(X,Y) )
                        | p2(X)
                        | p4(X)
                        | ~ r1(Y,X) )
                    | p1(Y)
                    | p2(Y) )
                | ~ ! [Y] :
                      ( p3(Y)
                      | p2(Y)
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | p3(Y)
                                | ! [X] :
                                    ( p4(X)
                                    | p2(X)
                                    | ! [Y] :
                                        ( ~ r1(X,Y)
                                        | p4(Y)
                                        | p2(Y)
                                        | ! [X] :
                                            ( ~ r1(Y,X)
                                            | $false )
                                        | p1(Y)
                                        | p3(Y) )
                                    | p1(X)
                                    | p3(X)
                                    | ~ r1(Y,X) )
                                | p1(Y)
                                | p2(Y) )
                            | ~ ( p2(X)
                                | p1(X)
                                | ! [Y] :
                                    ( p4(Y)
                                    | ! [X] :
                                        ( ~ r1(Y,X)
                                        | p3(X)
                                        | p2(X)
                                        | ! [Y] :
                                            ( ~ r1(X,Y)
                                            | $false )
                                        | p1(X)
                                        | p4(X) )
                                    | p1(Y)
                                    | p2(Y)
                                    | p3(Y)
                                    | ~ r1(X,Y) )
                                | p3(X) ) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | p3(Y)
                              | p2(Y)
                              | ! [X] :
                                  ( ~ r1(Y,X)
                                  | $false )
                              | p1(Y)
                              | p4(Y) )
                          | p1(X)
                          | p2(X)
                          | p3(X)
                          | p4(X) )
                      | p1(Y)
                      | ~ r1(X,Y) ) )
              & ( ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | p2(Y)
                      | p1(Y)
                      | ~ ! [X] :
                            ( ~ ( p2(X)
                                | ! [Y] :
                                    ( ~ r1(X,Y)
                                    | p2(Y)
                                    | ! [X] :
                                        ( ~ r1(Y,X)
                                        | ! [Y] :
                                            ( p4(Y)
                                            | p3(Y)
                                            | ! [X] :
                                                ( ~ r1(Y,X)
                                                | $false )
                                            | p1(Y)
                                            | p2(Y)
                                            | ~ r1(X,Y) )
                                        | p1(X)
                                        | p2(X)
                                        | p3(X)
                                        | p4(X) )
                                    | p1(Y)
                                    | p3(Y)
                                    | p4(Y) )
                                | p1(X) )
                            | ! [Y] :
                                ( p2(Y)
                                | p1(Y)
                                | ! [X] :
                                    ( ~ r1(Y,X)
                                    | p4(X)
                                    | p3(X)
                                    | p2(X)
                                    | p1(X)
                                    | ! [Y] :
                                        ( p2(Y)
                                        | ! [X] :
                                            ( p3(X)
                                            | ! [Y] :
                                                ( $false
                                                | ~ r1(X,Y) )
                                            | p1(X)
                                            | p2(X)
                                            | p4(X)
                                            | ~ r1(Y,X) )
                                        | p1(Y)
                                        | p3(Y)
                                        | p4(Y)
                                        | ~ r1(X,Y) ) )
                                | ~ r1(X,Y) )
                            | ~ r1(Y,X) )
                      | ! [X] :
                          ( ~ r1(Y,X)
                          | p3(X)
                          | ! [Y] :
                              ( ~ r1(X,Y)
                              | p4(Y)
                              | p2(Y)
                              | p1(Y)
                              | ! [X] :
                                  ( p4(X)
                                  | p2(X)
                                  | p1(X)
                                  | ! [Y] :
                                      ( ~ r1(X,Y)
                                      | $false )
                                  | p3(X)
                                  | ~ r1(Y,X) )
                              | p3(Y) )
                          | p1(X)
                          | p2(X)
                          | p4(X) ) )
                | ! [Y] :
                    ( ~ r1(X,Y)
                    | ! [X] :
                        ( p4(X)
                        | p3(X)
                        | ! [Y] :
                            ( p4(Y)
                            | p3(Y)
                            | ! [X] :
                                ( ~ r1(Y,X)
                                | p2(X)
                                | ! [Y] :
                                    ( $false
                                    | ~ r1(X,Y) )
                                | p1(X)
                                | p3(X)
                                | p4(X) )
                            | p1(Y)
                            | p2(Y)
                            | ~ r1(X,Y) )
                        | p1(X)
                        | p2(X)
                        | ~ r1(Y,X) )
                    | p1(Y)
                    | p2(Y) ) )
              & ( ! [Y] :
                    ( ~ r1(X,Y)
                    | ( ( ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ~ ! [X] :
                                      ( ~ r1(Y,X)
                                      | ~ p2(X)
                                      | ! [Y] :
                                          ( ~ r1(X,Y)
                                          | p2(Y) ) )
                                | p2(Y)
                                | ~ r1(X,Y) ) )
                        | ~ ! [X] :
                              ( p2(X)
                              | ~ ! [Y] :
                                    ( ~ r1(X,Y)
                                    | ! [X] :
                                        ( p2(X)
                                        | ~ r1(Y,X) )
                                    | ~ p2(Y) )
                              | ~ r1(Y,X) ) )
                      & ( p2(Y)
                        | ~ ! [X] :
                              ( ~ r1(Y,X)
                              | ! [Y] :
                                  ( p2(Y)
                                  | ~ r1(X,Y) )
                              | ~ p2(X) ) ) ) )
                | ~ ! [Y] :
                      ( ~ r1(X,Y)
                      | ( ( ~ ! [X] :
                                ( ~ r1(Y,X)
                                | ! [Y] :
                                    ( p2(Y)
                                    | ~ r1(X,Y) )
                                | ~ p2(X) )
                          | p2(Y) )
                        & ( ~ ! [X] :
                                ( ~ r1(Y,X)
                                | ~ ! [Y] :
                                      ( ! [X] :
                                          ( ~ r1(Y,X)
                                          | p2(X) )
                                      | ~ p2(Y)
                                      | ~ r1(X,Y) )
                                | p2(X) )
                          | ! [X] :
                              ( ~ r1(Y,X)
                              | ! [Y] :
                                  ( p2(Y)
                                  | ~ ! [X] :
                                        ( ~ r1(Y,X)
                                        | ! [Y] :
                                            ( ~ r1(X,Y)
                                            | p2(Y) )
                                        | ~ p2(X) )
                                  | ~ r1(X,Y) ) ) ) )
                      | ~ ! [X] :
                            ( ~ r1(Y,X)
                            | ! [Y] :
                                ( ~ r1(X,Y)
                                | ( ( ! [X] :
                                        ( ! [Y] :
                                            ( ~ r1(X,Y)
                                            | p2(Y)
                                            | ~ ! [X] :
                                                  ( ~ p2(X)
                                                  | ! [Y] :
                                                      ( ~ r1(X,Y)
                                                      | p2(Y) )
                                                  | ~ r1(Y,X) ) )
                                        | ~ r1(Y,X) )
                                    | ~ ! [X] :
                                          ( ~ ! [Y] :
                                                ( ~ r1(X,Y)
                                                | ! [X] :
                                                    ( p2(X)
                                                    | ~ r1(Y,X) )
                                                | ~ p2(Y) )
                                          | p2(X)
                                          | ~ r1(Y,X) ) )
                                  & ( p2(Y)
                                    | ~ ! [X] :
                                          ( ~ r1(Y,X)
                                          | ~ p2(X)
                                          | ! [Y] :
                                              ( p2(Y)
                                              | ~ r1(X,Y) ) ) ) ) )
                            | ~ ( ( ~ ! [Y] :
                                        ( ~ r1(X,Y)
                                        | ~ p2(Y)
                                        | ! [X] :
                                            ( ~ r1(Y,X)
                                            | p2(X) ) )
                                  | p2(X) )
                                & ( ! [Y] :
                                      ( ! [X] :
                                          ( ~ ! [Y] :
                                                ( ~ r1(X,Y)
                                                | ! [X] :
                                                    ( p2(X)
                                                    | ~ r1(Y,X) )
                                                | ~ p2(Y) )
                                          | p2(X)
                                          | ~ r1(Y,X) )
                                      | ~ r1(X,Y) )
                                  | ~ ! [Y] :
                                        ( p2(Y)
                                        | ~ ! [X] :
                                              ( ! [Y] :
                                                  ( ~ r1(X,Y)
                                                  | p2(Y) )
                                              | ~ p2(X)
                                              | ~ r1(Y,X) )
                                        | ~ r1(X,Y) ) ) ) ) ) ) )
          | ! [Y] :
              ( p3(Y)
              | ~ r1(X,Y) ) ) )).

fof(transitivity,axiom,(
    ! [X,Y,Z] :
      ( r1(X,Z)
     <= ( r1(Y,Z)
        & r1(X,Y) ) ) )).

fof(reflexivity,axiom,(
    ! [X] : r1(X,X) )).

