include('Axioms/PLA002+0.ax').
fof(goal_state,conjecture,(
    ! [S] :
      ( ( on(block_2,block_3,S)
        & on(block_1,table,S)
        & on(block_3,block_1,S)
        & clear(block_2,S) )
     <= goal_time(S) ) )).

fof(time_0,hypothesis,(
    time(time_0) )).

fof(initial_3_on_table,hypothesis,(
    on(block_3,table,time_0) )).

fof(time_2,hypothesis,(
    time(s(s(time_0))) )).

fof(block_2,hypothesis,(
    a_block(block_2) )).

fof(some_source,hypothesis,(
    ! [I] :
      ( ( source(block_2,I)
        | source(table,I)
        | source(block_3,I)
        | source(block_1,I) )
     <= time(I) ) )).

fof(block_1_not_block_2,hypothesis,(
    different(block_1,block_2) )).

fof(block_3_not_table,hypothesis,(
    different(block_3,table) )).

fof(fixed_table,hypothesis,(
    fixed(table) )).

fof(nonfixed_block_1,hypothesis,(
    nonfixed(block_1) )).

fof(some_destination,hypothesis,(
    ! [I] :
      ( ( destination(block_3,I)
        | destination(table,I)
        | destination(block_2,I)
        | destination(block_1,I) )
     <= time(I) ) )).

fof(block_2_not_table,hypothesis,(
    different(block_2,table) )).

fof(block_2_not_block_3,hypothesis,(
    different(block_2,block_3) )).

fof(block_3,hypothesis,(
    a_block(block_3) )).

fof(goal_time_3,hypothesis,(
    goal_time(s(s(s(time_0)))) )).

fof(time_1,hypothesis,(
    time(s(time_0)) )).

fof(initial_clear_1,hypothesis,(
    clear(block_1,time_0) )).

fof(some_object,hypothesis,(
    ! [I] :
      ( time(I)
     => ( object(block_2,I)
        | object(block_3,I)
        | object(block_1,I) ) ) )).

fof(different_not_equal,hypothesis,(
    ! [Y,X] :
      ( neq(X,Y)
     <= ( different(X,Y)
        | different(Y,X) ) ) )).

fof(initial_2_on_table,hypothesis,(
    on(block_2,table,time_0) )).

fof(block_1_not_block_3,hypothesis,(
    different(block_1,block_3) )).

fof(block_1,hypothesis,(
    a_block(block_1) )).

fof(time_3,hypothesis,(
    time(s(s(s(time_0)))) )).

fof(initial_clear_3,hypothesis,(
    clear(block_3,time_0) )).

fof(block_1_not_table,hypothesis,(
    different(block_1,table) )).

fof(initial_1_on_2,hypothesis,(
    on(block_1,block_2,time_0) )).

fof(table,hypothesis,(
    a_block(table) )).

fof(nonfixed_block_3,hypothesis,(
    nonfixed(block_3) )).

fof(nonfixed_block_2,hypothesis,(
    nonfixed(block_2) )).

