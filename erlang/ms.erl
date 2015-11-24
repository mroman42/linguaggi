-module(ms).
-export([start/1, to_slave/2]).

start(N) ->
    register(master, spawn(fun() -> masterpr(N) end)),
    ok.

to_slave(Text, M) ->
    master ! {msg, M, Text}.


master_spawn(N) ->
    lists:map(fun(X) ->
                      register(
                        list_to_atom("slave"++integer_to_list(X)),
                        spawn(fun() -> slavepr(X) end)) 
              end,
              lists:seq(1,N)
             ).

master_wait() ->
    receive
        {msg, M, Text} -> 
            list_to_atom("slave"++integer_to_list(M)) ! {msg,Text},
            master_wait();
        {'EXIT', Pid, {M,_}} ->
            Pid = spawn(fun() -> slavepr(M) end),
            register("slave"++integer_to_list(M), Pid),
            io:format("Slave process "++integer_to_list(M)++" restarted.")
    end.

masterpr(N) ->
    process_flag(trap_exit, true),
    master_spawn(N),
    master_wait().

slavepr(X) ->
    link(whereis(master)),
    slave_wait(X).

slave_wait(X) ->
    receive
        {msg, die} ->
            io:format("slave" ++ integer_to_list(X) ++  " dies."),
            exit({X,normal});
        {msg, Text} ->
            io:format("slave" ++ integer_to_list(X) ++  "prints: " ++ Text),
            slave_wait(X)
    end.

