-module(ms).
-export([start/1, to_slave/2]).

% Registers the master at start
start(N) ->
    register(master, spawn(fun() -> masterpr(N) end)),
    ok.


% Master main function
masterpr(N) ->
    process_flag(trap_exit, true),
    master_spawn(N),
    master_wait().

% Sends a message to the master, in order to send it to the slave
to_slave(Text, M) ->
    master ! {msg, M, Text}.

% Spawn slave processes
master_spawn(N) ->
    lists:map(fun(X) ->
                      register(
                        list_to_atom("slave"++integer_to_list(X)),
                        spawn(fun() -> slavepr(X) end)) 
              end,
              lists:seq(1,N)
             ).


% Wait for a message to the slaves
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


% Slave main function. It is linked to the master.
slavepr(X) ->
    link(whereis(master)),
    slave_wait(X).

% Wait for a message from the master
slave_wait(X) ->
    receive
        {msg, die} ->
            io:format("slave" ++ integer_to_list(X) ++  " dies.\n"),
            exit({X,normal});
        {msg, Text} ->
            io:format("slave" ++ integer_to_list(X) ++  " prints: " ++ Text ++ "\n"),
            slave_wait(X)
    end.

