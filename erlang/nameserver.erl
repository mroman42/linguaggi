-module(nameserver).
-export([start/0,store/2,lookup/1]).

% User interface
start() -> register(kvs, spawn(fun() -> loop() end)).
store(Key,Value) -> query({store,Key,Value}).
lookup(Key) -> query({lookup,Key}).

% A function that makes a query to the server and waits for a response.
% In this protocol, it is necessary to send your PID to the server
query(Q) ->
    kvs ! {self(), Q},
    receive
        {kvs,Reply} -> Reply
    end.

% Server main function
% The server uses the process dictionary to store the given keys.
% Note that this use is discouraged in Erlang. (!)
loop() ->
    receive
        {From, {store,Key,Value}} ->
            put(Key, Value),
            From ! {kvs,true},
            loop();
        {From, {lookup,Key}} -> 
            From ! {kvs,get(Key)},
            loop()
    end.
                                
