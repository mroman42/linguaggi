-module(nameserver).
-export([start/0,store/2,lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).
store(Key,Value) -> query({store,Key,Value}).
lookup(Key) -> query({lookup,Key}).

query(Q) ->
    kvs ! {self(), Q},
    receive
        {kvs,Reply} -> Reply
    end.

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
                                
