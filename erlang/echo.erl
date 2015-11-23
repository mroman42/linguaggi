-module(echo).
-export([start/0, print/1, stop/0]).    

server() ->
    receive
        {msg, Text} -> io:format("~p~n", [Text]), server();
        stop -> io:format("Echo server finished~n", [])
    end.

start() ->
    register(echoserver,
             spawn(fun() -> server() end)),
    ok.

print(Text) ->
    echoserver ! {msg, Text},
    ok.

stop() ->
    echoserver ! stop,
    ok.

