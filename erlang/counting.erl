-module(counting).
-export([start/0, stop/0, tot/0, echo/1, reverse/1, sort/1]).

start() ->
    register(server, spawn(fun() -> wait() end)),
    ok.

wait() ->
    receive
        {echo,Text} -> io:format(Text++"\n");
        {reverse,Text} -> io:format(lists:reverse(Text)++"\n");
        {sort,Text} -> io:format(lists:sort(Text)++"\n")
    end.

echo(Text) -> server ! {echo,Text}.
reverse(Text) -> server ! {reverse,Text}.
sort(Text) -> server ! {sort,Text}.
