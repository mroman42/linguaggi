-module(counting).
-export([start/0, stop/0, tot/0, echo/1, reverse/1, sort/1]).

% Start the server
start() ->
    register(server, spawn(fun() -> loop({count,0,0,0}) end)),
    ok.

% Server main function
loop({count,E,R,S}) ->
    receive
        {echo,Text} -> io:format(Text++"\n"),
                       loop({count,E+1,R,S});
        {reverse,Text} -> io:format(lists:reverse(Text)++"\n"), 
                          loop({count,E,R+1,S});
        {sort,Text} -> io:format(lists:sort(Text)++"\n"), 
                       loop({count,E,R,S+1});
        {tot} -> print(E,R,S),
                 loop({count,E,R,S})
    end.

% Functions calling the server
tot() -> server ! {tot}.
echo(Text) -> server ! {echo,Text}.
reverse(Text) -> server ! {reverse,Text}.
sort(Text) -> server ! {sort,Text}.

% Auxiliary format function
print(E,R,S) ->
    io:format("Counter:\n"++
                  "echo: "++integer_to_list(E)++"\n"++
                  "reverse: "++integer_to_list(R)++"\n"++
                  "sort: "++integer_to_list(S)++"\n").

% Stops the server. Unregisters the process
stop() ->
    exit(whereis(server), kill),
    ok.
