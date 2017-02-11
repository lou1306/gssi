-module(echo).
-export([echo/0]).

echo() ->
    register(echo_server, self()),
    recv_loop().

recv_loop() ->
    receive
        {happy, Msg} ->
            io:format("Echo: ~p :)~n", [Msg]);
        {sad, Msg} ->
            io:format("Echo: ~p :(~n ", [Msg]);
        Msg ->
            io:format("Echo: ~p~n", [Msg])
    end,
    recv_loop().
