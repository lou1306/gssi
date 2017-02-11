-module(mutex).
-export([mutex/1, worker/2, start/0]).
-l(timer).


mutex(N) ->
    io:format("MUTEX --> ~B~n", [N]),
    receive
        {From, p} when N > 0 ->
            From ! ok,
            mutex(N-1);
        {From, p} ->
            From ! wait,
            mutex(N);
        {From, v} ->
            From ! ok,
            mutex(N+1)
    end.

worker(N, Mutex) ->
    timer:sleep(rand:uniform(50)),
    Mutex ! {self(), p},
    receive
        ok ->
            io:format("Mutex acquired by ~B.~n", [N]),
            io:format("Critical section...~n"),
            Mutex ! {self(), v};
        wait ->
            io:format("~B waiting.~n", [N]),
            worker(N, Mutex)
    end.

start() ->
    Mutex = spawn(?MODULE, mutex, [1]),
    _ = [spawn(?MODULE, worker, [X, Mutex]) || X <- lists:seq(1,10)].