-module(queuedmutex).
-export([mutex/1, worker/2, start/0]).
-l(timer).

mutex(N) ->
    mutex(N, queue:new()).

mutex(N, Queue) ->
    io:format("MUTEX --> ~B~n", [N]),
    case (N>0) and
    not queue:is_empty(Queue) of
        false -> ok;
        true ->
            {{value, Pid}, NewQueue} = queue:out(Queue),
            Pid ! ok,
            mutex(N-1, NewQueue)
    end,
    receive
        {From, p} when N > 0 ->
            From ! ok,
            mutex(N-1, Queue);
        {From, p} ->
            From ! wait,
            mutex(N, queue:in(From, Queue));
        {From, v} ->
            From ! ok,
            mutex(N+1, Queue)
    end.

worker(N, Mutex) ->
    %timer:sleep(rand:uniform(50)),
    PrintMsg = "~B acquired mutex.~n",
    Mutex ! {self(), p},
    receive
        ok ->
            io:format(PrintMsg, [N]);
        wait ->
            io:format("~B waiting.~n", [N]),
            receive 
                ok -> io:format(PrintMsg, [N])
            end
    end,
    io:format("Critical section...~n"),
    Mutex ! {self(), v}.

start() ->
    Mutex = spawn(?MODULE, mutex, [1]),
    _ = [spawn(?MODULE, worker, [X, Mutex]) || X <- lists:seq(1,10)].