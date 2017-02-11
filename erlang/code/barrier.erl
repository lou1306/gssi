-module(barrier).
-export([barrier/1, worker/2, start/0]).


barrier(N) ->
    barrier(N, 0, []).

barrier(N, ProcsLen, Procs) ->
    case ProcsLen == N of
        true ->
            send_continue(Procs);
        false ->
            receive
                {From, done} ->
                    barrier(N, ProcsLen+1, [From | Procs])
            end
    end.

send_continue([]) ->
    ok;
send_continue([Head | Tail]) ->
    Head ! continue,
    send_continue(Tail).

worker(N, Barrier) ->
    io:format("~B before barrier...~n", [N]),
    Barrier ! {self(), done},
    receive
        continue ->
            io:format("~B after barrier!~n", [N])
    end.

start() ->
    Barrier = spawn(?MODULE, barrier, [5]),
    _ = [spawn(?MODULE, worker, [N, Barrier]) || N <-[1,2,3,4,5]].