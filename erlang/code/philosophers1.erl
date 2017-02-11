-module(philosophers1).
-export([start/0, phil/4, phil/2, table/0]).
-l(rand).
-l(timer).

start() ->
    register(controller, self()),
    timer:start(),
    io:format("Type 'controller ! stop' to kill the processes.~n Starting...~n"),
    receive after 2000 -> ok end,
    restart().
restart() ->
    TablePid = spawn(?MODULE, table, []),
    PhilPids = [spawn(?MODULE, phil, [N, TablePid]) || N <-lists:seq(1,5)],
    receive
        stop ->
            [exit(X, quit) || X <- PhilPids],
            exit(TablePid, kill)
    end.


phil(N, Table) ->
    phil(N, Table, 0, takeleft).

phil(N, Table, Meals, Msg) ->
    process_flag(trap_exit, true),
    case Msg of
        takeleft -> timer:sleep(rand:uniform(10));
        _ -> ok
    end,
    Table ! {self(), N, Msg},
    receive
        {_, okleft} ->
            phil(N,Table, Meals, takeright);
        {_, okright} ->
            io:format("~B is eating.~n", [N]),
            Table ! {self(), N, dropright},
            Table ! {self(), N, dropleft},
            phil(N,Table, Meals+1, takeleft);
        {_, wait} ->
            io:format("~B WAIT~n", [N]),
            erlang:yield(),
            phil(N,Table, Meals, Msg);
        {'EXIT', _, _} ->
            io:format("Philosoper ~B ate ~B times. Current: ~p ~n", [N, Meals, Msg])
    end.

table() ->
    table({0,0,0,0,0}, 4).

table({1,2,3,4,5}, _) ->
    io:format("-- 1 2 3 4 5 --~n"),
    io:format("Deadlock detected!~n"),
    controller ! deadlock;

table(Chopsticks = {C1, C2, C3, C4, C5}, Count) ->
    io:format("-- ~B ~B ~B ~B ~B --, ~B ~n", [C1,C2,C3,C4,C5, Count]),
    receive
        {From, N, takeleft} when element(N, Chopsticks) =:= 0,  Count > 0 ->
            From ! {self(), okleft},
            table(setelement(N, Chopsticks, N), Count-1);
        {From, N, takeright} when element((N rem 5) + 1, Chopsticks) =:= 0 ->
            From ! {self(), okright},
            table(setelement((N rem 5) + 1, Chopsticks, N), Count);
        {_, N, dropleft} ->
            table(setelement(N, Chopsticks, 0), Count+1);
        {_, N, dropright} ->
            table(setelement((N rem 5) + 1, Chopsticks, 0), Count);
        {From, _, _} ->
            From ! {self(), wait},
            table(Chopsticks, Count)
    end.