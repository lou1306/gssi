% phil(N, Table, none) ->
%     timer:sleep(rand:uniform(500)),
%     Table ! {self(), N, takeleft},
%     receive
%         {_, okleft} ->
%             phil(N, Table, okleft)
%     after 1000 ->
%         phil(N, Table, none)
%     end;
% phil(N, Table, okleft) ->
%     Table ! {self(), N, takeright},
%     receive
%         {_, okright} ->
%             % io:format("~B L+R~n", [N]),
%             phil(N, Table, okboth)
%     after 1000 ->
%         phil(N, Table, okleft)
%     end;
% phil(N, Table, okboth) ->
%     io:format("~B is eating.~n", [N]),
%     Table ! {self(), N, dropright},
%     Table ! {self(), N, dropleft},
%     phil(N, Table, none).