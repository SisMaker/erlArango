-module(test).


-compile([export_all, nowarn_export_all]).


tt(C, N) ->
   application:start(erlArango),
   agHttpCli:startPool(tt, [{poolSize, 100}], []),
   Request = {<<"GET">>, <<"/_api/database/current">>, [], []},
   [spawn(test, test, [N, Request]) || _Idx <- lists:seq(1, C)].
   %%test(N, Request).

%% /_api/database

test(0, Request) ->
   R1 = {<<"GET">>, <<"/_api/database">>, [], []},
   agHttpCli:callAgency(tt, R1, 5000);
test(N, Request) ->
   erlang:put(cnt, N),
   agHttpCli:callAgency(tt, Request, 5000),
   test(N - 1, Request).