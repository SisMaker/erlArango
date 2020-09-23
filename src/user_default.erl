-module(user_default).
-include("agHttpCli.hrl").

-compile([export_all, nowarn_export_all]).

start() ->
   erlSync:run(),
   application:ensure_all_started(erlArango),
   agHttpCli:startPool(tt, [{poolSize, 10}], []).

tt(C, N) ->
   application:ensure_all_started(erlArango),
   agHttpCli:startPool(tt, [{poolSize, 16}], []),
   StartTime = erlang:system_time(millisecond),
   io:format("IMY**********************  started~n"),
   [spawn(?MODULE, test, [N, StartTime]) || _Idx <- lists:seq(1, C)].
%%test(N, Request).

%% /_api/database

test(0, StartTime) ->
   agMiscFuns:curDbTime(tt),
   io:format("IMY******test over use time ~pms~n", [erlang:system_time(millisecond) - StartTime]);
test(N, StartTime) ->
   agMiscFuns:curDbTime(tt),
   test(N - 1, StartTime).

%% tt(C, N) ->
%%    application:start(erlArango),
%%    agHttpCli:startPool(tt, [{poolSize, 1}, {baseUrl, <<"http://localhost:8181">>}], []),
%%    Request = {<<"GET">>, <<"/_api/database/current">>, [], []},
%%    io:format("IMY**********************  start time ~p~n",[erlang:system_time(millisecond)]),
%%    [spawn(test, test, [N, Request]) || _Idx <- lists:seq(1, C)].
%% %%test(N, Request).
%%
%% %% /_api/database
%%
%% test(0, Request) ->
%%    R1 = {<<"POST">>, <<"/echo_body">>, [], []},
%%    agHttpCli:callAgency(tt, {<<"GET">>, <<"/ibrowse_stream_once_chunk_pipeline_test">>, [], []}, infinity),
%%    agHttpCli:callAgency(tt, {<<"POST">>, <<"/echo_body">>, [], []}, infinity),
%%    io:format("IMY**********************  test over ~p~n",[erlang:system_time(millisecond)]);
%% test(N, Request) ->
%%    erlang:put(cnt, N),
%%    agHttpCli:callAgency(tt, Request, 5000),
%%    test(N - 1, Request).

-define(HeadBin, <<"X-Content-Type-Options: nosniff\r\nEtag: \"_aKwJ_tm--E\"\r\nServer: ArangoDB\r\nConnection: Keep-Alive\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: 178">>).
th1(0, Fun, Rn) ->
   ?MODULE:Fun(?HeadBin, Rn);
th1(N, Fun, Rn) ->
   ?MODULE:Fun(?HeadBin, Rn),
   th1(N - 1, Fun, Rn).

th2(0, Fun, Cl, Rn) ->
   ?MODULE:Fun(?HeadBin, Cl, Rn);
th2(N, Fun, Cl, Rn) ->
   ?MODULE:Fun(?HeadBin, Cl, Rn),
   th2(N - 1, Fun, Cl, Rn).

head1(Headers, Rn) ->
   HeadersList = binary:split(Headers, Rn, [global]),
   contentLength(HeadersList).

head2(Headers, _Rn) ->
   HeadersList = binary:split(Headers, <<"\r\n">>, [global]),
   contentLength(HeadersList).

head3(Headers, CL, Rn) ->
   case binary:split(Headers, CL) of
      [_, Rest1] ->
         case binary:split(Rest1, Rn) of
            [InBin, _Rest2] ->
               binary_to_integer(InBin);
            [InBin] ->
               binary_to_integer(InBin)
         end;
      _ ->
         0
   end.

%% binary:compile_pattern(<<"\r\n">>)
%% binary:compile_pattern(<<"Content-Length: ">>)

head4(Headers, _CL, _Rn) ->
   case binary:split(Headers, <<"Content-Length: ">>) of
      [_, Rest1] ->
         case binary:split(Rest1, <<"\r\n">>) of
            [InBin, _Rest2] ->
               binary_to_integer(InBin);
            [InBin] ->
               binary_to_integer(InBin)
         end;
      _ ->
         0
   end.

contentLength([]) ->
   undefined;
contentLength([<<"Content-Length: ", Rest/binary>> | _T]) ->
   binary_to_integer(Rest);
contentLength([<<"content-length: ", Rest/binary>> | _T]) ->
   binary_to_integer(Rest);
contentLength([<<"Transfer-Encoding: chunked">> | _T]) ->
   chunked;
contentLength([<<"transfer-encoding: chunked">> | _T]) ->
   chunked;
contentLength([_ | T]) ->
   contentLength(T).

%% 测试 jiffy 与 jsx 编码解码性能
tcjf(0, _Args1) ->
   Args = #{name => ffd, tet => "fdsff", <<"dfdf">> => 131245435346},
   jiffy:encode(Args);
tcjf(N, Args1) ->
   Args = #{name => ffd, tet => "fdsff", <<"dfdf">> => 131245435346},
   jiffy:encode(Args),
   tcjf(N - 1, Args1).

tcjx(0, _Args1) ->
   Args = {[{name, ffd}, {tet, "fdsff"}, {<<"dfdf">>, 131245435346}]},
   jiffy:encode(Args);
tcjx(N, Args1) ->
   Args = {[{name, ffd}, {tet, "fdsff"}, {<<"dfdf">>, 131245435346}]},
   jiffy:encode(Args),
   tcjx(N - 1, Args1).


-define(BodyBin1, <<"{\"_key\":\"01J\",\"_id\":\"airports/01J\",\"_rev\":\"_aKwJ_tm--E\",\"name\":\"Hilliard Airpark\",\"city\":\"Hilliard\",\"state\":\"FL\",\"country\":\"USA\",\"lat\":30.6880125,\"long\":-81.90594389,\"vip\":false}">>).
-define(BodyBin2, <<"{\"_key\":\"01J\",\"_id\":\"airports/01J\",\"_rev\":\"_aPaBl7O--_\",\"name\":\"Hilliard Airpark\",\"city\":\"Hilliardfdfsdfdsffffffffffffffffffffffffffffffffffffffffffffffffffffffffafdsfasdfdafsdafdsfsdafdsafdsfdsfdsafdsfdsfdsfhghfghfghgfhsdsdfdsfdsfdsffdfddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddggggggggggggggggggggggggggggggggggggggggg\",\"state\":\"FL\",\"country\":\"USAjjkjkjkfgjkgjfkdjgldgjldjglfdjglfjdljljrlejtrltjewltjrelwtjrletjrletrletjlrejtjtrlwjrejwlrjjreljtljelwjrtlwjtreljrlewjrlwjrlwejrlejltkdfsafd\",\"lat\":30.6880125,\"long\":-81.90594389,\"vip\":false}">>).

jd1(0, Fun) ->
   ?MODULE:Fun(?BodyBin1);
jd1(N, Fun) ->
   ?MODULE:Fun(?BodyBin1),
   jd1(N - 1, Fun).

jd2(0, Fun) ->
   ?MODULE:Fun(?BodyBin2);
jd2(N, Fun) ->
   ?MODULE:Fun(?BodyBin2),
   jd2(N - 1, Fun).

decodeJy1(Bin) ->
   jiffy:decode(Bin, [return_maps]).

decodeJy2(Bin) ->
   jiffy:decode(Bin, [return_maps, copy_strings]).


decodeJx1(Bin) ->
   jsx:decode(Bin, [return_maps]).

decodeJx2(Bin) ->
   jsx:decode(Bin, []).
