-module(agHttpProtocol).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   headers/1
   , request/7
   , response/2
   , response/5
]).

%% <<"Content-Type: application/json; charset=utf-8">>,
-spec request(boolean(), body(), method(), host(), binary(), path(), headers()) -> iolist().
request(true, undefined, Method, Host, _DbName, Path, Headers) ->
   [
      Method, <<"/_db/_system">>, Path, <<" HTTP/1.1\r\nHost: ">>, Host,
      <<"\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: 0\r\n">>,
      spellHeaders(Headers), <<"\r\n">>
   ];
request(false, undefined, Method, Host, DbName, Path, Headers) ->
   [
      Method, DbName, Path, <<" HTTP/1.1\r\nHost: ">>, Host,
      <<"\r\nContent-Type: application/json; charset=utf-8\r\nContent-Length: 0\r\n">>,
      spellHeaders(Headers), <<"\r\n">>
   ];
request(false, Body, Method, Host, DbName, Path, Headers) ->
   ContentLength = integer_to_binary(iolist_size(Body)),
   NewHeaders = [{<<"Content-Length">>, ContentLength} | Headers],
   [
      Method, DbName, Path, <<" HTTP/1.1\r\nHost: ">>, Host,
      <<"\r\nContent-Type: application/json; charset=utf-8\r\n">>,
      spellHeaders(NewHeaders), <<"\r\n">>, Body
   ];
request(true, Body, Method, Host, _DbName, Path, Headers) ->
   ContentLength = integer_to_binary(iolist_size(Body)),
   NewHeaders = [{<<"Content-Length">>, ContentLength} | Headers],
   [
      Method,  <<"/_db/_system">>, Path, <<" HTTP/1.1\r\nHost: ">>, Host,
      <<"\r\nContent-Type: application/json; charset=utf-8\r\n">>,
      spellHeaders(NewHeaders), <<"\r\n">>, Body
   ].


-spec response(binary(), boolean()) -> {ok, recvState(), binary()} | error().
response(Data, IsHeadMethod) ->
   response(undefined, binary:compile_pattern(<<"\r\n">>), binary:compile_pattern(<<"\r\n\r\n">>), Data, IsHeadMethod).

-spec response(undefined | recvState(), binary:cp(), binary:cp(), binary(), boolean()) -> {ok, recvState()} | error().
response(undefined, Rn, RnRn, Data, IsHeadMethod) ->
   case parseStatusLine(Data, Rn) of
      {StatusCode, Rest} ->
         case splitHeaders(Rest, Rn, RnRn) of
            {undefined, Headers, Body} ->
               {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = undefined, body = Body}};
            {0, Headers, Rest} ->
               {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = 0, body = Rest}};
            {chunked, Headers, Body} ->
               case IsHeadMethod orelse StatusCode == 204 orelse StatusCode == 304 orelse (StatusCode < 200 andalso StatusCode >= 100) of
                  true ->
                     {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = 0, body = Body}};
                  _ ->
                     RecvState = #recvState{stage = body, contentLength = chunked, statusCode = StatusCode, headers = Headers},
                     response(RecvState, Rn, RnRn, Body, IsHeadMethod)
               end;
            {ContentLength, Headers, Body} ->
               BodySize = erlang:size(Body),
               if
                  BodySize == ContentLength ->
                     {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}};
                  BodySize > ContentLength ->
                     ?WARN(agTcpAgencyIns, "11 contentLength get to long data why? more: ~p ~n", [BodySize - ContentLength]),
                     {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}};
                  true ->
                     case IsHeadMethod orelse StatusCode == 204 orelse StatusCode == 304 orelse (StatusCode < 200 andalso StatusCode >= 100) of
                        true ->
                           {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}};
                        _ ->
                           {ok, #recvState{stage = body, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}}
                     end
               end;
            not_enough_data ->
               {ok, #recvState{stage = header, body = Data}}
         end;
      not_enough_data ->
         {ok, #recvState{stage = header, body = Data}};
      {error, Reason} ->
         {error, Reason}
   end;
response(#recvState{stage = body, contentLength = chunked, body = Body, buffer = Buffer} = RecvState, Rn, _RnRn, Data, _IsHeadMethod) ->
   NewBuffer = <<Buffer/binary, Data/binary>>,
   case parseChunks(NewBuffer, Rn, []) of
      {ok, AddBody, _Rest} ->
         LastBody = <<Body/binary, AddBody/binary>>,
         {done, RecvState#recvState{stage = done, body = LastBody}};
      {not_enough_data, AddBody, Rest} ->
         NewBody = <<Body/binary, AddBody/binary>>,
         {ok, RecvState#recvState{body = NewBody, buffer = Rest}};
      {error, Reason} ->
         {error, Reason}
   end;
response(#recvState{stage = body, contentLength = ContentLength, body = Body} = RecvState, _Rn, _RnRn, Data, _IsHeadMethod) ->
   CurData = <<Body/binary, Data/binary>>,
   BodySize = erlang:size(CurData),
   if
      BodySize == ContentLength ->
         {done, RecvState#recvState{stage = done, body = CurData}};
      BodySize > ContentLength ->
         ?WARN(agTcpAgencyIns, "22 contentLength get to long data why? more: ~p ~n", [BodySize - ContentLength]),
         {done, #recvState{stage = done, body = CurData}};
      true ->
         {ok, RecvState#recvState{body = CurData}}
   end;
response(#recvState{stage = header, body = OldBody}, Rn, RnRn, Data, IsHeadMethod) ->
   CurBody = <<OldBody/binary, Data/binary>>,
   case parseStatusLine(CurBody, Rn) of
      {StatusCode, Rest} ->
         case splitHeaders(Rest, Rn, RnRn) of
            {undefined, Headers, Body} ->
               {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = undefined, body = Body}};
            {0, Headers, Body} ->
               {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = 0, body = Body}};
            {chunked, Headers, Rest} ->
               case IsHeadMethod orelse StatusCode == 204 orelse StatusCode == 304 orelse (StatusCode < 200 andalso StatusCode >= 100) of
                  true ->
                     {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = 0, body = <<>>}};
                  _ ->
                     RecvState = #recvState{stage = body, contentLength = chunked, statusCode = StatusCode, headers = Headers},
                     response(RecvState, Rn, RnRn, Rest, IsHeadMethod)
               end;
            {ContentLength, Headers, Body} ->
               BodySize = erlang:size(Body),
               if
                  BodySize == ContentLength ->
                     {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}};
                  BodySize > ContentLength ->
                     ?WARN(agTcpAgencyIns, "33 contentLength get to long data why? more: ~p ~n", [BodySize - ContentLength]),
                     {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}};
                  true ->
                     case IsHeadMethod orelse StatusCode == 204 orelse StatusCode == 304 orelse (StatusCode < 200 andalso StatusCode >= 100) of
                        true ->
                           {done, #recvState{stage = done, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}};
                        _ ->
                           {ok, #recvState{stage = body, statusCode = StatusCode, headers = Headers, contentLength = ContentLength, body = Body}}
                     end
               end;
            not_enough_data ->
               {ok, #recvState{stage = header, body = CurBody}}
         end;
      not_enough_data ->
         {ok, #recvState{stage = header, body = CurBody}};
      {error, Reason} ->
         {error, Reason}
   end.

spellHeaders(Headers) ->
   <<<<Key/binary, ": ", Value/binary, "\r\n">> || {Key, Value} <- Headers>>.

splitHeaders(Data, Rn, RnRn) ->
   case binary:split(Data, RnRn) of
      [Data] ->
         not_enough_data;
      [Headers, Body] ->
         HeadersList = binary:split(Headers, Rn, [global]),
         ContentLength = contentLength(HeadersList),
         {ContentLength, Headers, Body}
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

parseStatusLine(Data, Rn) ->
   case binary:split(Data, Rn) of
      [Data] ->
         not_enough_data;
      [Line, Rest] ->
         case parseStatusReason(Line) of
            {ok, StatusCode} ->
               {StatusCode, Rest};
            {error, Reason} ->
               {error, Reason}
         end
   end.

parseStatusReason(<<"HTTP/1.1 200 OK">>) ->
   {ok, 200};
parseStatusReason(<<"HTTP/1.1 204 No Content">>) ->
   {ok, 204};
parseStatusReason(<<"HTTP/1.1 301 Moved Permanently">>) ->
   {ok, 301};
parseStatusReason(<<"HTTP/1.1 302 Found">>) ->
   {ok, 302};
parseStatusReason(<<"HTTP/1.1 403 Forbidden">>) ->
   {ok, 403};
parseStatusReason(<<"HTTP/1.1 404 Not Found">>) ->
   {ok, 404};
parseStatusReason(<<"HTTP/1.1 500 Internal Server Error">>) ->
   {ok, 500};
parseStatusReason(<<"HTTP/1.1 502 Bad Gateway">>) ->
   {ok, 502};
parseStatusReason(<<"HTTP/1.1 ", N1, N2, N3, " ", _Reason/bits>>)
   when $0 =< N1, N1 =< $9,
   $0 =< N2, N2 =< $9,
   $0 =< N3, N3 =< $9 ->
   StatusCode = (N1 - $0) * 100 + (N2 - $0) * 10 + (N3 - $0),
   {ok, StatusCode};
parseStatusReason(<<"HTTP/1.0 ", _/binary>>) ->
   {error, unsupported_feature};
parseStatusReason(_) ->
   {error, bad_request}.

parseChunks(Data, Rn, Acc) ->
   case parseChunk(Data, Rn) of
      done ->
         {ok, iolist_to_binary(lists:reverse(Acc)), <<>>};
      {ok, Body, Rest} ->
         parseChunks(Rest, Rn, [Body | Acc]);
      not_enough_data ->
         {not_enough_data, iolist_to_binary(lists:reverse(Acc)), Data};
      {error, Reason} ->
         {error, Reason}
   end.

parseChunk(Data, Rn) ->
   case binary:split(Data, Rn) of
      [Size, Rest] ->
         case parseChunkSize(Size) of
            undefined ->
               {error, invalid_chunk_size};
            0 ->
               done;
            HexSize ->
               parseChunkBody(Rest, HexSize)
         end;
      [Data] ->
         not_enough_data
   end.

parseChunkBody(Data, Size) ->
   case Data of
      <<Body:Size/binary, "\r\n", Rest/binary>> ->
         {ok, Body, Rest};
      _ ->
         not_enough_data
   end.

parseChunkSize(Bin) ->
   try
      binary_to_integer(Bin, 16)
   catch
      error:badarg ->
         undefined
   end.

-spec headers(recvState()) -> {ok, headers()} | {error, invalidHeaders}.
headers(#recvState{headers = Headers}) ->
   parseHeaders(Headers, []).

parseHeaders([], Acc) ->
   {ok, lists:reverse(Acc)};
parseHeaders([Header | T], Acc) ->
   case binary:split(Header, <<":">>) of
      [Header] ->
         {error, invalidHeaders};
      [Key, <<>>] ->
         parseHeaders(T, [{Key, undefined} | Acc]);
      [Key, <<" ", Value/binary>>] ->
         parseHeaders(T, [{Key, Value} | Acc])
   end.

