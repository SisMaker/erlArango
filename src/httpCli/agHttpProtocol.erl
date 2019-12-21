-module(agHttpProtocol).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   binPatterns/0,
   headers/1,
   request/5,
   response/1,
   response/3
]).

-record(binPatterns, {
   rn :: binary:cp(),
   rnrn :: binary:cp()
}).

-type binPatterns() :: #binPatterns {}.

-spec binPatterns() -> binPatterns().
binPatterns() ->
   #binPatterns{
      rn = binary:compile_pattern(<<"\r\n">>),
      rnrn = binary:compile_pattern(<<"\r\n\r\n">>)
   }.

-spec headers(requestRet()) -> {ok, headers()} | {error, invalid_headers}.
headers(#requestRet{headers = Headers}) ->
   parseHeaders(Headers, []).

-spec request(method(), host(), path(), headers(), body()) -> iolist().
request(Method, Host, Path, Headers, undefined) ->
   [
      Method, <<" ">>, Path, <<" HTTP/1.1\r\nHost: ">>, Host,
      <<"\r\nConnection: Keep-alive\r\nUser-Agent: erlArango\r\n">>,
      formatHeaders(Headers), <<"\r\n">>
   ];
request(Method, Host, Path, Headers, Body) ->
   ContentLength = integer_to_binary(iolist_size(Body)),
   NewHeaders = [{<<"Content-Length">>, ContentLength} | Headers],
   [
      Method, <<" ">>, Path,
      <<" HTTP/1.1\r\nHost: ">>, Host,
      <<"\r\nConnection: Keep-alive\r\nUser-Agent: erlArango\r\n">>,
      formatHeaders(NewHeaders), <<"\r\n">>, Body
   ].

-spec response(binary()) -> {ok, requestRet(), binary()} | error().
response(Data) ->
   response(Data, undefined, binPatterns()).

-spec response(binary(), undefined | requestRet(), binPatterns()) -> {ok, requestRet(), binary()} | error().
response(Data, undefined, BinPatterns) ->
   case parseStatusLine(Data, BinPatterns) of
      {StatusCode, Reason, Rest} ->
         case splitHeaders(Rest, BinPatterns) of
            {undefined, Headers, Rest2} ->
               {ok, #requestRet{state = done, status_code = StatusCode, reason = Reason, headers = Headers, content_length = undefined}, Rest2};
            {0, Headers, Rest2} ->
               {ok, #requestRet{state = done, status_code = StatusCode, reason = Reason, headers = Headers, content_length = 0}, Rest2};
            {ContentLength, Headers, Rest2} ->
               response(Rest2, #requestRet{state = body, status_code = StatusCode, reason = Reason, headers = Headers, content_length = ContentLength}, BinPatterns);
            {error, Reason2} ->
               {error, Reason2}
         end;
      {error, Reason} ->
         {error, Reason}
   end;
response(Data, #requestRet{state = body, content_length = chunked} = Response, BinPatterns) ->
   case parseChunks(Data, BinPatterns) of
      {ok, Body, Rest} ->
         {ok, Response#requestRet{state = done, body = Body}, Rest};
      {error, Reason} ->
         {error, Reason}
   end;
response(Data, #requestRet{state = body, content_length = ContentLength} = Response, _BinPatterns) when size(Data) >= ContentLength ->
   <<Body:ContentLength/binary, Rest/binary>> = Data,
   {ok, Response#requestRet{state = done, body = Body}, Rest};
response(Data, #requestRet{state = body} = Response, _BinPatterns) ->
   {ok, Response, Data}.

binarySplitGlobal(Bin, Pattern) ->
   case binary:split(Bin, Pattern) of
      [Split, Rest] ->
         [Split | binarySplitGlobal(Rest, Pattern)];
      Rest ->
         Rest
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

formatHeaders(Headers) ->
   [[Key, <<": ">>, Value, <<"\r\n">>] || {Key, Value} <- Headers].


parseChunks(Data, BinPatterns) ->
   parse_chunks(Data, BinPatterns, []).

parse_chunks(Data, BinPatterns, Acc) ->
   case parseChunk(Data, BinPatterns) of
      {ok, <<>>, Rest} ->
         {ok, iolist_to_binary(lists:reverse(Acc)), Rest};
      {ok, Body, Rest} ->
         parse_chunks(Rest, BinPatterns, [Body | Acc]);
      {error, Reason} ->
         {error, Reason}
   end.

parseChunk(Data, #binPatterns{rn = Rn}) ->
   case binary:split(Data, Rn) of
      [Size, Rest] ->
         case parse_chunk_size(Size) of
            undefined ->
               {error, invalid_chunk_size};
            Size2 ->
               parse_chunk_body(Rest, Size2)
         end;
      [Data] ->
         {error, not_enough_data}
   end.

parse_chunk_body(Data, Size) ->
   case Data of
      <<Body:Size/binary, "\r\n", Rest/binary>> ->
         {ok, Body, Rest};
      _ ->
         {error, not_enough_data}
   end.

parse_chunk_size(Bin) ->
   try
      binary_to_integer(Bin, 16)
   catch
      error:badarg ->
         undefined
   end.

parseHeaders([], Acc) ->
   {ok, lists:reverse(Acc)};
parseHeaders([Header | T], Acc) ->
   case binary:split(Header, <<":">>) of
      [Header] ->
         {error, invalid_headers};
      [Key, <<>>] ->
         parseHeaders(T, [{Key, undefined} | Acc]);
      [Key, <<" ", Value/binary>>] ->
         parseHeaders(T, [{Key, Value} | Acc])
   end.

parseStatusLine(Data, #binPatterns{rn = Rn}) ->
   case binary:split(Data, Rn) of
      [Data] ->
         {error, not_enough_data};
      [Line, Rest] ->
         case parseStatusReason(Line) of
            {ok, StatusCode, Reason} ->
               {StatusCode, Reason, Rest};
            {error, Reason} ->
               {error, Reason}
         end
   end.

parseStatusReason(<<"HTTP/1.1 200 OK">>) ->
   {ok, 200, <<"OK">>};
parseStatusReason(<<"HTTP/1.1 204 No Content">>) ->
   {ok, 204, <<"No Content">>};
parseStatusReason(<<"HTTP/1.1 301 Moved Permanently">>) ->
   {ok, 301, <<"Moved Permanently">>};
parseStatusReason(<<"HTTP/1.1 302 Found">>) ->
   {ok, 302, <<"Found">>};
parseStatusReason(<<"HTTP/1.1 403 Forbidden">>) ->
   {ok, 403, <<"Forbidden">>};
parseStatusReason(<<"HTTP/1.1 404 Not Found">>) ->
   {ok, 404, <<"Not Found">>};
parseStatusReason(<<"HTTP/1.1 500 Internal Server Error">>) ->
   {ok, 500, <<"Internal Server Error">>};
parseStatusReason(<<"HTTP/1.1 502 Bad Gateway">>) ->
   {ok, 502, <<"Bad Gateway">>};
parseStatusReason(<<"HTTP/1.1 ", N1, N2, N3, " ", Reason/bits>>)
   when $0 =< N1, N1 =< $9,
   $0 =< N2, N2 =< $9,
   $0 =< N3, N3 =< $9 ->

   StatusCode = (N1 - $0) * 100 + (N2 - $0) * 10 + (N3 - $0),
   {ok, StatusCode, Reason};
parseStatusReason(<<"HTTP/1.0 ", _/binary>>) ->
   {error, unsupported_feature};
parseStatusReason(_) ->
   {error, bad_request}.

splitHeaders(Data, #binPatterns{rn = Rn, rnrn = Rnrn}) ->
   case binary:split(Data, Rnrn) of
      [Data] ->
         {error, not_enough_data};
      [Headers, Rest] ->
         Headers2 = binarySplitGlobal(Headers, Rn),
         ContentLength = contentLength(Headers2),
         {ContentLength, Headers2, Rest}
   end.
