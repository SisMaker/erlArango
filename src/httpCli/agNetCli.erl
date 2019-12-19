-module(agNetCli).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   handleRequest/2,
   handleData/2,
   terminate/1
]).

-record(state, {
   binPatterns :: tuple(),
   buffer = <<>> :: binary(),
   response :: undefined | requestRet(),
   requestsIn = 0 :: non_neg_integer(),
   requestsOut = 0 :: non_neg_integer()
}).

-type state() :: #state {}.

-spec handleRequest(term(), state()) -> {ok, non_neg_integer(), iodata(), state()}.
handleRequest({request, Method, Path, Headers, Host, Body}, #state{requestsOut = RequestsOut} = State) ->
   Request = agHttpProtocol:request(Method, Path, Headers, Host, Body),
   {ok, RequestsOut, Request, State#state{requestsOut = RequestsOut + 1}}.

-spec handleData(binary(), state()) -> {ok, [{pos_integer(), term()}], state()} | {error, atom(), state()}.
handleData(Data, #state{binPatterns = BinPatterns, buffer = Buffer, requestsIn = RequestsIn, response = Response} = State) ->
   Data2 = <<Buffer/binary, Data/binary>>,
   case responses(Data2, RequestsIn, Response, BinPatterns, []) of
      {ok, RequestsIn2, Response2, Responses, Rest} ->
         {ok, Responses, State#state{
            buffer = Rest,
            requestsIn = RequestsIn2,
            response = Response2
         }};
      {error, Reason} ->
         {error, Reason, State}
   end.

-spec terminate(state()) -> ok.
terminate(_State) ->
   ok.

responses(<<>>, RequestsIn, Response, _BinPatterns, Responses) ->
   {ok, RequestsIn, Response, Responses, <<>>};
responses(Data, RequestsIn, Response, BinPatterns, Responses) ->
   case agHttpProtocol:response(Data, Response, BinPatterns) of
      {ok, #requestRet{state = done} = Response2, Rest} ->
         Responses2 = [{RequestsIn, {ok, Response2}} | Responses],
         responses(Rest, RequestsIn + 1, undefined, BinPatterns, Responses2);
      {ok, #requestRet{} = Response2, Rest} ->
         {ok, RequestsIn, Response2, Responses, Rest};
      {error, not_enough_data} ->
         {ok, RequestsIn, Response, Responses, Data};
      {error, _Reason} = E ->
         E
   end.
