-module(agNetCli).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   handleRequest/2,
   handleData/2
]).

-spec handleRequest(term(), cliState()) -> {ok, non_neg_integer(), iodata(), cliState()}.
handleRequest({Method, Host, Path, Headers, Body}, #cliState{requestsOut = RequestsOut} = CliState) ->
   Request = agHttpProtocol:request(Method, Host, Path, Headers, Body),
   {ok, RequestsOut, Request, CliState#cliState{requestsOut = RequestsOut + 1}}.

-spec handleData(binary(), cliState()) -> {ok, [{pos_integer(), term()}], cliState()} | {error, atom(), cliState()}.
handleData(Data, #cliState{binPatterns = BinPatterns, buffer = Buffer, requestsIn = RequestsIn, response = Response} = CliState) ->
   NewData = <<Buffer/binary, Data/binary>>,
   case responses(NewData, RequestsIn, Response, BinPatterns, []) of
      {ok, NewRequestsIn, NewResponse, Responses, Rest} ->
         {ok, Responses, CliState#cliState{buffer = Rest, requestsIn = NewRequestsIn, response = NewResponse}};
      {error, Reason} ->
         {error, Reason, CliState}
   end.

responses(<<>>, RequestsIn, Response, _BinPatterns, Responses) ->
   {ok, RequestsIn, Response, Responses, <<>>};
responses(Data, RequestsIn, Response, BinPatterns, Responses) ->
   case agHttpProtocol:response(Data, Response, BinPatterns) of
      {ok, #requestRet{state = done} = NewResponse, Rest} ->
         NewResponses = [{RequestsIn, {ok, NewResponse}} | Responses],
         responses(Rest, RequestsIn + 1, undefined, BinPatterns, NewResponses);
      {ok, #requestRet{} = NewResponse, Rest} ->
         {ok, RequestsIn, NewResponse, Responses, Rest};
      {error, not_enough_data} ->
         {ok, RequestsIn, Response, Responses, Data};
      {error, _Reason} = Err ->
         Err
   end.
