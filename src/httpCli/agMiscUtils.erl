-module(agMiscUtils).

-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   parseUrl/1
   , warnMsg/3
   , getListValue/3
   , randomElement/1
]).

-spec parseUrl(binary()) -> poolOpts() | {error, invalid_url}.
parseUrl(<<"http://", Rest/binary>>) ->
   parseUrl(tcp, Rest);
parseUrl(<<"https://", Rest/binary>>) ->
   parseUrl(ssl, Rest);
parseUrl(_) ->
   {error, invalid_url}.

parseUrl(Protocol, Rest) ->
   {Host, _Path} =
      case binary:split(Rest, <<"/">>, [trim]) of
         [UrlHost] ->
            {UrlHost, <<"/">>};
         [UrlHost, UrlPath] ->
            {UrlHost, <<"/", UrlPath/binary>>}
      end,

   {Hostname, Port} =
      case binary:split(Host, <<":">>, [trim]) of
         [Host] ->
            case Protocol of
               tcp ->
                  {Host, 80};
               ssl ->
                  {Host, 443}
            end;
         [UrlHostname, UrlPort] ->
            {UrlHostname, binary_to_integer(UrlPort)}
      end,
   #poolOpts{host = Host, port = Port, hostname = binary_to_list(Hostname), protocol = Protocol}.

getListValue(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
      false ->
         Default;
      {Key, Value} ->
         Value
   end.

-spec warnMsg(term(), string(), [term()]) -> ok.
warnMsg(Tag, Format, Data) ->
   error_logger:warning_msg("[~p] " ++ Format, [Tag | Data]).

-spec randomElement([term()]) -> term().
randomElement([X]) ->
   X;
randomElement([_ | _] = List) ->
   T = list_to_tuple(List),
   element(rand:uniform(tuple_size(T)), T).
