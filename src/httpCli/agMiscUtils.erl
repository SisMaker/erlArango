-module(agMiscUtils).

-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   parseUrl/1
   , random/1
   , randomElement/1
   , warnMsg/3
]).

-spec parseUrl(binary()) -> dbUrl() | {error, invalid_url}.
parseUrl(<<"http://", Rest/binary>>) ->
   parseUrl(tcp, Rest);
parseUrl(<<"https://", Rest/binary>>) ->
   parseUrl(ssl, Rest);
parseUrl(_) ->
   {error, invalid_url}.

parseUrl(Protocol, Rest) ->
   {Host, Path} =
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

   #dbUrl{
      host = Host,
      path = Path,
      port = Port,
      hostname = Hostname,
      protocol = Protocol
   }.

%% public
-export([

]).



-spec random(pos_integer()) -> non_neg_integer().
random(1) -> 1;
random(N) ->
   rand:uniform(N).

-spec randomElement([term()]) -> term().

randomElement([X]) ->
   X;
randomElement([_ | _] = List) ->
   T = list_to_tuple(List),
   element(random(tuple_size(T)), T).

-spec warnMsg(pool_name(), string(), [term()]) -> ok.
warnMsg(Pool, Format, Data) ->
   error_logger:warning_msg("[~p] " ++ Format, [Pool | Data]).
