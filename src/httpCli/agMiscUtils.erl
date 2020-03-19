-module(agMiscUtils).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   parseUrl/1
   , dbOpts/1
   , agencyOpts/1
   , warnMsg/3
   , getListValue/3
   , randomElement/1
]).

-spec parseUrl(binary()) -> dbOpts() | {error, invalid_url}.
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
   #dbOpts{host = Host, port = Port, hostname = binary_to_list(Hostname), protocol = Protocol}.

dbOpts(DbCfgs) ->
   BaseUrl = ?GET_FROM_LIST(baseUrl, DbCfgs, ?DEFAULT_BASE_URL),
   DbName = ?GET_FROM_LIST(dbName, DbCfgs, ?USER_PASSWORD),
   UserPassword = ?GET_FROM_LIST(userPassword, DbCfgs, ?USER_PASSWORD),
   PoolSize = ?GET_FROM_LIST(poolSize, DbCfgs, ?DEFAULT_POOL_SIZE),
   SocketOpts = ?GET_FROM_LIST(socketOpts, DbCfgs, ?DEFAULT_SOCKET_OPTS),
   DbOpts = agMiscUtils:parseUrl(BaseUrl),
   UserPasswordBase64 = <<"Basic ", (base64:encode(UserPassword))/binary>>,
   DbOpts#dbOpts{dbName = DbName, userPassword = UserPasswordBase64, poolSize = PoolSize, socketOpts = SocketOpts}.

agencyOpts(AgencyCfgs) ->
   IsReconnect = ?GET_FROM_LIST(reconnect, AgencyCfgs, ?DEFAULT_BASE_URL),
   BacklogSize = ?GET_FROM_LIST(backlogSize, AgencyCfgs, ?USER_PASSWORD),
   Min = ?GET_FROM_LIST(reconnectTimeMin, AgencyCfgs, ?USER_PASSWORD),
   Max = ?GET_FROM_LIST(reconnectTimeMax, AgencyCfgs, ?DEFAULT_POOL_SIZE),
   #agencyOpts{reconnect = IsReconnect, backlogSize = BacklogSize, reconnectTimeMin = Min, reconnectTimeMax = Max}.


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
