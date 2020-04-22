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
   , toBinary/1
   , spellQueryPars/1
   , getHeaderValue/2
   , lookHeader/2
]).

-spec parseUrl(binary()) -> dbOpts() | {error, invalidUrl}.
parseUrl(<<"http://", Rest/binary>>) ->
   parseUrl(tcp, Rest);
parseUrl(<<"https://", Rest/binary>>) ->
   parseUrl(ssl, Rest);
parseUrl(_) ->
   {error, invalidUrl}.

-spec parseUrl(protocol(), binary()) -> dbOpts().
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

-spec dbOpts(list()) -> dbOpts().
dbOpts(DbCfgs) ->
   BaseUrl = ?GET_FROM_LIST(baseUrl, DbCfgs, ?DEFAULT_BASE_URL),
   DbName = ?GET_FROM_LIST(dbName, DbCfgs, ?DEFAULT_DBNAME),
   User = ?GET_FROM_LIST(user, DbCfgs, ?DEFAULT_USER),
   Password = ?GET_FROM_LIST(password, DbCfgs, ?DEFAULT_PASSWORD),
   PoolSize = ?GET_FROM_LIST(poolSize, DbCfgs, ?DEFAULT_POOL_SIZE),
   SocketOpts = ?GET_FROM_LIST(socketOpts, DbCfgs, ?DEFAULT_SOCKET_OPTS),
   DbOpts = agMiscUtils:parseUrl(BaseUrl),
   UserPasswordBase64 = {<<"Authorization">>, <<"Basic ", (base64:encode(<<User/binary, ":", Password/binary>>))/binary>>},
   DbOpts#dbOpts{dbName = <<"/_db/", DbName/binary>>, userPassword = UserPasswordBase64, poolSize = PoolSize, socketOpts = SocketOpts}.

-spec agencyOpts(list()) -> agencyOpts().
agencyOpts(AgencyCfgs) ->
   IsReconnect = ?GET_FROM_LIST(reconnect, AgencyCfgs, ?DEFAULT_IS_RECONNECT),
   BacklogSize = ?GET_FROM_LIST(backlogSize, AgencyCfgs, ?DEFAULT_BACKLOG_SIZE),
   Min = ?GET_FROM_LIST(reconnectTimeMin, AgencyCfgs, ?DEFAULT_RECONNECT_MIN),
   Max = ?GET_FROM_LIST(reconnectTimeMax, AgencyCfgs, ?DEFAULT_RECONNECT_MAX),
   #agencyOpts{reconnect = IsReconnect, backlogSize = BacklogSize, reconnectTimeMin = Min, reconnectTimeMax = Max}.

-spec getListValue(term(), list(), term()) -> term().
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

-spec toBinary(term()) -> binary().
toBinary(Value) when is_integer(Value) -> integer_to_binary(Value);
toBinary(Value) when is_list(Value) -> list_to_binary(Value);
toBinary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 6}, compact]);
toBinary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
toBinary(Value) when is_binary(Value) -> Value;
toBinary([Tuple | PropList] = Value) when is_list(PropList) and is_tuple(Tuple) ->
   lists:map(fun({K, V}) -> {toBinary(K), toBinary(V)} end, Value);
toBinary(Value) -> term_to_binary(Value).

-spec spellQueryPars(list()) -> binary().
spellQueryPars([]) ->
   <<>>;
spellQueryPars([{Key, Value}]) ->
   <<"?", (toBinary(Key))/binary, "=", (toBinary(Value))/binary>>;
spellQueryPars([{Key, Value} | Tail]) ->
   FirstBinary = <<"?", (toBinary(Key))/binary, "=", (toBinary(Value))/binary>>,
   TailBinary = <<<<"&", (toBinary(OtherKey))/binary, "=", (toBinary(OtherValue))/binary>> || {OtherKey, OtherValue} <- Tail>>,
   <<FirstBinary/binary, TailBinary/binary>>.

-spec getHeaderValue(binary(), binary()) -> binary().
getHeaderValue(Header, HeaderBin) ->
   HeadersList = binary:split(HeaderBin, <<"\r\n">>, [global]),
   lookHeader(Header, HeadersList).

-spec lookHeader(binary, list()) -> binary().
lookHeader(_Header, []) ->
   undefined;
lookHeader(Header, [H | T]) ->
   case binary:split(H, <<": ">>) of
      [Header, Value] ->
         Value;
      _ ->
         lookHeader(Header, T)
   end.

