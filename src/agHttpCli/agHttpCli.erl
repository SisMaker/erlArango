-module(agHttpCli).
-include("agHttpCli.hrl").
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   %% Common Request API
   callAgency/5
   , callAgency/6
   , callAgency/7
   , castAgency/5
   , castAgency/6
   , castAgency/7
   , castAgency/8
   , receiveRequestRet/2

   %% Pools API
   , startPool/2
   , startPool/3
   , stopPool/1

   %% Single Process DbAPI
   , connectDb/1
   , disConnectDb/1
   , getCurDbInfo/1
   , useDatabase/2

]).

-spec callAgency(poolNameOrSocket(), method(), path(), headers(), body()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path, Headers, Body) ->
   callAgency(PoolNameOrSocket, Method, Path, Headers, Body, false, ?DEFAULT_TIMEOUT).

-spec callAgency(poolNameOrSocket(), method(), path(), headers(), body(), boolean()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, Headers, Body, IsSystem) ->
   callAgency(PoolNameOrSocket, Method, Path, Headers, Body, IsSystem, ?DEFAULT_TIMEOUT).

-spec callAgency(poolNameOrSocket(), method(), path(), headers(), body(), boolean(), timeout()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, Headers, Body, IsSystem, Timeout) ->
   case castAgency(PoolNameOrSocket, Method, Path, Headers, Body, self(), IsSystem, Timeout) of
      {waitRRT, RequestId, MonitorRef} ->
         receiveRequestRet(RequestId, MonitorRef);
      {error, _Reason} = Err ->
         Err;
      Ret ->
         Ret
   end.

-spec castAgency(poolNameOrSocket(), method(), path(), headers(), body()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, Headers, Body) ->
   castAgency(PoolNameOrSocket, Method, Path, Headers, Body, self(), false, ?DEFAULT_TIMEOUT).

-spec castAgency(poolNameOrSocket(), method(), path(), headers(), body(), boolean()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, Headers, Body, IsSystem) ->
   castAgency(PoolNameOrSocket, Method, Path, Headers, Body, self(), IsSystem, ?DEFAULT_TIMEOUT).

-spec castAgency(poolNameOrSocket(), method(), path(), headers(), body(), boolean(), timeout()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, Headers, Body, IsSystem, Timeout) ->
   castAgency(PoolNameOrSocket, Method, Path, Headers, Body, self(), IsSystem, Timeout).

-spec castAgency(poolNameOrSocket(), method(), path(), headers(), body(), pid(), boolean(), timeout()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, Headers, Body, Pid, IsSystem, Timeout) ->
   OverTime =
      case Timeout of
         infinity -> infinity;
         _ ->
            erlang:monotonic_time(millisecond) + Timeout
      end,
   case erlang:is_atom(PoolNameOrSocket) of
      true ->
         case agAgencyPoolMgrIns:getOneAgency(PoolNameOrSocket) of
            {error, pool_not_found} = Err ->
               Err;
            undefined ->
               {error, undefined_server};
            AgencyName ->
               MonitorRef = erlang:monitor(process, AgencyName),
               RequestId = {AgencyName, MonitorRef},
               catch AgencyName ! #miRequest{method = Method, path = Path, headers = Headers, body = Body, requestId = RequestId, fromPid = Pid, overTime = OverTime, isSystem = IsSystem},
               {waitRRT, RequestId, MonitorRef}
         end;
      _ ->
         case getCurDbInfo(PoolNameOrSocket) of
            {DbName, UserPassWord, Host, Protocol} ->
               Request = agHttpProtocol:request(IsSystem, Body, Method, Host, DbName, Path, [UserPassWord | Headers]),
               case Protocol of
                  tcp ->
                     case gen_tcp:send(PoolNameOrSocket, Request) of
                        ok ->
                           receiveTcpData(undefined, PoolNameOrSocket, binary:compile_pattern(<<"\r\n">>), binary:compile_pattern(<<"\r\n\r\n">>), Method == ?AgHead);
                        {error, Reason} = Err ->
                           ?WARN(castAgency, ":gen_tcp send error: ~p ~n", [Reason]),
                           disConnectDb(PoolNameOrSocket),
                           Err
                     end;
                  ssl ->
                     case ssl:send(PoolNameOrSocket, Request) of
                        ok ->
                           receiveSslData(undefined, PoolNameOrSocket, binary:compile_pattern(<<"\r\n">>), binary:compile_pattern(<<"\r\n\r\n">>), Method == ?AgHead);
                        {error, Reason} = Err ->
                           ?WARN(castAgency, ":ssl send error: ~p ~n", [Reason]),
                           disConnectDb(PoolNameOrSocket),
                           Err
                     end
               end;
            _ ->
               {error, dbinfoNotFound}
         end
   end.

-spec receiveRequestRet(requestId(), reference()) -> {StatusCode :: non_neg_integer(), Body :: binary(), Headers :: binary()} | {error, term()}.
receiveRequestRet(RequestId, MonitorRef) ->
   receive
      #miRequestRet{requestId = RequestId, reply = Reply} ->
         erlang:demonitor(MonitorRef),
         case Reply of
            {_StatusCode, Body, _Headers} ->
               case Body of
                  <<>> ->
                     erlang:setelement(2, Reply, #{});
                  _ ->
                     erlang:setelement(2, Reply, jiffy:decode(Body, [return_maps, copy_strings]))
               end;
            _ ->
               Reply
         end;
      {'DOWN', MonitorRef, process, _Pid, Reason} ->
         {error, {agencyDown, Reason}}
   end.

-spec receiveTcpData(recvState() | undefined, socket(), binary:cp(), binary:cp(), boolean()) -> {ok, term(), term()} | {error, term()}.
receiveTcpData(RecvState, Socket, Rn, RnRn, IsHeadMethod) ->
   receive
      {tcp, Socket, Data} ->
         try agHttpProtocol:response(RecvState, Rn, RnRn, Data, IsHeadMethod) of
            {done, #recvState{statusCode = StatusCode, headers = Headers, body = Body}} ->
               case Body of
                  <<>> ->
                     {ok, #{}, StatusCode, Headers};
                  _ ->
                     {ok, jiffy:decode(Body, [return_maps, copy_strings]), StatusCode, Headers}
               end;
            {ok, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket, Rn, RnRn, IsHeadMethod);
            {error, Reason} ->
               ?WARN(receiveTcpData, "handle tcp data error: ~p ~n", [Reason]),
               disConnectDb(Socket),
               {error, {tcpDataError, Reason}}
         catch
            E:R:S ->
               ?WARN(receiveTcpData, "handle tcp data crash: ~p:~p~n~p ~n ", [E, R, S]),
               disConnectDb(Socket),
               {error, handledataError}
         end;
      {tcp_closed, Socket} ->
         disConnectDb(Socket),
         {error, tcp_closed};
      {tcp_error, Socket, Reason} ->
         disConnectDb(Socket),
         {error, {tcp_error, Reason}}
   end.

-spec receiveSslData(recvState() | undefined, socket(), binary:cp(), binary:cp(), boolean()) -> {ok, term(), term()} | {error, term()}.
receiveSslData(RecvState, Socket, Rn, RnRn, IsHeadMethod) ->
   receive
      {ssl, Socket, Data} ->
         try agHttpProtocol:response(RecvState, Rn, RnRn, Data, IsHeadMethod) of
            {done, #recvState{statusCode = StatusCode, headers = Headers, body = Body}} ->
               case Body of
                  <<>> ->
                     {ok, #{}, StatusCode, Headers};
                  _ ->
                     {ok, jiffy:decode(Body, [return_maps, copy_strings]), StatusCode, Headers}
               end;
            {ok, NewRecvState} ->
               receiveSslData(NewRecvState, Socket, Rn, RnRn, IsHeadMethod);
            {error, Reason} ->
               ?WARN(receiveSslData, "handle tcp data error: ~p ~n", [Reason]),
               disConnectDb(Socket),
               {error, {sslDataError, Reason}}
         catch
            E:R:S ->
               ?WARN(receiveSslData, "handle tcp data crash: ~p:~p~n~p ~n ", [E, R, S]),
               disConnectDb(Socket),
               {error, handledataError}
         end;
      {ssl_closed, Socket} ->
         disConnectDb(Socket),
         {error, ssl_closed};
      {ssl_error, Socket, Reason} ->
         disConnectDb(Socket),
         {error, {ssl_error, Reason}}
   end.

-spec startPool(poolName(), dbCfgs()) -> ok | {error, poolNameUsed}.
startPool(PoolName, DbCfgs) ->
   agAgencyPoolMgrIns:startPool(PoolName, DbCfgs, []).

-spec startPool(poolName(), dbCfgs(), agencyCfgs()) -> ok | {error, poolNameUsed}.
startPool(PoolName, DbCfgs, AgencyCfgs) ->
   agAgencyPoolMgrIns:startPool(PoolName, DbCfgs, AgencyCfgs).

-spec stopPool(poolName()) -> ok | {error, poolNotStarted}.
stopPool(PoolName) ->
   agAgencyPoolMgrIns:stopPool(PoolName).

-spec connectDb(dbCfgs()) -> {ok, socket()} | {error, term()}.
connectDb(DbCfgs) ->
   #dbOpts{
      host = Host,
      port = Port,
      hostname = HostName,
      dbName = DbName,
      protocol = Protocol,
      userPassword = UserPassword,
      socketOpts = SocketOpts
   } = agMiscUtils:dbOpts(DbCfgs),
   case inet:getaddrs(HostName, inet) of
      {ok, IPList} ->
         Ip = agMiscUtils:randomElement(IPList),
         case Protocol of
            tcp ->
               case gen_tcp:connect(Ip, Port, SocketOpts, ?DEFAULT_CONNECT_TIMEOUT) of
                  {ok, Socket} ->
                     setCurDbInfo(Socket, DbName, UserPassword, Host, Protocol),
                     {ok, Socket};
                  {error, Reason} = Err ->
                     ?WARN(connectDb, "connect error: ~p~n", [Reason]),
                     Err
               end;
            ssl ->
               case ssl:connect(Ip, Port, SocketOpts, ?DEFAULT_CONNECT_TIMEOUT) of
                  {ok, Socket} ->
                     setCurDbInfo(Socket, DbName, UserPassword, Host, Protocol),
                     {ok, Socket};
                  {error, Reason} = Err ->
                     ?WARN(connectDb, "connect error: ~p~n", [Reason]),
                     Err
               end
         end;
      {error, Reason} = Err ->
         ?WARN(connectDb, "getaddrs error: ~p~n", [Reason]),
         Err
   end.

-spec disConnectDb(socket()) -> ok | {error, term()}.
disConnectDb(Socket) ->
   case erlang:erase({'$agDbInfo', Socket}) of
      undefined ->
         ignore;
      {_DbName, _UserPassword, _Host, Protocol} ->
         case Protocol of
            tcp ->
               gen_tcp:close(Socket);
            ssl ->
               ssl:close(Socket)
         end
   end.

-spec setCurDbInfo(socket(), binary(), tuple(), host(), protocol()) -> term().
setCurDbInfo(Socket, DbName, UserPassword, Host, Protocol) ->
   erlang:put({'$agDbInfo', Socket}, {DbName, UserPassword, Host, Protocol}).

-spec getCurDbInfo(socket()) -> term().
getCurDbInfo(Socket) ->
   erlang:get({'$agDbInfo', Socket}).

-spec useDatabase(socket(), binary()) -> ok.
useDatabase(Socket, NewDbName) ->
   case erlang:get({'$agDbInfo', Socket}) of
      undefined ->
         ignore;
      {_DbName, UserPassword, Host, Protocol} ->
         erlang:put({'$agDbInfo', Socket}, {<<"/_db/", NewDbName/binary>>, UserPassword, Host, Protocol})
   end,
   ok.
