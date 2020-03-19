-module(agTcpAgencyIns).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   %% 内部行为API
   init/1
   , handleMsg/3
   , terminate/3
]).

-record(srvState, {
   poolName :: poolName(),
   serverName :: serverName(),
   userPassWord :: binary(),
   host :: binary(),
   dbName :: binary(),
   rn :: binary:cp(),
   rnrn :: binary:cp(),
   reconnectState :: undefined | reconnectState(),
   socket :: undefined | inet:socket(),
   timerRef :: undefined | reference()
}).

-type srvState() :: #srvState{}.

-spec init(term()) -> no_return().
init({PoolName, AgencyName, #agencyOpts{reconnect = Reconnect, backlogSize = BacklogSize, reconnectTimeMin = Min, reconnectTimeMax = Max}}) ->
   ReconnectState = agAgencyUtils:initReconnectState(Reconnect, Min, Max),
   self() ! ?miDoNetConnect,
   {ok, #srvState{poolName = PoolName, serverName = AgencyName, rn = binary:compile_pattern(<<"\r\n">>), rnrn = binary:compile_pattern(<<"\r\n\r\n">>), reconnectState = ReconnectState}, #cliState{backlogSize = BacklogSize}}.

-spec handleMsg(term(), srvState(), cliState()) -> {ok, term(), term()}.
handleMsg(#miRequest{method = Method, path = Path, headers = Headers, body = Body, requestId = RequestId, fromPid = FromPid, overTime = OverTime, isSystem = IsSystem} = MiRequest,
   #srvState{serverName = ServerName, host = Host, userPassWord = UserPassWord, dbName = DbName, socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, backlogSize = BacklogSize, requestsIn = RequestsIn, status = Status} = CliState) ->
   case Socket of
      undefined ->
         agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, no_socket}),
         {ok, SrvState, CliState};
      _ ->
         case BacklogNum >= BacklogSize of
            true ->
               ?WARN(ServerName, ":backlog full curNum:~p Total: ~p ~n", [BacklogNum, BacklogSize]),
               agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, backlog_full}),
               {ok, SrvState, CliState};
            _ ->
               case Status of
                  leisure -> %% 空闲模式
                     Request = agHttpProtocol:request(IsSystem, Body, Method, Host, DbName, Path, [{<<"Authorization">>, UserPassWord} | Headers]),
                     case gen_tcp:send(Socket, Request) of
                        ok ->
                           TimerRef =
                              case OverTime of
                                 infinity ->
                                    undefined;
                                 _ ->
                                    erlang:start_timer(OverTime, self(), waiting_over, [{abs, true}])
                              end,
                           {ok, SrvState, CliState#cliState{status = waiting, backlogNum = BacklogNum + 1, curInfo = {FromPid, RequestId, TimerRef}}};
                        {error, Reason} ->
                           ?WARN(ServerName, ":send error: ~p ~p ~p ~n", [Reason, FromPid, RequestId]),
                           gen_tcp:close(Socket),
                           agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socket_send_error}),
                           dealClose(SrvState, CliState, {error, {socket_send_error, Reason}})
                     end;
                  _ ->
                     agAgencyUtils:addQueue(RequestsIn, MiRequest),
                     {ok, SrvState, CliState#cliState{requestsIn = RequestsIn + 1, backlogNum = BacklogNum + 1}}
               end
         end
   end;
handleMsg({tcp, Socket, Data},
   #srvState{serverName = ServerName, rn = Rn, rnrn = RnRn, socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, curInfo = CurInfo, requestsOut = RequestsOut, recvState = RecvState} = CliState) ->
   try agHttpProtocol:response(RecvState, Rn, RnRn, Data) of
      {done, #recvState{statusCode = StatusCode, contentLength = ContentLength, body = Body}} ->
         agAgencyUtils:agencyReply(CurInfo, #requestRet{statusCode = StatusCode, contentLength = ContentLength, body = Body}),
         case agAgencyUtils:getQueue(RequestsOut + 1) of
            undefined ->
               {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1, status = leisure, curInfo = undefined, recvState = undefined}};
            MiRequest ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{backlogNum = BacklogNum - 1, status = leisure, curInfo = undefined, recvState = undefined})
         end;
      {ok, NewRecvState} ->
         {ok, SrvState, CliState#cliState{recvState = NewRecvState}};
      {error, Reason} ->
         ?WARN(ServerName, "handle tcp data error: ~p ~p ~n", [Reason, CurInfo]),
         gen_tcp:close(Socket),
         dealClose(SrvState, CliState, {error, {tcp_data_error, Reason}})
   catch
      E:R:S ->
         ?WARN(ServerName, "handle tcp data crash: ~p:~p~n~p~n ~p ~n ", [E, R, S, CurInfo]),
         gen_tcp:close(Socket),
         dealClose(SrvState, CliState, {error, agency_handledata_error})
   end;
handleMsg({timeout, TimerRef, waiting_over},
   #srvState{socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, curInfo = {FromPid, RequestId, TimerRef}} = CliState) ->
   agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, timeout}),
   %% 之前的数据超时之后 要关闭tcp 然后重新建立连接 以免后面该tcp收到该次超时数据 影响后面请求的接收数据 导致数据错乱
   gen_tcp:close(Socket),
   self() ! ?miDoNetConnect,
   {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1}};
handleMsg({tcp_closed, Socket},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?WARN(ServerName, "connection closed~n", []),
   dealClose(SrvState, CliState, {error, tcp_closed});
handleMsg({tcp_error, Socket, Reason},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?WARN(ServerName, "connection error: ~p~n", [Reason]),
   gen_tcp:close(Socket),
   dealClose(SrvState, CliState, {error, {tcp_error, Reason}});
handleMsg(?miDoNetConnect,
   #srvState{poolName = PoolName, serverName = ServerName, reconnectState = ReconnectState} = SrvState,
   #cliState{requestsOut = RequestsOut} = CliState) ->
   case ?agBeamPool:get(PoolName) of
      #dbOpts{host = Host, port = Port, hostname = HostName, dbName = DbName, userPassword = UserPassword, socketOpts = SocketOpts} ->
         case dealConnect(ServerName, HostName, Port, SocketOpts) of
            {ok, Socket} ->
               NewReconnectState = agAgencyUtils:resetReconnectState(ReconnectState),
               %% 新建连接之后 需要重置之前的buff之类状态数据
               NewCliState = CliState#cliState{status = leisure, recvState = undefined, curInfo = undefined},
               case agAgencyUtils:getQueue(RequestsOut + 1) of
                  undefined ->
                     {ok, SrvState#srvState{userPassWord = UserPassword, dbName = DbName, host = Host, reconnectState = NewReconnectState, socket = Socket}, NewCliState};
                  MiRequest ->
                     dealQueueRequest(MiRequest, SrvState#srvState{socket = Socket, reconnectState = NewReconnectState}, NewCliState)
               end;
            {error, _Reason} ->
               reconnectTimer(SrvState, CliState)
         end;
      _Ret ->
         ?WARN(ServerName, "deal connect not found agBeamPool:get(~p) ret ~p is error ~n", [PoolName, _Ret])
   end;
handleMsg(Msg, #srvState{serverName = ServerName} = SrvState, CliState) ->
   ?WARN(ServerName, "unknown msg: ~p~n", [Msg]),
   {ok, SrvState, CliState}.

-spec terminate(term(), srvState(), cliState()) -> ok.
terminate(_Reason,
   #srvState{timerRef = TimerRef},
   _CliState) ->
   agAgencyUtils:cancelTimer(TimerRef),
   agAgencyUtils:agencyReplyAll({error, shutdown}),
   ok.

dealConnect(ServerName, HostName, Port, SocketOptions) ->
   case inet:getaddrs(HostName, inet) of
      {ok, IPList} ->
         Ip = agMiscUtils:randomElement(IPList),
         case gen_tcp:connect(Ip, Port, SocketOptions, ?DEFAULT_CONNECT_TIMEOUT) of
            {ok, Socket} ->
               {ok, Socket};
            {error, Reason} ->
               ?WARN(ServerName, "connect error: ~p~n", [Reason]),
               {error, Reason}
         end;
      {error, Reason} ->
         ?WARN(ServerName, "getaddrs error: ~p~n", [Reason]),
         {error, Reason}
   end.

dealClose(SrvState, ClientState, Reply) ->
   agAgencyUtils:agencyReplyAll(Reply),
   reconnectTimer(SrvState, ClientState).

reconnectTimer(#srvState{reconnectState = undefined} = SrvState, CliState) ->
   {ok, {SrvState#srvState{socket = undefined}, CliState}};
reconnectTimer(#srvState{reconnectState = ReconnectState} = SrvState, CliState) ->
   #reconnectState{current = Current} = MewReconnectState = agAgencyUtils:updateReconnectState(ReconnectState),
   TimerRef = erlang:send_after(Current, self(), ?miDoNetConnect),
   {ok, SrvState#srvState{reconnectState = MewReconnectState, socket = undefined, timerRef = TimerRef}, CliState}.


dealQueueRequest(#miRequest{method = Method, path = Path, headers = Headers, body = Body, requestId = RequestId, fromPid = FromPid, overTime = OverTime, isSystem = IsSystem},
   #srvState{serverName = ServerName, host = Host, userPassWord = UserPassWord, dbName = DbName, socket = Socket} = SrvState,
   #cliState{requestsOut = RequestsOut} = CliState) ->
   agAgencyUtils:delQueue(RequestsOut + 1),
   case erlang:system_time(millisecond) > OverTime of
      true ->
         %% 超时了
         agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, timeout}),
         case agAgencyUtils:getQueue(RequestsOut + 2) of
            undefined ->
               {ok, SrvState, CliState#cliState{requestsOut = RequestsOut + 1}};
            MiRequest ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOut = RequestsOut + 1})
         end;
      _ ->
         Request = agHttpProtocol:request(IsSystem, Body, Method, Host, DbName, Path, [{<<"Authorization">>, UserPassWord} | Headers]),
         case gen_tcp:send(Socket, Request) of
            ok ->
               TimerRef =
                  case OverTime of
                     infinity ->
                        undefined;
                     _ ->
                        erlang:start_timer(OverTime, self(), waiting_over, [{abs, true}])
                  end,
               {ok, SrvState, CliState#cliState{status = waiting, requestsOut = RequestsOut + 1, curInfo = {FromPid, RequestId, TimerRef}}};
            {error, Reason} ->
               ?WARN(ServerName, ":send error: ~p~n", [Reason]),
               gen_tcp:close(Socket),
               agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socket_send_error}),
               dealClose(SrvState, CliState, {error, socket_send_error})
         end
   end.

