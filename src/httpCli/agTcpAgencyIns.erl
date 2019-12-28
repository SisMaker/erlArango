-module(agTcpAgencyIns).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

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
   reconnectState :: undefined | reconnectState(),
   socket :: undefined | inet:socket(),
   timerRef :: undefined | reference()
}).

-type srvState() :: #srvState{}.

-spec init(term()) -> no_return().
init({PoolName, AgencyName, AgencyOpts}) ->
   BacklogSize = ?GET_FROM_LIST(backlogSize, AgencyOpts, ?DEFAULT_BACKLOG_SIZE),
   ReconnectState = agAgencyUtils:initReconnectState(AgencyOpts),
   self() ! ?miDoNetConnect,
   {ok, #srvState{poolName = PoolName, serverName = AgencyName, reconnectState = ReconnectState}, #cliState{backlogSize = BacklogSize}}.

-spec handleMsg(term(), srvState(), cliState()) -> {ok, term(), term()}.
handleMsg({miRequest, FromPid, _Method, _Path, _Headers, _Body, RequestId, _OverTime},
   #srvState{socket = undefined} = SrvState,
   CliState) ->
   agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, no_socket}),
   {ok, SrvState, CliState};
handleMsg({miRequest, FromPid, Method, Path, Headers, Body, RequestId, OverTime} = MiRequest,
   #srvState{serverName = ServerName, host = Host, userPassWord = UserPassWord, socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, backlogSize = BacklogSize, requestsIn = RequestsIn, status = Status} = CliState) ->
   case BacklogNum >= BacklogSize of
      true ->
         ?WARN(ServerName, ":backlog full curNum:~p Total: ~p ~n", [BacklogNum, BacklogSize]),
         agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, backlog_full}),
         {ok, SrvState, CliState};
      _ ->
         case Status of
            leisure -> %% 空闲模式
               Request = agHttpProtocol:request(Method, Host, Path, [{<<"Authorization">>, UserPassWord} | Headers], Body),
               case gen_tcp:send(Socket, Request) of
                  ok ->
                     TimerRef =
                        case OverTime of
                           infinity ->
                              undefined;
                           _ ->
                              erlang:start_timer(OverTime, self(), waiting, [{abs, true}])
                        end,
                     {ok, SrvState, CliState#cliState{status = waiting, backlogNum = BacklogNum + 1, curInfo = {FromPid, RequestId, TimerRef}}};
                  {error, Reason} ->
                     ?WARN(ServerName, ":send error: ~p~n", [Reason]),
                     gen_tcp:close(Socket),
                     agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socket_send_error}),
                     dealClose(SrvState, CliState, {error, socket_send_error})
               end;
            _ ->
               agAgencyUtils:addQueue(RequestsIn, MiRequest),
               {ok, SrvState, CliState#cliState{requestsIn = RequestsIn + 1, backlogNum = BacklogNum + 1}}
         end
   end;
handleMsg({tcp, Socket, Data},
   #srvState{serverName = ServerName, socket = Socket} = SrvState,
   #cliState{binPatterns = BinPatterns, backlogNum = BacklogNum, curInfo = CurInfo, requestsOut = RequestsOut, recvState = RecvState} = CliState) ->
   try agAgencyUtils:handleData(Data, BinPatterns, RecvState) of
      {ok, waiting_data, NewClientState} ->
         {ok, SrvState, NewClientState};
      {ok, RequestRet, NewClientState} ->
         agAgencyUtils:agencyResponse(RequestRet, CurInfo),
         case agAgencyUtils:getQueue(RequestsOut + 1) of
            undefined ->
               {ok, SrvState, NewClientState#cliState{backlogNum = BacklogNum - 1, status = leisure, curInfo = undefined}};
            MiRequest ->
               dealQueueRequest(MiRequest, SrvState, NewClientState#cliState{backlogNum = BacklogNum - 1, status = leisure, curInfo = undefined})
         end;
      {error, Reason, NewClientState} ->
         ?WARN(ServerName, "handle tcp data error: ~p~n", [Reason]),
         gen_tcp:close(Socket),
         dealClose(SrvState, NewClientState, {error, tcp_data_error})
   catch
      E:R:S ->
         ?WARN(ServerName, "handle tcp data crash: ~p:~p~n~p~n", [E, R, S]),
         gen_tcp:close(Socket),
         dealClose(SrvState, CliState, {{error, agency_handledata_error}})
   end;
handleMsg({timeout, TimerRef, waiting},
   #srvState{socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, curInfo = {FromPid, RequestId, TimerRef}} = CliState) ->
   agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, timeout}),
   %% 之前的数据超时之后 要关闭tcp 然后重新建立连接 以免后面该tcp收到该次超时数据 影响后面请求的接收数据 导致数据错乱
   gen_tcp:close(Socket),
   timer:sleep(1000),
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
   dealClose(SrvState, CliState, {error, tcp_error});
handleMsg(?miDoNetConnect,
   #srvState{poolName = PoolName, serverName = ServerName, reconnectState = ReconnectState} = SrvState,
   #cliState{requestsOut = RequestsOut} = CliState) ->
   case ?agBeamPool:get(PoolName) of
      #poolOpts{hostname = HostName, port = Port, host = Host, userPassword = UserPassword} ->
         case dealConnect(ServerName, HostName, Port, ?DEFAULT_SOCKET_OPTS) of
            {ok, Socket} ->
               NewReconnectState = agAgencyUtils:resetReconnectState(ReconnectState),
               %% 新建连接之后 需要重置之前的buff之类状态数据
               NewCliState = CliState#cliState{binPatterns = agHttpProtocol:binPatterns(), buffer = <<>>, status = leisure, recvState = undefined, curInfo = undefined},
               case agAgencyUtils:getQueue(RequestsOut + 1) of
                  undefined ->
                     {ok, SrvState#srvState{userPassWord = UserPassword, host = Host, reconnectState = NewReconnectState, socket = Socket}, NewCliState};
                  MiRequest ->
                     dealQueueRequest(MiRequest, SrvState#srvState{socket = Socket}, NewCliState)
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


dealQueueRequest({miRequest, FromPid, Method, Path, Headers, Body, RequestId, OverTime},
   #srvState{serverName = ServerName, host = Host, userPassWord = UserPassWord, socket = Socket} = SrvState,
   #cliState{requestsOut = RequestsOut} = CliState) ->
   agAgencyUtils:delQueue(RequestsOut + 1),
   case erlang:system_time(millisecond) > OverTime of
      true ->
         %% 超时了
         case agAgencyUtils:getQueue(RequestsOut + 2) of
            undefined ->
               {ok, SrvState, CliState#cliState{status = waiting, requestsOut = RequestsOut + 1}};
            MiRequest ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOut = RequestsOut + 1})
         end;
      _ ->
         Request = agHttpProtocol:request(Method, Host, Path, [{<<"Authorization">>, UserPassWord} | Headers], Body),
         case gen_tcp:send(Socket, Request) of
            ok ->
               TimerRef =
                  case OverTime of
                     infinity ->
                        undefined;
                     _ ->
                        erlang:start_timer(OverTime, self(), waiting, [{abs, true}])
                  end,
               {ok, SrvState, CliState#cliState{status = waiting, requestsOut = RequestsOut + 1, curInfo = {FromPid, RequestId, TimerRef}}};
            {error, Reason} ->
               ?WARN(ServerName, ":send error: ~p~n", [Reason]),
               gen_tcp:close(Socket),
               agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socket_send_error}),
               dealClose(SrvState, CliState, {error, socket_send_error})
         end
   end.

