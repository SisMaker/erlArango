-module(agTcpAgencyIns).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   %% 内部行为API
   init/1,
   handleMsg/3,
   terminate/3
]).

-record(srvState, {
   poolName :: poolName(),
   serverName :: serverName(),
   reconnectState :: undefined | reconnectState(),
   socket :: undefined | inet:socket(),
   socketOpts :: [gen_tcp:connect_option()],
   timerRef :: undefined | reference()
}).

-type srvState() :: #srvState{}.

-spec init(term()) -> no_return().
init({PoolName, AgencyName, AgencyOpts}) ->
   SocketOptions = ?GET_FROM_LIST(socketOpts, AgencyOpts, ?DEFAULT_SOCKET_OPTS),
   BacklogSize = ?GET_FROM_LIST(backlogSize, AgencyOpts, ?DEFAULT_BACKLOG_SIZE),
   ReconnectState = agAgencyUtils:initReconnectState(AgencyOpts),
   self() ! ?miDoNetConnect,
   {ok, #srvState{poolName = PoolName, serverName = AgencyName, reconnectState = ReconnectState, socketOpts = SocketOptions}, #cliState{backlogSize = BacklogSize}}.

-spec handleMsg(term(), srvState(), cliState()) -> {ok, term(), term()}.
handleMsg({miRequest, FromPid, _RequestContent, RequestId, _Timeout},
   #srvState{socket = undefined} = SrvState,
   CliState) ->
   agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, no_socket}),
   {ok, SrvState, CliState};
handleMsg({miRequest, FromPid, RequestContent, RequestId, Timeout},
   #srvState{serverName = ServerName, socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, backlogSize = BacklogSize} = ClientState) ->
   ?WARN(ServerName, "miRequest data ~p~n",[RequestContent]),
   case BacklogNum > BacklogSize of
      true ->
         ?WARN(ServerName, ":backlog full curNum:~p Total: ~p ~n", [BacklogNum, BacklogSize]),
         agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, backlog_full}),
         {ok, SrvState, ClientState};
      _ ->
         try agNetCli:handleRequest(RequestContent, ClientState) of
            {ok, ExtRequestId, Data, NewClientState} ->
               case gen_tcp:send(Socket, Data) of
                  ok ->
                     TimerRef = erlang:start_timer(Timeout, self(), ExtRequestId),
                     agAgencyUtils:addQueue(ExtRequestId, FromPid, RequestId, TimerRef),
                     {ok, SrvState, NewClientState#cliState{backlogNum = BacklogNum + 1}};
                  {error, Reason} ->
                     ?WARN(ServerName, ":send error: ~p~n", [Reason]),
                     gen_tcp:close(Socket),
                     agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socket_send_error}),
                     dealClose(SrvState, NewClientState, {error, socket_send_error})
               end
         catch
            E:R:S ->
               ?WARN(ServerName, ":miRequest crash: ~p:~p~n~p~n", [E, R, S]),
               agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, agency_crash}),
               {ok, SrvState, ClientState}
         end
   end;
handleMsg({tcp, Socket, Data},
   #srvState{serverName = ServerName, socket = Socket} = SrvState,
   CliState) ->
   ?WARN(ServerName, "get tcp data ~p~n",[Data]),
   try agNetCli:handleData(Data, CliState) of
      {ok, Replies, NewClientState} ->
         agAgencyUtils:agencyResponses(Replies, ServerName),
         {ok, SrvState, NewClientState};
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
handleMsg({timeout, _TimerRef, ExtRequestId},
   #srvState{serverName = ServerName} = SrvState,
   CliState) ->
   case agAgencyUtils:delQueue(ExtRequestId) of
      {FormPid, RequestId, _TimerRef} ->
         agAgencyUtils:agencyReply(FormPid, RequestId, undefined, {error, timeout});
      undefined ->
         ?WARN(ServerName, "timeout not found ExtRequestId ~p~n", [ExtRequestId]),
         ok
   end,
   {ok, SrvState, CliState};
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
   #srvState{poolName = PoolName, serverName = ServerName, reconnectState = ReconnectState, socketOpts = SocketOptions} = SrvState,
   CliState) ->
   case ?agBeamPool:get(PoolName) of
      #poolOpts{hostname = HostName, port = Port} ->
         case dealConnect(ServerName, HostName, Port, SocketOptions) of
            {ok, Socket} ->
               NewReconnectState = agAgencyUtils:resetReconnectState(ReconnectState),
               {ok, SrvState#srvState{reconnectState = NewReconnectState, socket = Socket}, CliState#cliState{binPatterns = agHttpProtocol:binPatterns()}};
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
         case gen_tcp:connect(Ip, Port, SocketOptions,
            ?DEFAULT_CONNECT_TIMEOUT) of
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
