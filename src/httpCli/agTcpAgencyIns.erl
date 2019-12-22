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
   ip :: inet:ip_address() | inet:hostname(),
   serverName :: serverName(),
   poolName :: poolName(),
   port :: inet:port_number(),
   reconnectState :: undefined | reconnectState(),
   socket :: undefined | inet:socket(),
   socketOpts :: [gen_tcp:connect_option()],
   backlogNum :: integer(),
   backlogSize :: integer(),
   timerRef :: undefined | reference()
}).

-type srvState() :: #srvState{}.

-spec init(clientOpts()) -> no_return().
init(ClientOpts) ->
   Protocol = ?GET_FROM_LIST(protocol, ClientOpts, ?DEFAULT_PROTOCOL),
   Ip = ?GET_FROM_LIST(ip, ClientOpts, ?DEFAULT_IP),
   Port = ?GET_FROM_LIST(port, ClientOpts, ?DEFAULT_PORTO(Protocol)),
   ReconnectState = agAgencyUtils:initReconnectState(ClientOpts),
   SocketOptions = ?GET_FROM_LIST(socketOpts, ClientOpts, ?DEFAULT_SOCKET_OPTS),
   self() ! ?miDoNetConnect,
   {ok, #srvState{ip = Ip, port = Port, reconnectState = ReconnectState, socketOpts = SocketOptions}, undefined}.

-spec handleMsg(term(), srvState(), cliState()) -> {ok, term(), term()}.
handleMsg({miRequest, FromPid, _RequestContent, _RequestId, _Timeout},
   #srvState{socket = undefined, serverName = Name} = SrvState,
   CliState) ->
   agAgencyUtils:agencyReply(Name, {error, no_socket}, FromPid),
   {ok, SrvState, CliState};
handleMsg({miRequest, FromPid, RequestContent, RequestId, Timeout},
   #srvState{serverName = ServerName, socket = Socket, backlogNum = BacklogNum, backlogSize = BacklogSize} = SrvState,
   ClientState) ->
   case BacklogNum > BacklogSize of
      true ->
         ?WARN(ServerName, ":backlog full curNum:~p Total: ~p", [BacklogNum, BacklogSize]),
         agAgencyUtils:agencyReply(ServerName, {error, socket_closed}, RequestId),
         {ok, SrvState, ClientState};
      _ ->
         try agNetCli:handleRequest(RequestContent, ClientState) of
            {ok, ExtRequestId, Data, NewClientState} ->
               case gen_tcp:send(Socket, Data) of
                  ok ->
                     TimerRef = erlang:start_timer(Timeout, self(), ExtRequestId),
                     agAgencyUtils:addQueue(ExtRequestId, RequestId, TimerRef),
                     {ok, {SrvState, NewClientState}};
                  {error, Reason} ->
                     ?WARN(ServerName, ":send error: ~p", [Reason]),
                     gen_tcp:close(Socket),
                     agAgencyUtils:agencyReply(ServerName, {error, socket_closed}, RequestId),
                     dealClose(SrvState, NewClientState)
               end
         catch
            E:R:S ->
               ?WARN(ServerName, ":miRequest crash: ~p:~p~n~p~n", [E, R, S]),
               agAgencyUtils:agencyReply(ServerName, {error, client_crash}, FromPid),
               {ok, SrvState, ClientState}
         end
   end;
handleMsg({tcp, Socket, Data},
   #srvState{serverName = ServerName, socket = Socket} = SrvState,
   CliState) ->
   try agNetCli:handleData(Data, CliState) of
      {ok, Replies, NewClientState} ->
         agAgencyUtils:agencyResponses(Replies, ServerName),
         {ok, SrvState, NewClientState};
      {error, Reason, NewClientState} ->
         ?WARN(ServerName, "handle tcp data error: ~p", [Reason]),
         gen_tcp:close(Socket),
         dealClose(SrvState, NewClientState)
   catch
      E:R:S ->
         ?WARN(ServerName, "handle tcp data crash: ~p:~p~n~p~n", [E, R, S]),
         gen_tcp:close(Socket),
         dealClose(SrvState, CliState)
   end;
handleMsg({timeout, _TimerRef, ExtRequestId},
   #srvState{serverName = ServerName} = SrvState,
   CliState) ->
   case agAgencyUtils:delQueue(ServerName, ExtRequestId) of
      {ok, Cast, _TimerRef} ->
         agAgencyUtils:agencyReply(ServerName, {error, timeout}, Cast);
      {error, not_found} ->
         ok
   end,
   {ok, SrvState, CliState};
handleMsg({tcp_closed, Socket},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?WARN(ServerName, "connection closed", []),
   dealClose(SrvState, CliState);
handleMsg({tcp_error, Socket, Reason},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->

   ?WARN(ServerName, "connection error: ~p", [Reason]),
   gen_tcp:close(Socket),
   dealClose(SrvState, CliState);
handleMsg(?miDoNetConnect,
   #srvState{ip = Ip, port = Port, serverName = ServerName, reconnectState = ReconnectState, socketOpts = SocketOptions} = SrvState,
   CliState) ->
   case dealConnect(ServerName, Ip, Port, SocketOptions) of
      {ok, Socket} ->
         MewCliState = agHttpProtocol:binPatterns(),
         NewReconnectState = agAgencyUtils:resetReconnectState(ReconnectState),
         {ok, SrvState#srvState{reconnectState = NewReconnectState, socket = Socket}, MewCliState};
      {error, _Reason} ->
         reconnectTimer(SrvState, CliState)
   end;
handleMsg(Msg, #srvState{serverName = ServerName} = SrvState, CliState) ->
   ?WARN(ServerName, "unknown msg: ~p", [Msg]),
   {ok, SrvState, CliState}.

-spec terminate(term(), srvState(), cliState()) -> ok.
terminate(_Reason,
   #srvState{timerRef = TimerRef},
      _CliState) ->
   agAgencyUtils:cancelTimer(TimerRef),
   agAgencyUtils:agencyReplyAll({error, shutdown}),
   ok.

dealConnect(ServerName, Ip, Port, SocketOptions) ->
   case inet:getaddrs(Ip, inet) of
      {ok, Addrs} ->
         Ip2 = agMiscUtils:randomElement(Addrs),
         case gen_tcp:connect(Ip2, Port, SocketOptions,
            ?DEFAULT_CONNECT_TIMEOUT) of
            {ok, Socket} ->
               {ok, Socket};
            {error, Reason} ->
               ?WARN(ServerName, "connect error: ~p", [Reason]),
               {error, Reason}
         end;
      {error, Reason} ->
         ?WARN(ServerName, "getaddrs error: ~p", [Reason]),
         {error, Reason}
   end.

dealClose(SrvState, ClientState) ->
   agAgencyUtils:agencyReplyAll({error, socket_closed}),
   reconnectTimer(SrvState, ClientState).

reconnectTimer(#srvState{reconnectState = undefined} = SrvState, CliState) ->
   {ok, {SrvState#srvState{socket = undefined}, CliState}};
reconnectTimer(#srvState{reconnectState = ReconnectState} = SrvState, CliState) ->
   #reconnectState{current = Current} = MewReconnectState = agAgencyUtils:updateReconnectState(ReconnectState),
   TimerRef = erlang:send_after(Current, self(), ?miDoNetConnect),
   {ok, SrvState#srvState{reconnectState = MewReconnectState, socket = undefined, timerRef = TimerRef}, CliState}.
