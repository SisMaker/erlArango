-module(agTcpAgency).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    %% 内部行为API
    start_link/3,
    init_it/3,
    system_code_change/4,
    system_continue/3,
    system_get_state/1,
    system_terminate/4,
    init/1,
    handle_msg/4,
    terminate/2
]).

-record(srvState, {
    initOpts         :: initOpts(),
    ip               :: inet:ip_address() | inet:hostname(),
    name             :: serverName(),
    pool_name        :: pool_name(),
    port             :: inet:port_number(),
    reconnect_state  :: undefined | reconnect_state(),
    socket           :: undefined | inet:socket(),
    socket_options   :: [gen_tcp:connect_option()],
    timer_ref        :: undefined | reference()
}).

-record(cliState, {
    initOpts         :: initOpts(),
    ip               :: inet:ip_address() | inet:hostname(),
    name             :: serverName(),
    pool_name        :: pool_name(),
    port             :: inet:port_number(),
    reconnect_state  :: undefined | reconnect_state(),
    socket           :: undefined | inet:socket(),
    socket_options   :: [gen_tcp:connect_option()],
    timer_ref        :: undefined | reference()
}).

-type state() :: #srvState {}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(module(), atom(), term(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(ServerName, Args, SpawnOpts) ->
    proc_lib:start_link(?MODULE, init_it, [ServerName, self(), Args], infinity, SpawnOpts).

init_it(ServerName, Parent, Args) ->
    case safeRegister(ServerName) of
        true ->
            process_flag(trap_exit, true),
            moduleInit(Parent, Args);
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% sys callbacks
-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(MiscState, _Module, _OldVsn, _Extra) ->
    {ok, MiscState}.

-spec system_continue(pid(), [], {module(), atom(), pid(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, SrvState, CliState}) ->
    loop(Parent, SrvState, CliState).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state({_Parent, SrvState, _CliState}) ->
    {ok, SrvState}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _Parent, _Debug, {_Parent, SrvState, CliState}) ->
    terminate(Reason, SrvState, CliState).

safeRegister(Name) ->
    try register(Name, self()) of
        true -> true
    catch
        _:_ -> {false, whereis(Name)}
    end.

moduleInit(Parent, Args) ->
    case ?MODULE:init(Args) of
        {ok, SrvState, CliState} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(Parent, SrvState, CliState);
        {stop, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason}),
            exit(Reason)
    end.

loop(Parent, SrvState, CliState) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, SrvState, CliState});
        {'EXIT', Parent, Reason} ->
            terminate(Reason, SrvState, CliState);
        Msg ->
            {ok, NewSrvState, NewCliState} = ?MODULE:handleMsg(Msg, SrvState, CliState),
            loop(Parent, NewSrvState, NewCliState)
    end.

terminate(Reason, SrvState, CliState) ->
    ?MODULE:terminate(Reason, SrvState, CliState),
    exit(Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(clientOpts()) -> no_return().
init(ClientOpts) ->
    self() ! ?MSG_CONNECT,

    %%ok = shackle_backlog:new(Name),

    InitOptions = ?GET_FROM_LIST(initOpts, ClientOpts, ?DEFAULT_INIT_OPTS),
    Protocol = ?GET_FROM_LIST(protocol, ClientOpts, ?DEFAULT_PROTOCOL),
    Ip = ?GET_FROM_LIST(ip, ClientOpts, ?DEFAULT_IP),
    Port = ?GET_FROM_LIST(port, ClientOpts, ?DEFAULT_PORTO(Protocol)),
    ReconnectState = agAgencyUtils:initReconnectState(ClientOpts),
    SocketOptions = ?GET_FROM_LIST(socketOpts, ClientOptions, ?DEFAULT_SOCKET_OPTS),
    {ok, #srvState{initOpts = InitOptions, ip = Ip, port = Port, reconnect_state = ReconnectState, socket_options = SocketOptions}, undefined}.

-spec handleMsg(term(), {state(), client_state()}) -> {ok, term()}.
handleMsg({_, #request{} = Cast}, #srvState{socket = undefined, name = Name} = SrvState, CliState) ->
    agAgencyUtils:agencyReply(Name, {error, no_socket}, Cast),
    {ok, {SrvState, CliState}};
handleMsg({Request, #request{timeout = Timeout} = Cast},
   #srvState{name = Name, pool_name = PoolName, socket = Socket} = State,
   ClientState) ->
    try agNetCli:handleRequest(Request, ClientState) of
        {ok, ExtRequestId, Data, ClientState2} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    Msg = {timeout, ExtRequestId},
                    TimerRef = erlang:send_after(Timeout, self(), Msg),
                    shackle_queue:add(ExtRequestId, Cast, TimerRef),
                    {ok, {State, ClientState2}};
                {error, Reason} ->
                    ?WARN(PoolName, "send error: ~p", [Reason]),
                    gen_tcp:close(Socket),
                    agAgencyUtils:agencyReply(Name, {error, socket_closed}, Cast),
                    close(State, ClientState2)
            end
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handleRequest crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            agAgencyUtils:agencyReply(Name, {error, client_crash}, Cast),
            {ok, {State, ClientState}}
    end;
handleMsg({tcp, Socket, Data},
   #srvState{name = Name, pool_name = PoolName, socket = Socket} = SrvState,
   CliState) ->

    try agNetCli:handleData(Data, CliState) of
        {ok, Replies, ClientState2} ->
            agAgencyUtils:agencyResponses(Replies, Name),
            {ok, SrvState, ClientState2};
        {error, Reason, ClientState2} ->
            ?WARN(PoolName, "handleData error: ~p", [Reason]),
            gen_tcp:close(Socket),
            close(State, ClientState2)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handleData crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            gen_tcp:close(Socket),
            close(State, ClientState)
    end;
handleMsg({timeout, ExtRequestId}, {#srvState{
        name = Name
    } = State, ClientState}) ->

    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, _TimerRef} ->
            agAgencyUtils:agencyReply(Name, {error, timeout}, Cast);
        {error, not_found} ->
            ok
    end,
    {ok, {State, ClientState}};
handleMsg({tcp_closed, Socket}, {#srvState{
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "connection closed", []),
    close(State, ClientState);
handleMsg({tcp_error, Socket, Reason}, {#srvState{
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "connection error: ~p", [Reason]),
    gen_tcp:close(Socket),
    close(State, ClientState);
handleMsg(?MSG_CONNECT, {#srvState{
        client = Client,
        initOpts = Init,
        ip = Ip,
        pool_name = PoolName,
        port = Port,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    } = State, ClientState}) ->

    case connect(PoolName, Ip, Port, SocketOptions) of
        {ok, Socket} ->
            ClientState2 = agHttpProtocol:bin_patterns(),
            ReconnectState2 = agAgencyUtils:resetReconnectState(ReconnectState),

                    {ok, {State#srvState{
                        reconnect_state = ReconnectState2,
                        socket = Socket
                    }, ClientState2}};
        {error, _Reason} ->
            reconnect(State, ClientState)
    end;
handleMsg(Msg, {#srvState{
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "unknown msg: ~p", [Msg]),
    {ok, {State, ClientState}}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, {#srvState{
        client = Client,
        name = Name,
        pool_name = PoolName,
        timer_ref = TimerRef
    }, ClientState}) ->

    agAgencyUtils:cancel_timer(TimerRef),
    try agNetCli:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    agAgencyUtils:agencyReplyAll(Name, {error, shutdown}),
    shackle_backlog:delete(Name),
    ok.

%% private
close(#srvState{name = Name} = State, ClientState) ->
    agAgencyUtils:agencyReplyAll(Name, {error, socket_closed}),
    reconnect(State, ClientState).

connect(PoolName, Ip, Port, SocketOptions) ->
    case inet:getaddrs(Ip, inet) of
        {ok, Addrs} ->
            Ip2 = agMiscUtils:randomElement(Addrs),
            case gen_tcp:connect(Ip2, Port, SocketOptions,
                ?DEFAULT_CONNECT_TIMEOUT) of
                {ok, Socket} ->
                    {ok, Socket};
                {error, Reason} ->
                    ?WARN(PoolName, "connect error: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?WARN(PoolName, "getaddrs error: ~p", [Reason]),
            {error, Reason}
    end.

reconnect(State, undefined) ->
    reconnect_timer(State, undefined);
reconnect(#srvState{
        client = Client,
        pool_name = PoolName
    } = State, ClientState) ->

    try agNetCli:terminate(ClientState)
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "terminate crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)])
    end,
    reconnect_timer(State, ClientState).

reconnect_timer(#srvState{
        reconnect_state = undefined
    } = State, ClientState) ->

    {ok, {State#srvState{
        socket = undefined
    }, ClientState}};
reconnect_timer(#srvState{
        reconnect_state = ReconnectState
    } = State, ClientState)  ->

    ReconnectState2 = shackle_backoff:timeout(ReconnectState),
    #reconnect_state {current = Current} = ReconnectState2,
    TimerRef = erlang:send_after(Current, self(), ?MSG_CONNECT),

    {ok, {State#srvState{
        reconnect_state = ReconnectState2,
        socket = undefined,
        timer_ref = TimerRef
    }, ClientState}}.
