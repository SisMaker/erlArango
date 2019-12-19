-module(agTcpAgency).
-include("shackle_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    start_link/3,
    init_it/3,
    system_code_change/4,
    system_continue/3,
    system_get_state/1,
    system_terminate/4,

    init/3,
    handle_msg/2,
    terminate/2
]).

-record(state, {
    client           :: client(),
    init_options     :: init_options(),
    ip               :: inet:ip_address() | inet:hostname(),
    name             :: server_name(),
    parent           :: pid(),
    pool_name        :: pool_name(),
    port             :: inet:port_number(),
    reconnect_state  :: undefined | reconnect_state(),
    socket           :: undefined | inet:socket(),
    socket_options   :: [gen_tcp:connect_option()],
    timer_ref        :: undefined | reference()
}).

-type init_opts() :: {pool_name(), client(), client_options()}.
-type state() :: #state {}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(module(), atom(), term(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(Name, Args, SpawnOpts) ->
    proc_lib:start_link(?MODULE, init_it, [Name, self(), Args], infinity, SpawnOpts).

init_it(Name, Parent, Args) ->
    case safeRegister(Name) of
        true ->
            process_flag(trap_exit, true),
            moduleInit(Parent, Args);
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% sys callbacks
-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

-spec system_continue(pid(), [], {module(), atom(), pid(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, State}) ->
    loop(Parent, State).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state(State) ->
    {ok, State}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

safeRegister(Name) ->
    try register(Name, self()) of
        true -> true
    catch
        _:_ -> {false, whereis(Name)}
    end.

moduleInit(Parent, Args) ->
    case ?MODULE:init(Args) of
        {ok, State} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(Parent, State);
        {stop, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason}),
            exit(Reason)
    end.

loop(Parent, State) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, State});
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State);
        Msg ->
            {ok, NewState} = ?MODULE:handleMsg(Msg, State),
            loop(Parent, NewState)
    end.

terminate(Reason, State) ->
    ?MODULE:terminate(Reason, State),
    exit(Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% metal callbacks
-spec init(server_name(), pid(), init_opts()) ->
    no_return().

init(Name, Parent, Opts) ->
    {PoolName, Client, ClientOptions} = Opts,
    self() ! ?MSG_CONNECT,
    ok = shackle_backlog:new(Name),

    InitOptions = ?LOOKUP(init_options, ClientOptions,
        ?DEFAULT_INIT_OPTS),
    Ip = ?LOOKUP(ip, ClientOptions, ?DEFAULT_IP),
    Port = ?LOOKUP(port, ClientOptions),
    ReconnectState = agAgencyUtils:initReconnectState(ClientOptions),
    SocketOptions = ?LOOKUP(socket_options, ClientOptions,
        ?DEFAULT_SOCKET_OPTS),

    {ok, {#state {
        client = Client,
        init_options = InitOptions,
        ip = Ip,
        name = Name,
        parent = Parent,
        pool_name = PoolName,
        port = Port,
        reconnect_state = ReconnectState,
        socket_options = SocketOptions
    }, undefined}}.

-spec handle_msg(term(), {state(), client_state()}) ->
    {ok, term()}.

handle_msg({_, #cast {} = Cast}, {#state {
        socket = undefined,
        name = Name
    } = State, ClientState}) ->

    agAgencyUtils:reply(Name, {error, no_socket}, Cast),
    {ok, {State, ClientState}};
handle_msg({Request, #cast {
        timeout = Timeout
    } = Cast}, {#state {
        client = Client,
        name = Name,
        pool_name = PoolName,
        socket = Socket
    } = State, ClientState}) ->

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
                    agAgencyUtils:reply(Name, {error, socket_closed}, Cast),
                    close(State, ClientState2)
            end
    catch
        ?EXCEPTION(E, R, Stacktrace) ->
            ?WARN(PoolName, "handleRequest crash: ~p:~p~n~p~n",
                [E, R, ?GET_STACK(Stacktrace)]),
            agAgencyUtils:reply(Name, {error, client_crash}, Cast),
            {ok, {State, ClientState}}
    end;
handle_msg({tcp, Socket, Data}, {#state {
        client = Client,
        name = Name,
        pool_name = PoolName,
        socket = Socket
    } = State, ClientState}) ->

    try agNetCli:handleData(Data, ClientState) of
        {ok, Replies, ClientState2} ->
            agAgencyUtils:agencyResponses(Replies, Name),
            {ok, {State, ClientState2}};
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
handle_msg({timeout, ExtRequestId}, {#state {
        name = Name
    } = State, ClientState}) ->

    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, _TimerRef} ->
            agAgencyUtils:reply(Name, {error, timeout}, Cast);
        {error, not_found} ->
            ok
    end,
    {ok, {State, ClientState}};
handle_msg({tcp_closed, Socket}, {#state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "connection closed", []),
    close(State, ClientState);
handle_msg({tcp_error, Socket, Reason}, {#state {
        socket = Socket,
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "connection error: ~p", [Reason]),
    gen_tcp:close(Socket),
    close(State, ClientState);
handle_msg(?MSG_CONNECT, {#state {
        client = Client,
        init_options = Init,
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

                    {ok, {State#state {
                        reconnect_state = ReconnectState2,
                        socket = Socket
                    }, ClientState2}};
        {error, _Reason} ->
            reconnect(State, ClientState)
    end;
handle_msg(Msg, {#state {
        pool_name = PoolName
    } = State, ClientState}) ->

    ?WARN(PoolName, "unknown msg: ~p", [Msg]),
    {ok, {State, ClientState}}.

-spec terminate(term(), term()) ->
    ok.

terminate(_Reason, {#state {
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
    agAgencyUtils:reply_all(Name, {error, shutdown}),
    shackle_backlog:delete(Name),
    ok.

%% private
close(#state {name = Name} = State, ClientState) ->
    agAgencyUtils:reply_all(Name, {error, socket_closed}),
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
reconnect(#state {
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

reconnect_timer(#state {
        reconnect_state = undefined
    } = State, ClientState) ->

    {ok, {State#state {
        socket = undefined
    }, ClientState}};
reconnect_timer(#state {
        reconnect_state = ReconnectState
    } = State, ClientState)  ->

    ReconnectState2 = shackle_backoff:timeout(ReconnectState),
    #reconnect_state {current = Current} = ReconnectState2,
    TimerRef = erlang:send_after(Current, self(), ?MSG_CONNECT),

    {ok, {State#state {
        reconnect_state = ReconnectState2,
        socket = undefined,
        timer_ref = TimerRef
    }, ClientState}}.
