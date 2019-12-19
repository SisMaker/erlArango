-module(agAgencyUtils).

-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    cancelTimer/1,
    agencyResponses/2,
    initReconnectState/1,
    resetReconnectState/1,
    reply/3,
    reply_all/2
]).

-spec cancelTimer(undefined | reference()) -> ok.
cancelTimer(undefined) -> ok;
cancelTimer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
        false ->
            %% 找不到计时器，我们还没有看到超时消息
            receive
                {timeout, TimerRef, _Msg} ->
                    %% 丢弃该超时消息
                    ok
            end;
        _ ->
            %% Timer 已经运行了
            ok
    end.

-spec agencyResponses([response()], server_name()) -> ok.
agencyResponses([], _Name) ->
    ok;
agencyResponses([{ExtRequestId, Reply} | T], Name) ->
    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            reply(Name, Reply, Cast);
        {error, not_found} ->
            ok
    end,
    agencyResponses(T, Name).

-spec initReconnectState(client_options()) -> reconnect_state() | undefined.
initReconnectState(Options) ->
    IsReconnect = ?GET_FROM_LIST(reconnect, ?KEY_FIND(Options), ?DEFAULT_IS_RECONNECT),
    case IsReconnect of
        true ->
            Max = ?GET_FROM_LIST(reconnectTimeMax, ?KEY_FIND(Options), ?DEFAULT_RECONNECT_MAX),
            Min = ?GET_FROM_LIST(reconnectTimeMin, ?KEY_FIND(Options), ?DEFAULT_RECONNECT_MIN),
            #reconnectState{min = Min, max = Max};
        false ->
            undefined
    end.

-spec resetReconnectState(undefined | reconnect_state()) -> reconnect_state() | undefined.
resetReconnectState(ReconnectState) ->
    ReconnectState#reconnectState{current = undefined}.

-spec reply(server_name(), term(), undefined | cast()) -> ok.
reply(Name, _Reply, #request{pid = undefined}) ->
    shackle_backlog:decrement(Name),
    ok;
reply(Name, Reply, #request{pid = Pid} = Request) ->
    shackle_backlog:decrement(Name),
    Pid ! {Request, Reply},
    ok.

-spec reply_all(server_name(), term()) -> ok.
reply_all(Name, Reply) ->
    reply_all(Name, Reply, shackle_queue:clear(Name)).


reply_all([{Cast, TimerRef} | T], Name, Reply) ->
    cancelTimer(TimerRef),
    reply(Name, Reply, Cast),
    reply_all(Name, Reply, T);
reply_all([], _Name, _Reply) ->
    ok.
