-module(agAgencyUtils).

-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    cancelTimer/1,
    agencyResponses/2,
    initReconnectState/1,
    resetReconnectState/1,
    agencyReply/3,
    agencyReplyAll/2
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

-spec agencyResponses([response()], serverName()) -> ok.
agencyResponses([], _Name) ->
    ok;
agencyResponses([{ExtRequestId, Reply} | T], Name) ->
    case shackle_queue:remove(Name, ExtRequestId) of
        {ok, Cast, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            agencyReply(Name, Reply, Cast);
        {error, not_found} ->
            ok
    end,
    agencyResponses(T, Name).

-spec initReconnectState(client_options()) -> reconnect_state() | undefined.
initReconnectState(Options) ->
    IsReconnect = ?GET_FROM_LIST(reconnect, Options, ?DEFAULT_IS_RECONNECT),
    case IsReconnect of
        true ->
            Max = ?GET_FROM_LIST(reconnectTimeMax, Options, ?DEFAULT_RECONNECT_MAX),
            Min = ?GET_FROM_LIST(reconnectTimeMin, Options, ?DEFAULT_RECONNECT_MIN),
            #reconnectState{min = Min, max = Max};
        false ->
            undefined
    end.

-spec resetReconnectState(undefined | reconnect_state()) -> reconnect_state() | undefined.
resetReconnectState(ReconnectState) ->
    ReconnectState#reconnectState{current = undefined}.

-spec agencyReply(serverName(), term(), undefined | cast()) -> ok.
agencyReply(Name, _Reply, #request{pid = undefined}) ->
    shackle_backlog:decrement(Name),
    ok;
agencyReply(Name, Reply, #request{pid = Pid} = Request) ->
    shackle_backlog:decrement(Name),
    Pid ! {Request, Reply},
    ok.

-spec agencyReplyAll(serverName(), term()) -> ok.
agencyReplyAll(Name, Reply) ->
    agencyReplyAll(Name, Reply, shackle_queue:clear(Name)).


agencyReplyAll([{Cast, TimerRef} | T], Name, Reply) ->
    cancelTimer(TimerRef),
    agencyReply(Name, Reply, Cast),
    agencyReplyAll(Name, Reply, T);
agencyReplyAll([], _Name, _Reply) ->
    ok.
