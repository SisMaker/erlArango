-module(agAgencyUtils).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   getQueue/1
   , addQueue/2
   , delQueue/1
   , clearQueue/0
   , cancelTimer/1
   , agencyReply/2
   , agencyReply/4
   , agencyReplyAll/1
   , initReconnectState/1
   , resetReconnectState/1
   , updateReconnectState/1
]).

getQueue(RequestsIn) ->
   erlang:get(RequestsIn).

addQueue(RequestsIn, MiRequest) ->
   erlang:put(RequestsIn, MiRequest).

delQueue(RequestsIn) ->
   erlang:erase(RequestsIn).

clearQueue() ->
   erlang:erase().

-spec agencyReply(term(), term()) -> ok.
agencyReply({undefined, _RequestId, TimerRef}, _Reply) ->
   agAgencyUtils:cancelTimer(TimerRef);
agencyReply({PidForm, RequestId, TimerRef}, Reply) ->
   agAgencyUtils:cancelTimer(TimerRef),
   catch PidForm ! #miAgHttpCliRet{requestId = RequestId, reply = Reply},
   ok;
agencyReply(undefined, RequestRet) ->
   ?WARN(not_curInfo, "not find curInfo ret is:~p~n ", [RequestRet]),
   ok.

-spec agencyReply(undefined | pid(), requestId(), undefined | reference(), term()) -> ok.
agencyReply(undefined, _RequestId, TimerRef, _Reply) ->
   agAgencyUtils:cancelTimer(TimerRef),
   ok;
agencyReply(FormPid, RequestId, TimerRef, Reply) ->
   agAgencyUtils:cancelTimer(TimerRef),
   catch FormPid ! #miAgHttpCliRet{requestId = RequestId, reply = Reply},
   ok.

-spec agencyReplyAll(term()) -> ok.
agencyReplyAll(Reply) ->
   AllList = agAgencyUtils:clearQueue(),
   [agencyReply(FormPid, RequestId, undefined, Reply) || {miRequest, FormPid, _Method, _Path, _Headers, _Body, RequestId, _Timeout} <- AllList],
   ok.

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
         after 0 ->
            ok
         end;
      _ ->
         %% Timer 已经运行了
         ok
   end.

-spec initReconnectState(agencyOpts()) -> reconnectState() | undefined.
initReconnectState(Options) ->
   IsReconnect = ?GET_FROM_LIST(reconnect, Options, ?DEFAULT_IS_RECONNECT),
   case IsReconnect of
      true ->
         Max = ?GET_FROM_LIST(reconnectTimeMax, Options, ?DEFAULT_RECONNECT_MAX),
         Min = ?GET_FROM_LIST(reconnectTimeMin, Options, ?DEFAULT_RECONNECT_MIN),
         #reconnectState{min = Min, max = Max, current = Min};
      false ->
         undefined
   end.

-spec resetReconnectState(undefined | reconnectState()) -> reconnectState() | undefined.
resetReconnectState(#reconnectState{min = Min} = ReconnectState) ->
   ReconnectState#reconnectState{current = Min}.

-spec updateReconnectState(reconnectState()) -> reconnectState().
updateReconnectState(#reconnectState{current = Current, max = Max} = ReconnectState) ->
   NewCurrent = Current + Current,
   ReconnectState#reconnectState{current = minCur(NewCurrent, Max)}.

minCur(A, B) when B >= A ->
   A;
minCur(_, B) ->
   B.

