-module(agHttpCli).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   callAgency/5
   , callAgency/6
   , castAgency/5
   , castAgency/6
   , castAgency/7
   , receiveResponse/1

   , startPool/2
   , startPool/3
   , stopPool/1
   , start/0

]).

-spec callAgency(poolName(), method(), path(), headers(), body()) -> term() | {error, term()}.
callAgency(PoolName, Method, Path, Headers, Body) ->
   callAgency(PoolName, Method, Path, Headers, Body, ?DEFAULT_TIMEOUT).

-spec callAgency(poolName(), method(), path(), headers(), body(), timeout()) -> term() | {error, atom()}.
callAgency(PoolName, Method, Path, Headers, Body, Timeout) ->
   case castAgency(PoolName, Method, Path, Headers, Body, Timeout, self()) of
      {ok, RequestId} ->
         receiveResponse(RequestId);
      {error, Reason} ->
         {error, Reason}
   end.

-spec castAgency(poolName(), method(), path(), headers(), body()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolName, Method, Path, Headers, Body) ->
   castAgency(PoolName, Method, Path, Headers, Body, ?DEFAULT_TIMEOUT, self()).

-spec castAgency(poolName(), method(), path(), headers(), body(), timeout()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolName, Method, Path, Headers, Body, Timeout) ->
   castAgency(PoolName, Method, Path, Headers, Body, Timeout, self()).

-spec castAgency(poolName(), method(), path(), headers(), body(), timeout(), pid()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolName, Method, Path, Headers, Body, Timeout, Pid) ->
   case agAgencyPoolMgrIns:getOneAgency(PoolName) of
      {error, pool_not_found} = Error ->
         Error;
      undefined ->
         {error, undefined_server};
      AgencyName ->
         RequestId = {AgencyName, make_ref()},
         OverTime = case Timeout == infinity of true -> infinity; _ -> erlang:system_time(millisecond) + Timeout end,
         catch AgencyName ! {miRequest, Pid, Method, Path, Headers, Body, RequestId, OverTime},
         {ok, RequestId}
   end.

-spec receiveResponse(requestId()) -> term() | {error, term()}.
receiveResponse(RequestId) ->
   receive
      #miAgHttpCliRet{requestId = RequestId, reply = Reply} ->
         Reply
   end.

-spec startPool(poolName(), poolCfgs()) -> ok | {error, pool_name_used}.
startPool(PoolName, PoolCfgs) ->
   agAgencyPoolMgrIns:startPool(PoolName, PoolCfgs, []).

-spec startPool(poolName(), poolCfgs(), agencyOpts()) -> ok | {error, pool_name_used}.
startPool(PoolName, PoolCfgs, AgencyOpts) ->
   agAgencyPoolMgrIns:startPool(PoolName, PoolCfgs, AgencyOpts).

-spec stopPool(poolName()) -> ok | {error, pool_not_started}.
stopPool(PoolName) ->
   agAgencyPoolMgrIns:stopPool(PoolName).

start() ->
   application:start(erlArango),
   agHttpCli:startPool(tp, []).
