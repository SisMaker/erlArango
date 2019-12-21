-module(agHttpCli).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   syncGet/3,
   syncPost/3,
   syncPut/3,
   syncGet/4,
   syncPost/4,
   syncPut/4,
   syncRequest/5,

   asyncGet/2,
   asyncPost/2,
   asyncPut/2,
   asyncCustom/3,
   asyncRequest/3,

   callAgency/2,
   callAgency/3,
   castAgency/2,
   castAgency/3,
   castAgency/4,
   receiveResponse/1

]).

-spec syncGet(dbUrl(), headers(), body()) -> {ok, requestRet()} | error().
syncGet(Url, Headers, Body) ->
   syncRequest(<<"GET">>, Url, Headers, Body, ?DEFAULT_TIMEOUT).

-spec syncGet(dbUrl(), headers(), body(), timeout()) -> {ok, requestRet()} | error().
syncGet(Url, Headers, Body, Timeout) ->
   syncRequest(<<"GET">>, Url, Headers, Body, Timeout).

-spec syncPost(dbUrl(), headers(), body()) -> {ok, requestRet()} | error().
syncPost(Url, Headers, Body) ->
   syncRequest(<<"POST">>, Url, Headers, Body, ?DEFAULT_TIMEOUT).

-spec syncPost(dbUrl(), headers(), body(), timeout()) -> {ok, requestRet()} | error().
syncPost(Url, Headers, Body, Timeout) ->
   syncRequest(<<"POST">>, Url, Headers, Body, Timeout).

-spec syncPut(dbUrl(), headers(), body()) -> {ok, requestRet()} | error().
syncPut(Url, Headers, Body) ->
   syncRequest(<<"PUT">>, Url, Headers, Body, ?DEFAULT_TIMEOUT).

-spec syncPut(dbUrl(), headers(), body(), timeout()) -> {ok, requestRet()} | error().
syncPut(Url, Headers, Body, Timeout) ->
   syncRequest(<<"PUT">>, Url, Headers, Body, Timeout).

%% -spec syncCustom(binary(), dbUrl(), httpParam()) -> {ok, requestRet()} | error().
%% syncCustom(Verb, Url, Headers, Body) ->
%%    syncRequest({custom, Verb}, Url, Headers, Body).

-spec syncRequest(method(), dbUrl(), headers(), body(), timeout()) -> {ok, requestRet()} | error().
syncRequest(Method, #dbUrl{
   host = Host,
   path = Path,
   poolName = PoolName
}, Headers, Body, Timeout) ->
   Request = {request, Method, Path, Headers, Host, Body},
   callAgency(PoolName, Request, Timeout).

-spec asyncGet(dbUrl(), httpParam()) -> {ok, requestId()} | error().
asyncGet(Url, HttpParam) ->
   asyncRequest(<<"GET">>, Url, HttpParam).

-spec asyncPost(dbUrl(), httpParam()) ->
   {ok, shackle:request_id()} | error().

asyncPost(Url, HttpParam) ->
   asyncRequest(<<"POST">>, Url, HttpParam).

-spec asyncPut(dbUrl(), httpParam()) -> {ok, requestId()} | error().
asyncPut(Url, HttpParam) ->
   asyncRequest(<<"PUT">>, Url, HttpParam).

-spec asyncCustom(binary(), dbUrl(), httpParam()) -> {ok, requestId()} | error().
asyncCustom(Verb, Url, HttpParam) ->
   asyncRequest({custom, Verb}, Url, HttpParam).

-spec asyncRequest(method(), dbUrl(), httpParam()) -> {ok, requestId()} | error().
asyncRequest(Method,
   #dbUrl{host = Host, path = Path, poolName = PoolName},
   #httpParam{headers = Headers, body = Body, pid = Pid, timeout = Timeout}) ->
   RequestContent = {Method, Host, Path, Headers, Body},
   castAgency(PoolName, RequestContent, Pid, Timeout).

-spec callAgency(poolName(), term()) -> term() | {error, term()}.
callAgency(PoolName, Request) ->
   callAgency(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec callAgency(atom(), term(), timeout()) -> term() | {error, atom()}.
callAgency(PoolName, Request, Timeout) ->
   case castAgency(PoolName, Request, self(), Timeout) of
      {ok, RequestId} ->
         receiveResponse(RequestId);
      {error, Reason} ->
         {error, Reason}
   end.

-spec castAgency(poolName(), term()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolName, Request) ->
   castAgency(PoolName, Request, self()).

-spec castAgency(poolName(), term(), pid()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolName, Request, Pid) ->
   castAgency(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-spec castAgency(poolName(), term(), pid(), timeout()) -> {ok, requestId()} | {error, atom()}.
castAgency(PoolName, RequestContent, Pid, Timeout) ->
   case agAgencyPoolMgr:getOneAgency(PoolName) of
      {error, pool_not_found} = Error ->
         Error;
      undefined ->
         {error, undefined_server};
      AgencyName ->
         RequestId = {AgencyName, make_ref()},
         catch AgencyName ! {miRequest, RequestContent, Pid, RequestId, Timeout},
         {ok, RequestId}
   end.

-spec receiveResponse(requestId()) -> term() | {error, term()}.
receiveResponse(RequestId) ->
   receive
      {#miAgHttpCliRet{requestId = RequestId}, Reply} ->
         Reply
   end.

