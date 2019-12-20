-module(agHttpCli).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
   syncCustom/3,
   syncGet/2,
   syncPost/2,
   syncPut/2,
   receiveResponse/1,
   syncRequest/3,
   asyncCustom/3,
   asyncGet/2,
   asyncPost/2,
   asyncPut/2,
   asyncRequest/3,


   callAgency/2,
   callAgency/3,
   castAgency/2,
   castAgency/3,
   castAgency/4,
   receiveResponse/1

]).


-spec asyncCustom(binary(), dbUrl(), httpParam()) -> {ok, shackle:request_id()} | error().
asyncCustom(Verb, Url, HttpParam) ->
   asyncRequest({custom, Verb}, Url, HttpParam).

-spec asyncGet(dbUrl(), httpParam()) -> {ok, shackle:request_id()} | error().
asyncGet(Url, HttpParam) ->
   asyncRequest(<<"GET">>, Url, HttpParam).

-spec asyncPost(dbUrl(), httpParam()) ->
   {ok, shackle:request_id()} | error().

asyncPost(Url, HttpParam) ->
   asyncRequest(<<"POST">>, Url, HttpParam).

-spec asyncPut(dbUrl(), httpParam()) -> {ok, shackle:request_id()} | error().
asyncPut(Url, HttpParam) ->
   asyncRequest(<<"PUT">>, Url, HttpParam).

-spec asyncRequest(method(), dbUrl(), httpParam()) -> {ok, shackle:request_id()} | error().
asyncRequest(Method,
   #dbUrl{host = Host, path = Path, poolName = PoolName},
   #httpParam{headers = Headers, body = Body, pid = Pid, timeout = Timeout}) ->
   Request = {request, Method, Path, Headers, Host, Body},
   castAgency(PoolName, Request, Pid, Timeout).

-spec syncCustom(binary(), dbUrl(), httpParam()) -> {ok, buoy_resp()} | error().
syncCustom(Verb, Url, BuoyOpts) ->
   syncRequest({custom, Verb}, Url, BuoyOpts).

-spec syncGet(dbUrl(), httpParam()) -> {ok, buoy_resp()} | error().
syncGet(Url, BuoyOpts) ->
   syncRequest(get, Url, BuoyOpts).

-spec syncPost(dbUrl(), httpParam()) -> {ok, buoy_resp()} | error().
syncPost(Url, BuoyOpts) ->
   syncRequest(post, Url, BuoyOpts).

-spec syncPut(dbUrl(), httpParam()) -> {ok, buoy_resp()} | error().
syncPut(Url, BuoyOpts) ->
   syncRequest(put, Url, BuoyOpts).

-spec receiveResponse(request_id()) -> {ok, term()} | error().
receiveResponse(RequestId) ->
   shackle:receive_response(RequestId).

-spec syncRequest(method(), dbUrl(), httpParam()) -> {ok, buoy_resp()} | error().
syncRequest(Method, #dbUrl{
   protocol = Protocol,
   host = Host,
   hostname = Hostname,
   port = Port,
   path = Path
}, BuoyOpts) ->
   case buoy_pool:lookup(Protocol, Hostname, Port) of
      {ok, PoolName} ->
         Headers = buoy_opts(headers, BuoyOpts),
         Body = buoy_opts(body, BuoyOpts),
         Request = {request, Method, Path, Headers, Host, Body},
         Timeout = buoy_opts(timeout, BuoyOpts),
         shackle:call(PoolName, Request, Timeout);
      {error, _} = E ->
         E
   end.

-spec callAgency(pool_name(), term()) -> term() | {error, term()}.
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

-spec castAgency(pool_name(), term()) -> {ok, request_id()} | {error, atom()}.
castAgency(PoolName, Request) ->
   castAgency(PoolName, Request, self()).

-spec castAgency(pool_name(), term(), pid()) -> {ok, request_id()} | {error, atom()}.
castAgency(PoolName, Request, Pid) ->
   castAgency(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-spec castAgency(pool_name(), term(), pid(), timeout()) -> {ok, request_id()} | {error, atom()}.
castAgency(PoolName, Request, Pid, Timeout) ->
   Timestamp = os:timestamp(),
   case agAgencyPoolMgr:getOneAgency(PoolName) of
      {error, pool_not_found} = Error ->
         Error;
      undefined ->
         {error, undefined_server};
      AgencyName ->
         RequestId = {AgencyName, make_ref()},
         catch AgencyName ! {Request, #request{pid = Pid, requestId = RequestId, timeout = Timeout, timestamp = Timestamp}},
         {ok, RequestId}
   end.

-spec receiveResponse(request_id()) -> term() | {error, term()}.
receiveResponse(RequestId) ->
   receive
      {#cast{request_id = RequestId}, Reply} ->
         Reply
   end.

