-module(agHttpCli).
-include("agHttpCli.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    async_custom/3,
    async_get/2,
    async_post/2,
    async_put/2,
    async_request/3,
    custom/3,
    get/2,
    post/2,
    put/2,
    receive_response/1,
    request/3
]).

%% public
-spec async_custom(binary(), buoy_url(), buoy_opts()) ->
    {ok, shackle:request_id()} | error().

async_custom(Verb, Url, BuoyOpts) ->
    async_request({custom, Verb}, Url, BuoyOpts).

-spec async_get(buoy_url(), buoy_opts()) ->
    {ok, shackle:request_id()} | error().

async_get(Url, BuoyOpts) ->
    async_request(get, Url, BuoyOpts).

-spec async_post(buoy_url(), buoy_opts()) ->
    {ok, shackle:request_id()} | error().

async_post(Url, BuoyOpts) ->
    async_request(post, Url, BuoyOpts).

-spec async_put(buoy_url(), buoy_opts()) ->
    {ok, shackle:request_id()} | error().

async_put(Url, BuoyOpts) ->
    async_request(put, Url, BuoyOpts).

-spec async_request(method(), buoy_url(), buoy_opts()) ->
    {ok, shackle:request_id()} | error().

async_request(Method, #dbUrl{
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
            Pid = buoy_opts(pid, BuoyOpts),
            Timeout = buoy_opts(timeout, BuoyOpts),
            shackle:cast(PoolName, Request, Pid, Timeout);
        {error, _} = E ->
            E
    end.

-spec custom(binary(), buoy_url(), buoy_opts()) ->
    {ok, buoy_resp()} | error().

custom(Verb, Url, BuoyOpts) ->
    request({custom, Verb}, Url, BuoyOpts).

-spec get(buoy_url(), buoy_opts()) ->
    {ok, buoy_resp()} | error().

get(Url, BuoyOpts) ->
    request(get, Url, BuoyOpts).

-spec post(buoy_url(), buoy_opts()) ->
    {ok, buoy_resp()} | error().

post(Url, BuoyOpts) ->
    request(post, Url, BuoyOpts).

-spec put(buoy_url(), buoy_opts()) ->
    {ok, buoy_resp()} | error().

put(Url, BuoyOpts) ->
    request(put, Url, BuoyOpts).

-spec receive_response(request_id()) ->
    {ok, term()} | error().

receive_response(RequestId) ->
    shackle:receive_response(RequestId).

-spec request(method(), buoy_url(), buoy_opts()) ->
    {ok, buoy_resp()} | error().

request(Method, #dbUrl{
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

%% private
buoy_opts(body, BuoyOpts) ->
    maps:get(body, BuoyOpts, ?DEFAULT_BODY);
buoy_opts(headers, BuoyOpts) ->
    maps:get(headers, BuoyOpts, ?DEFAULT_HEADERS);
buoy_opts(pid, BuoyOpts) ->
    maps:get(pid, BuoyOpts, ?DEFAULT_PID);
buoy_opts(timeout, BuoyOpts) ->
    maps:get(timeout, BuoyOpts, ?DEFAULT_TIMEOUT).


%% public
-export([
    call/2,
    call/3,
    cast/2,
    cast/3,
    cast/4,
    receive_response/1
]).

%% public
-spec call(pool_name(), term()) ->
    term() | {error, term()}.

call(PoolName, Request) ->
    call(PoolName, Request, ?DEFAULT_TIMEOUT).

-spec call(atom(), term(), timeout()) ->
    term() | {error, atom()}.

call(PoolName, Request, Timeout) ->
    case cast(PoolName, Request, self(), Timeout) of
        {ok, RequestId} ->
            receive_response(RequestId);
        {error, Reason} ->
            {error, Reason}
    end.

-spec cast(pool_name(), term()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request) ->
    cast(PoolName, Request, self()).

-spec cast(pool_name(), term(), pid()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid) ->
    cast(PoolName, Request, Pid, ?DEFAULT_TIMEOUT).

-spec cast(pool_name(), term(), pid(), timeout()) ->
    {ok, request_id()} | {error, atom()}.

cast(PoolName, Request, Pid, Timeout) ->
    Timestamp = os:timestamp(),
    case agAgencyPoolMgr:server(PoolName) of
        {ok, Client, Server} ->
            RequestId = {Server, make_ref()},
            Server ! {Request, #cast{
                client = Client,
                pid = Pid,
                request_id = RequestId,
                timeout = Timeout,
                timestamp = Timestamp
            }},
            {ok, RequestId};
        {error, Reason} ->
            {error, Reason}
    end.

-spec receive_response(request_id()) ->
    term() | {error, term()}.

receive_response(RequestId) ->
    receive
        {#cast{request_id = RequestId}, Reply} ->
            Reply
    end.

