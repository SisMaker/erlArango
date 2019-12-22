%% beam cache 模块名
-define(agBeamPool, agBeamPool).
-define(agBeamAgency, agBeamAgency).

%% 默认值定义
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_INIT_OPTS, undefined).
-define(DEFAULT_CONNECT_TIMEOUT, 500).
-define(DEFAULT_IP, "120.77.213.39").
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_POOL_OPTIONS, []).
-define(DEFAULT_IS_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, 120000).
-define(DEFAULT_RECONNECT_MIN, 500).
-define(DEFAULT_SOCKET_OPTS, []).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_BODY, undefined).
-define(DEFAULT_HEADERS, []).
-define(DEFAULT_PID, self()).
-define(DEFAULT_PROTOCOL, tcp).
-define(DEFAULT_PORTO(Protocol), 8529).
%%-define(DEFAULT_PORTO(Protocol), case Protocol of tcp -> 80; _ -> 443 end).

-define(GET_FROM_LIST(Key, List), agMiscUtils:getListValue(Key, List, undefined)).
-define(GET_FROM_LIST(Key, List, Default), agMiscUtils:getListValue(Key, List, Default)).

-define(WARN(Tag, Format, Data), agMiscUtils:warnMsg(Tag, Format, Data)).

-define(miDoNetConnect, miDoNetConnect).

-record(miAgHttpCliRet, {
   requestId :: requestId(),
   reply :: term()
}).

-record(dbUrl, {
   host :: host(),
   path :: path(),
   port :: 0..65535,
   hostname :: hostname(),
   protocol :: httpType(),
   poolName :: atom()               %% 请求该URL用到的poolName
}).

-record(requestRet, {
   state :: body | done,
   body :: undefined | binary(),
   content_length :: undefined | non_neg_integer() | chunked,
   headers :: undefined | [binary()],
   reason :: undefined | binary(),
   status_code :: undefined | 100..505
}).

-record(request, {
   requestId :: requestId(),
   pid :: pid() | undefined,
   timeout :: timeout(),
   timestamp :: erlang:timestamp()
}).

-record(httpParam, {
   headers = [] :: [binary()],
   body = undefined :: undefined | binary(),
   pid = self() :: pid(),
   timeout = 1000 :: non_neg_integer()
}).

-record(poolOpts, {
   poolSize :: poolSize(),
   backlogSize :: backlogSize(),
   poolStrategy :: poolStrategy()
}).

-record(reconnectState, {
   min :: time(),
   max :: time() | infinity,
   current :: time() | undefined
}).

-record(cliState, {
   requestsIn = 0 :: non_neg_integer(),
   requestsOut = 0 :: non_neg_integer(),
   binPatterns :: tuple(),
   buffer = <<>> :: binary(),
   response :: requestRet() | undefined
}).

-type cliState() :: #cliState{}.
-type requestRet() :: #requestRet{}.
-type dbUrl() :: #dbUrl {}.
-type error() :: {error, term()}.
-type headers() :: [{iodata(), iodata()}, ...].
-type host() :: binary().
-type hostname() :: binary().
-type path() :: binary().
-type method() :: binary().
-type httpType() :: http | https.
-type body() :: iodata() | undefined.
-type options() :: [option(), ...].
-type option() ::
   {backlogSize, pos_integer()} |
   {poolSize, pos_integer()} |
   {poolStrategy, random | round_robin} |
   {reconnect, boolean()} |
   {reconnectTimeMin, pos_integer()} |
   {reconnectTimeMax, pos_integer() | infinity}.

-type httpParam() :: #httpParam{}.

-type backlogSize() :: pos_integer() | infinity.
-type request() :: #request{}.
-type clientOpt() ::
   {ip, inet:ip_address() | inet:hostname()} |
   {port, inet:port_number()} |
   {protocol, protocol()} |
   {reconnect, boolean()} |
   {reconnectTimeMin, time()} |
   {reconnectTimeMax, time() | infinity} |
   {socketOpts, [gen_tcp:connect_option(), ...]}.

-type clientOpts() :: [clientOpt(), ...].
-type clientState() :: term().
-type externalRequestId() :: term().
-type poolName() :: atom().
-type poolOpt() ::
   {poolSize, poolSize()} |
   {backlogSize, backlogSize()} |
   {poolstrategy, poolStrategy()}.

-type poolOpts() :: [poolOpt()].
-type poolOptsRec() :: #poolOpts{}.
-type poolSize() :: pos_integer().
-type poolStrategy() :: random | round_robin.
-type protocol() :: ssl | tcp.
-type reconnectState() :: #reconnectState{}.
-type requestId() :: {serverName(), reference()}.
-type response() :: {externalRequestId(), term()}.
-type serverName() :: atom().
-type socket() :: inet:socket() | ssl:sslsocket().
-type socketType() :: inet | ssl.
-type time() :: pos_integer().
