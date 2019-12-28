%% beam cache 模块名
-define(agBeamPool, agBeamPool).
-define(agBeamAgency, agBeamAgency).

%% 默认值定义
-define(DEFAULT_BASE_URL, <<"http://127.0.0.1:8529">>).
-define(USER_PASSWORD, <<"root:156736">>).
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_INIT_OPTS, undefined).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_POOL_OPTIONS, []).
-define(DEFAULT_IS_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, 120000).
-define(DEFAULT_RECONNECT_MIN, 500).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_BODY, undefined).
-define(DEFAULT_HEADERS, []).
-define(DEFAULT_PID, self()).
-define(DEFAULT_PROTOCOL, tcp).
-define(DEFAULT_PORTO(Protocol), 8529).
%%-define(DEFAULT_PORTO(Protocol), case Protocol of tcp -> 80; _ -> 443 end).
-define(DEFAULT_SOCKET_OPTS, [binary, {active, true}, {delay_send, true}, {nodelay, true}, {keepalive, true}, {recbuf, 1048576}, {send_timeout, 5000}, {send_timeout_close, true}]).

-define(GET_FROM_LIST(Key, List), agMiscUtils:getListValue(Key, List, undefined)).
-define(GET_FROM_LIST(Key, List, Default), agMiscUtils:getListValue(Key, List, Default)).

-define(WARN(Tag, Format, Data), agMiscUtils:warnMsg(Tag, Format, Data)).

-define(miDoNetConnect, miDoNetConnect).

-record(miAgHttpCliRet, {
   requestId :: requestId(),
   reply :: term()
}).

-record(request, {
   requestId :: requestId(),
   pid :: pid() | undefined,
   timeout :: timeout(),
   timestamp :: erlang:timestamp()
}).

-record(requestRet1, {
   state :: body | done,
   body :: undefined | binary(),
   contentLength :: undefined | non_neg_integer() | chunked,
   headers :: undefined | [binary()],
   reason :: undefined | binary(),
   statusCode :: undefined | 100..505
}).

-record(recvState, {
   statusCode :: undefined | 100..505,
   reason :: undefined | binary(),
   headers :: undefined | [binary()],
   contentLength :: undefined | non_neg_integer() | chunked,
   stage = header  :: header | body | done,                    %% 一个请求收到tcp可能会有多个包 最多分三个阶接收
   body :: undefined | binary()
}).

-record(httpParam, {
   headers = [] :: [binary()],
   body = undefined :: undefined | binary(),
   pid = self() :: pid(),
   timeout = 1000 :: non_neg_integer()
}).

-record(reconnectState, {
   min :: non_neg_integer(),
   max :: non_neg_integer() | infinity,
   current :: non_neg_integer() | undefined
}).

-record(cliState, {
   binPatterns :: tuple(),
   requestsIn = 1 :: non_neg_integer(),
   requestsOut = 0 :: non_neg_integer(),
   status = leisure :: waiting | leisure,
   backlogNum = 0 :: integer(),
   backlogSize = 0 :: integer(),
   curInfo = undefined :: tuple(),
   recvState :: recvState() | undefined

}).

-record(poolOpts, {
   host :: host(),
   port :: 0..65535,
   hostname :: string(),
   protocol :: protocol(),
   poolSize ::binary(),
   userPassword :: binary()
}).

-record(binPatterns, {
   rn :: binary:cp(),
   rnrn :: binary:cp()
}).

-type binPatterns() :: #binPatterns {}.
-type miAgHttpCliRet() :: #miAgHttpCliRet{}.
-type request() :: #request{}.
-type recvState() :: #recvState{}.
-type httpParam() :: #httpParam{}.
-type cliState() :: #cliState{}.
-type reconnectState() :: #reconnectState{}.

-type poolName() :: atom().
-type serverName() :: atom().
-type protocol() :: ssl | tcp.
-type method() :: binary().
-type headers() :: [{iodata(), iodata()}].
-type body() :: iodata() | undefined.
-type path() :: binary().
-type host() :: binary().
-type poolSize() :: pos_integer().
-type backlogSize() :: pos_integer() | infinity.
-type requestId() :: {serverName(), reference()}.
-type externalRequestId() :: term().
-type response() :: {externalRequestId(), term()}.
-type socket() :: inet:socket() | ssl:sslsocket().
-type error() :: {error, term()}.

-type poolCfg() ::
   {baseUrl, binary()} |
   {user, binary()} |
   {password, binary()} |
   {poolSize, poolSize()}.

-type agencyOpt() ::
   {reconnect, boolean()} |
   {backlogSize, backlogSize()} |
   {reconnectTimeMin, pos_integer()} |
   {reconnectTimeMax, pos_integer()} |
   {socketOpts, [gen_tcp:connect_option(), ...]}.

-type poolCfgs() :: [poolCfg()].
-type poolOpts() :: #poolOpts{}.
-type agencyOpts() :: [agencyOpt()].

-record(dbUrl, {
   host :: host(),
   path :: path(),
   port :: 0..65535,
   hostname :: string(),
   protocol :: protocol(),
   poolName :: atom()               %% 请求该URL用到的poolName
}).

-type dbUrl() :: #dbUrl{}.

