%% beam cache 模块名
-define(agBeamPool, agBeamPool).
-define(agBeamAgency, agBeamAgency).

%% 默认值定义
-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_INIT_OPTS, undefined).
-define(DEFAULT_CONNECT_TIMEOUT, 500).
-define(DEFAULT_PORT, 80).
-define(DEFAULT_IP, <<"127.0.0.1">>).
-define(DEFAULT_POOL_SIZE, 16).
-define(DEFAULT_POOL_STRATEGY, random).
-define(DEFAULT_POOL_OPTIONS, []).
-define(DEFAULT_IS_RECONNECT, true).
-define(DEFAULT_RECONNECT_MAX, 120000).
-define(DEFAULT_RECONNECT_MIN, 500).
-define(DEFAULT_SOCKET_OPTS, []).
-define(DEFAULT_TIMEOUT, 1000).
-define(DEFAULT_BODY, undefined).
-define(DEFAULT_HEADERS, []).
-define(DEFAULT_PID, self()).
-define(DEFAULT_PROTOCOL, tcp).

-define(GET_FROM_LIST(Key, List), case lists:keyfind(Key, 1, List) of false -> undefined; {_, Value} -> Value end).
-define(GET_FROM_LIST(Key, List, Default), case lists:keyfind(Key, 1, List) of false -> Default; {_, Value} -> Value end).

-define(WARN(PoolName, Format, Data), agMiscUtils:warnMsg(PoolName, Format, Data)).

-record(dbUrl, {
   host     :: host(),
   path     :: path(),
   port     :: 0..65535,
   hostname :: hostname(),
   protocol :: httpType()
}).

-record(requestRet, {
   state          :: body | done,
   body           :: undefined | binary(),
   content_length :: undefined | non_neg_integer() | chunked,
   headers        :: undefined | [binary()],
   reason         :: undefined | binary(),
   status_code    :: undefined | 100..505
}).

-record(request, {
   requestId      :: requestId(),
   pid            :: pid() | undefined,
   timeout        :: timeout(),
   timestamp      :: erlang:timestamp()
}).

-record(poolOpts, {
   poolSize     :: poolSize(),
   backlogSize  :: backlogSize(),
   poolStrategy :: poolStrategy()
}).

-record(reconnectState, {
   min     :: time(),
   max     :: time() | infinity,
   current :: time() | undefined
}).

-type requestRet()     :: #requestRet {}.
-type dbUrl()         :: #dbUrl {}.
-type error()         :: {error, term()}.
-type headers()       :: [{iodata(), iodata()}, ...].
-type host()          :: binary().
-type hostname()      :: binary().
-type path()          :: binary().
-type method()        :: binary().
-type httpType() :: http | https.
-type body()          :: iodata() | undefined.
-type options()       :: [option(), ...].
-type option()        ::
   {backlogSize, pos_integer()} |
   {poolSize, pos_integer()} |
   {poolStrategy, random | round_robin} |
   {reconnect, boolean()} |
   {reconnectTimeMin, pos_integer()} |
   {reconnectTimeMax, pos_integer() | infinity}.

-type buoy_opts() ::
   #{
      headers => headers(),
      body    => body(),
      pid     => pid(),
      timeout => non_neg_integer()
   }.

-type backlogSize() :: pos_integer() | infinity.
-type request() :: #request{}.
-type clientOpt() ::
   {initOpts, term()} |
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
   {poolSize, pool_size()} |
   {backlogSize, backlog_size()} |
   {poolstrategy, poolStrategy()}.

-type poolOpts() :: [poolOpt()].
-type poolOptsRec() :: #poolOpts{}.
-type poolSize() :: pos_integer().
-type poolStrategy() :: random | round_robin.
-type protocol() :: ssl | tcp.
-type reconnectState() :: #reconnectState{}.
-type requestId() :: {server_name(), reference()}.
-type response() :: {external_request_id(), term()}.
-type server_name() :: atom().
-type socket() :: inet:socket() | ssl:sslsocket().
-type socketType() :: inet | ssl.
-type time() :: pos_integer().
