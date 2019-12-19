-module(agHttpCli_sup).
-include("buoy_internal.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

init([]) ->
    buoy_pool:init(),

    {ok, {{one_for_one, 5, 10}, []}}.

-behaviour(supervisor).
-export([
    init/1
]).

%% internal
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, shackle_sup}, shackle_sup, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

init([]) ->
    shackle_backlog:init(),
    agAgencyPoolMgr:init(),
    shackle_queue:init(),

    {ok, {{one_for_one, 5, 10}, []}}.
