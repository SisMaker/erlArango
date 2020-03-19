-module(erlArango_sup).

-include("erlArango.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
   SupFlags = #{strategy => one_for_one, intensity => 100, period => 3600},
   PoolMgrSpec = {agAgencyPoolMgrExm, {agAgencyPoolMgrExm, start_link, [?agAgencyPoolMgr, [], []]}, permanent, infinity, worker, [agAgencyPoolMgrExm]},
   HttpCliSupSpec = {agAgencyPool_sup, {agAgencyPool_sup, start_link, []}, permanent, infinity, supervisor, [agAgencyPool_sup]},
   {ok, {SupFlags, [PoolMgrSpec, HttpCliSupSpec]}}.

