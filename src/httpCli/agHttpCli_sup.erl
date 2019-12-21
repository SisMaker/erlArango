-module(agHttpCli_sup).

-behaviour(supervisor).

-export([
   start_link/0
   , init/1
]).

-spec start_link() -> {ok, pid()}.
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
init([]) ->
   PoolMgrSpec = {agAgencyPoolMgr, {agAgencyPoolMgr, start_link, [agAgencyPoolMgr, [], []]}, permanent, 5000, worker, [agAgencyPoolMgr]},
   {ok, {{one_for_one, 100, 3600}, [PoolMgrSpec]}}.
