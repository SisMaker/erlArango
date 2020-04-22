-module(erlArango_sup).
-include("agHttpCli.hrl").
-include("erlArango.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
   SupFlags = #{strategy => one_for_one, intensity => 100, period => 3600},
   PoolMgrSpec = #{id => agAgencyPoolMgrExm, start => {agAgencyPoolMgrExm, start_link, [?agAgencyPoolMgr, [], []]}, restart => permanent, shutdown => infinity, type => worker, modules => [agAgencyPoolMgrExm]},
   HttpCliSupSpec = #{id => agAgencyPool_sup, start => {agAgencyPool_sup, start_link, []}, restart => permanent, shutdown => infinity, type => supervisor, modules => [agAgencyPool_sup]},
   {ok, {SupFlags, [PoolMgrSpec, HttpCliSupSpec]}}.

