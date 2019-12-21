%%%-------------------------------------------------------------------
%% @doc erlArango top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlArango_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
   SupFlags = #{strategy => one_for_one, intensity => 100, period => 3600},
   HttpCliSupSpec = {agHttpCli_sup, {agHttpCli_sup, start_link, []}, permanent, 5000, supervisor, [agHttpCli_sup]},
   {ok, {SupFlags, [HttpCliSupSpec]}}.

