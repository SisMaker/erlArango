-module(agAgencyPoolMgrIns).

-include("agHttpCli.hrl").
-include("erlArango.hrl").

-export([
   startPool/2
   , startPool/3
   , stopPool/1
   , getOneAgency/1

   , init/1
   , handleMsg/2
   , terminate/2
]).

%% k-v缓存表
-define(ETS_AG_Pool, ets_ag_Pool).
-define(ETS_AG_Agency, ets_ag_Agency).
-record(state, {}).

-spec init(Args :: term()) -> ok.
init(_Args) ->
   ets:new(?ETS_AG_Pool, [named_table, set, protected]),
   ets:new(?ETS_AG_Agency, [named_table, set, protected]),
   agKvsToBeam:load(?agBeamPool, []),
   agKvsToBeam:load(?agBeamAgency, []),
   {ok, #state{}}.

handleMsg({'$gen_call', From, {startPool, Name, ClientOpts, PoolOpts}}, State) ->
   dealStart(Name, ClientOpts, PoolOpts),
   gen_server:reply(From, ok),
   {ok, State};
handleMsg({'$gen_call', From, {stopPool, Name}}, State) ->
   delaStop(Name),
   gen_server:reply(From, ok),
   {ok, State};
handleMsg(_Msg, State) ->
   ?WARN(?MODULE, "receive unexpected  msg: ~p", [_Msg]),
   {ok, State}.

terminate(_Reason, _State) ->
   ok.

-spec startPool(poolName(), clientOpts()) -> ok | {error, pool_name_used}.
startPool(PoolName, ClientOpts) ->
   startPool(PoolName, ClientOpts, []).

-spec startPool(poolName(), clientOpts(), poolOpts()) -> ok | {error, pool_name_used}.
startPool(PoolName, ClientOpts, PoolOpts) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         gen_server:call(?agAgencyPoolMgr, {startPool, PoolName, ClientOpts, PoolOpts});
      _ ->
         {error, pool_name_used}
   end.

-spec stopPool(poolName()) -> ok | {error, pool_not_started}.
stopPool(PoolName) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         {error, pool_not_started};
      _ ->
         gen_server:call(?agAgencyPoolMgr, {stopPool, PoolName})

   end.

dealStart(PoolName, ClientOpts, PoolOpts) ->
   #poolOpts{poolSize = PoolSize} = PoolOptsRec = poolOptsToRec(PoolOpts),
   startChildren(PoolName, ClientOpts, PoolOptsRec),
   cacheAddPool(PoolName, PoolSize),
   cacheAddAgency(PoolName, PoolSize),
   case persistent_term:get(PoolName, undefined) of
      undefined ->
         IndexRef = atomics:new(1, [{signed, false}]),
         persistent_term:put(PoolName, IndexRef);
      _ ->
         ignore
   end,
   ok.

delaStop(PoolName) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         {error, pool_not_started};
      PoolSize ->
         stopChildren(agencyNames(PoolName, PoolSize)),
         cacheDelPool(PoolName),
         cacheDelAgency(PoolName),
         ok
   end.

poolOptsToRec(Options) ->
   PoolSize = ?GET_FROM_LIST(poolSize, Options, ?DEFAULT_POOL_SIZE),
   BacklogSize = ?GET_FROM_LIST(backlogSize, Options, ?DEFAULT_BACKLOG_SIZE),
   PoolStrategy = ?GET_FROM_LIST(poolStrategy, Options, ?DEFAULT_POOL_STRATEGY),
   #poolOpts{poolSize = PoolSize, backlogSize = BacklogSize, poolStrategy = PoolStrategy}.

agencyName(PoolName, Index) ->
   list_to_atom(atom_to_list(PoolName) ++ "_" ++ integer_to_list(Index)).

agencyNames(PoolName, PoolSize) ->
   [agencyName(PoolName, N) || N <- lists:seq(1, PoolSize)].

agencyMod(tcp) ->
   agTcpAgencyExm;
agencyMod(ssl) ->
   agSslAgencyExm;
agencyMod(_) ->
   agTcpAgencyExm.

agencySpec(ServerMod, ServerName, ClientOptions) ->
   StartFunc = {ServerMod, start_link, [ServerName, ClientOptions, []]},
   {ServerName, StartFunc, transient, 5000, worker, [ServerMod]}.

-spec startChildren(atom(), clientOpts(), poolOpts()) -> ok.
startChildren(PoolName, ClientOpts, #poolOpts{poolSize = PoolSize}) ->
   Protocol = ?GET_FROM_LIST(protocol, ClientOpts, ?DEFAULT_PROTOCOL),
   AgencyMod = agencyMod(Protocol),
   AgencyNames = agencyNames(PoolName, PoolSize),
   AgencySpecs = [agencySpec(AgencyMod, AgencyName, ClientOpts) || AgencyName <- AgencyNames],
   [supervisor:start_child(agHttpCli_sup, AgencySpec) || AgencySpec <- AgencySpecs],
   ok.

stopChildren([AgencyName | T]) ->
   supervisor:terminate_child(agHttpCli_sup, AgencyName),
   supervisor:delete_child(agHttpCli_sup, AgencyName),
   stopChildren(T);
stopChildren([]) ->
   ok.

cacheAddPool(Key, Value) ->
   ets:insert(?ETS_AG_Pool, {Key, Value}),
   KVS = ets:tab2list(?ETS_AG_Pool),
   agKvsToBeam:load(?agBeamPool, KVS),
   ok.

cacheAddAgency(PoolName, PoolSize) ->
   NameList = [{{PoolName, N}, agencyName(PoolName, N)} || N <- lists:seq(1, PoolSize)],
   ets:insert(?ETS_AG_Agency, NameList),
   KVS = ets:tab2list(?ETS_AG_Agency),
   agKvsToBeam:load(?agBeamAgency, KVS),
   ok.

cacheDelPool(Key) ->
   ets:delete(?ETS_AG_Pool, Key),
   KVS = ets:tab2list(?ETS_AG_Pool),
   agKvsToBeam:load(?agBeamPool, KVS),
   ok.

cacheDelAgency(PoolName) ->
   ets:match_delete(?ETS_AG_Agency, {{PoolName, '_'}, '_'}),
   KVS = ets:tab2list(?ETS_AG_Agency),
   agKvsToBeam:load(?agBeamAgency, KVS),
   ok.

getOneAgency(PoolName) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         {error, pool_not_found};
      PoolSize ->
         Ref = persistent_term:get(PoolName),
         AgencyIdx = atomics:add_get(Ref, 1, 1),
         case AgencyIdx >= PoolSize of
            true ->
               atomics:put(Ref, 1, 0),
               ?agBeamAgency:get({PoolName, PoolSize});
            _ ->
               ?agBeamAgency:get({PoolName, AgencyIdx})
         end
   end.
