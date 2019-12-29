-module(agAgencyPoolMgrIns).

-compile(inline).
-compile({inline_size, 128}).

-include("agHttpCli.hrl").
-include("erlArango.hrl").

-export([
   startPool/2
   , startPool/3
   , stopPool/1
   , getOneAgency/1

   %% genExm API
   , init/1
   , handleMsg/2
   , terminate/2
]).

%% k-v缓存表
-define(ETS_AG_Pool, ets_ag_Pool).
-define(ETS_AG_Agency, ets_ag_Agency).

-spec init(Args :: term()) -> ok.
init(_Args) ->
   ets:new(?ETS_AG_Pool, [named_table, set, protected]),
   ets:new(?ETS_AG_Agency, [named_table, set, protected]),
   agKvsToBeam:load(?agBeamPool, []),
   agKvsToBeam:load(?agBeamAgency, []),
   {ok, undefined}.

handleMsg({'$gen_call', From, {startPool, PoolName, PoolCfgs, AgencyOpts}}, State) ->
   dealStart(PoolName, PoolCfgs, AgencyOpts),
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

-spec startPool(poolName(), poolCfgs()) -> ok | {error, pool_name_used}.
startPool(PoolName, PoolCfgs) ->
   startPool(PoolName, PoolCfgs, []).

-spec startPool(poolName(), poolCfgs(), agencyOpts()) -> ok | {error, pool_name_used}.
startPool(PoolName, PoolCfgs, AgencyOpts) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         gen_server:call(?agAgencyPoolMgr, {startPool, PoolName, PoolCfgs, AgencyOpts});
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

dealStart(PoolName, PoolCfgs, AgencyOpts) ->
   #poolOpts{poolSize = PoolSize, protocol = Protocol} = PoolOpts = poolOpts(PoolCfgs),
   cacheAddPool(PoolName, PoolOpts),

   startChildren(PoolName, Protocol, PoolSize, AgencyOpts),
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
      #poolOpts{poolSize = PoolSize} ->
         stopChildren(agencyNames(PoolName, PoolSize)),
         cacheDelPool(PoolName),
         cacheDelAgency(PoolName),
         ok
   end.

poolOpts(Options) ->
   BaseUrl = ?GET_FROM_LIST(baseUrl, Options, ?DEFAULT_BASE_URL),
   UserPassword = ?GET_FROM_LIST(user, Options, ?USER_PASSWORD),
   PoolSize = ?GET_FROM_LIST(poolSize, Options, ?DEFAULT_POOL_SIZE),
   PoolOpts = agMiscUtils:parseUrl(BaseUrl),
   UserPasswordBase64 = <<"Basic ", (base64:encode(UserPassword))/binary>>,
   PoolOpts#poolOpts{userPassword = UserPasswordBase64, poolSize = PoolSize}.


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

agencySpec(ServerMod, ServerName, Args) ->
   %% TODO 下面spawn_opt 参数需要调优
   StartFunc = {ServerMod, start_link, [ServerName, Args, [{min_heap_size, 5000}, {min_bin_vheap_size, 100000}, {fullsweep_after, 500}]]},
   {ServerName, StartFunc, transient, 5000, worker, [ServerMod]}.

-spec startChildren(atom(), protocol(), poolSize(), agencyOpts()) -> ok.
startChildren(PoolName, Protocol, PoolSize, AgencyOpts) ->
   AgencyMod = agencyMod(Protocol),
   AgencyNames = agencyNames(PoolName, PoolSize),
   AgencySpecs = [agencySpec(AgencyMod, AgencyName, {PoolName, AgencyName, AgencyOpts}) || AgencyName <- AgencyNames],
   [supervisor:start_child(agAgencyPool_sup, AgencySpec) || AgencySpec <- AgencySpecs],
   ok.

stopChildren([AgencyName | T]) ->
   supervisor:terminate_child(agAgencyPool_sup, AgencyName),
   supervisor:delete_child(agAgencyPool_sup, AgencyName),
   stopChildren(T);
stopChildren([]) ->
   ok.

cacheAddPool(Key, Value) ->
   ets:insert(?ETS_AG_Pool, {Key, Value}),
   KVS = ets:tab2list(?ETS_AG_Pool),
   agKvsToBeam:load(?agBeamPool, KVS),
   ok.

cacheDelPool(Key) ->
   ets:delete(?ETS_AG_Pool, Key),
   KVS = ets:tab2list(?ETS_AG_Pool),
   agKvsToBeam:load(?agBeamPool, KVS),
   ok.

cacheAddAgency(PoolName, PoolSize) ->
   NameList = [{{PoolName, N}, agencyName(PoolName, N)} || N <- lists:seq(1, PoolSize)],
   ets:insert(?ETS_AG_Agency, NameList),
   KVS = ets:tab2list(?ETS_AG_Agency),
   agKvsToBeam:load(?agBeamAgency, KVS),
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
      #poolOpts{poolSize = PoolSize} ->
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
