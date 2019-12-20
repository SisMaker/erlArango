-module(agAgencyPoolMgr).

-include("agHttpCli.hrl").

-export([
   startPool/2,
   startPool/3,
   stopPool/1,
   getOneAgency/1,

   %%  内部行为API
   start_link/3,
   init_it/3,
   system_code_change/4,
   system_continue/3,
   system_get_state/1,
   system_terminate/4,
   init/1,
   handleMsg/2,
   terminate/2
]).

%% k-v缓存表
-define(ETS_AG_Pool, ets_ag_Pool).
-define(ETS_AG_Agency, ets_ag_Agency).
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(module(), term(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(Name, Args, SpawnOpts) ->
   proc_lib:start_link(?MODULE, init_it, [Name, self(), Args], infinity, SpawnOpts).

init_it(Name, Parent, Args) ->
   case safeRegister(Name) of
      true ->
         process_flag(trap_exit, true),
         moduleInit(Parent, Args);
      {false, Pid} ->
         proc_lib:init_ack(Parent, {error, {already_started, Pid}})
   end.

%% sys callbacks
-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
   {ok, State}.

-spec system_continue(pid(), [], {module(), atom(), pid(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, State}) ->
   loop(Parent, State).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state(State) ->
   {ok, State}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _Parent, _Debug, _State) ->
   exit(Reason).

safeRegister(Name) ->
   try register(Name, self()) of
      true -> true
   catch
      _:_ -> {false, whereis(Name)}
   end.

moduleInit(Parent, Args) ->
   case ?MODULE:init(Args) of
      {ok, State} ->
         proc_lib:init_ack(Parent, {ok, self()}),
         loop(Parent, State);
      {stop, Reason} ->
         proc_lib:init_ack(Parent, {error, Reason}),
         exit(Reason)
   end.

loop(Parent, State) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, State});
      {'EXIT', Parent, Reason} ->
         terminate(Reason, State);
      Msg ->
         {ok, NewState} = ?MODULE:handleMsg(Msg, State),
         loop(Parent, NewState)
   end.

terminate(Reason, State) ->
   ?MODULE:terminate(Reason, State),
   exit(Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init(Args :: term()) -> ok.
init(_Args) ->
   ets:new(?ETS_AG_Pool, [named_table, set, public]),
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
   ?WARN(agAgencyPoolMgr, "receive unexpected  msg: ~p", [_Msg]),
   {ok, State}.

%% public
-spec startPool(poolName(), clientOpts()) -> ok | {error, pool_name_used}.
startPool(PoolName, ClientOpts) ->
   startPool(PoolName, ClientOpts, []).

-spec startPool(poolName(), clientOpts(), poolOpts()) -> ok | {error, pool_name_used}.
startPool(PoolName, ClientOpts, PoolOpts) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         gen_server:call(?MODULE, {startPool, PoolName, ClientOpts, PoolOpts});
      _ ->
         {error, pool_name_used}
   end.

-spec stopPool(poolName()) -> ok | {error, pool_not_started}.
stopPool(PoolName) ->
   case ?agBeamPool:get(PoolName) of
      undefined ->
         {error, pool_not_started};
      _ ->
         gen_server:call(?MODULE, {stopPool, PoolName})

   end.

dealStart(PoolName, ClientOpts, PoolOpts) ->
   #poolOpts{poolSize = PoolSize} = PoolOptsRec = poolOptsToRec(PoolOpts),
   startChildren(PoolName, ClientOpts, PoolOptsRec),
   cacheAddPool(PoolName, PoolSize),
   cacheAddAgency(PoolName, PoolSize),
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
   agTcpAgency;
agencyMod(ssl) ->
   agSslAgency;
agencyMod(_) ->
   agTcpAgency.

agencySpec(ServerMod, ServerName, ClientOptions) ->
   StartFunc = {ServerMod, start_link, [ServerName, ClientOptions, []]},
   {ServerName, StartFunc, permanent, 5000, worker, [ServerMod]}.

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
               ?ETS_AG_Agency:get({PoolName, PoolSize});
            _ ->
               ?ETS_AG_Agency:get({PoolName, AgencyIdx})
         end
   end.
