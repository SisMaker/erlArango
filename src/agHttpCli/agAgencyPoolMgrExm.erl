-module(agAgencyPoolMgrExm).

-compile(inline).
-compile({inline_size, 128}).

-export([
   start_link/3
   , init_it/3
   , system_code_change/4
   , system_continue/3
   , system_get_state/1
   , system_terminate/4
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genExm  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(module(), term(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(Name, Args, SpawnOpts) ->
   proc_lib:start_link(?MODULE, init_it, [Name, self(), Args], infinity, SpawnOpts).

init_it(Name, Parent, Args) ->
   case safeRegister(Name) of
      true ->
         process_flag(trap_exit, true),
         moduleInit(Parent, Args);
      {false, Pid} ->
         proc_lib:init_ack(Parent, {error, {alreadyStarted, Pid}})
   end.

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
   case agAgencyPoolMgrIns:init(Args) of
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
         {ok, NewState} = agAgencyPoolMgrIns:handleMsg(Msg, State),
         loop(Parent, NewState)
   end.

terminate(Reason, State) ->
   agAgencyPoolMgrIns:terminate(Reason, State),
   exit(Reason).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genExm  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


