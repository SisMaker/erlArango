-module(agSslAgencyExm).

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
start_link(ServerName, Args, SpawnOpts) ->
   proc_lib:start_link(?MODULE, init_it, [ServerName, self(), Args], infinity, SpawnOpts).

init_it(ServerName, Parent, Args) ->
   case safeRegister(ServerName) of
      true ->
         process_flag(trap_exit, true),
         moduleInit(Parent, Args);
      {false, Pid} ->
         proc_lib:init_ack(Parent, {error, {alreadyStarted, Pid}})
   end.

-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(MiscState, _Module, _OldVsn, _Extra) ->
   {ok, MiscState}.

-spec system_continue(pid(), [], {module(), term(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, SrvState, CliState}) ->
   loop(Parent, SrvState, CliState).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state({_Parent, SrvState, _CliState}) ->
   {ok, SrvState}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _Parent, _Debug, {_Parent, SrvState, CliState}) ->
   terminate(Reason, SrvState, CliState).

safeRegister(ServerName) ->
   try register(ServerName, self()) of
      true -> true
   catch
      _:_ -> {false, whereis(ServerName)}
   end.

moduleInit(Parent, Args) ->
   case agSslAgencyIns:init(Args) of
      {ok, SrvState, CliState} ->
         proc_lib:init_ack(Parent, {ok, self()}),
         loop(Parent, SrvState, CliState);
      {stop, Reason} ->
         proc_lib:init_ack(Parent, {error, Reason}),
         exit(Reason)
   end.

loop(Parent, SrvState, CliState) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, SrvState, CliState});
      {'EXIT', Parent, Reason} ->
         terminate(Reason, SrvState, CliState);
      Msg ->
         {ok, NewSrvState, NewCliState} = agSslAgencyIns:handleMsg(Msg, SrvState, CliState),
         loop(Parent, NewSrvState, NewCliState)
   end.

terminate(Reason, SrvState, CliState) ->
   agSslAgencyIns:terminate(Reason, SrvState, CliState),
   exit(Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genExm  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%