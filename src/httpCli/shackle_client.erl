-module(shackle_client).
-include("shackle_internal.hrl").

-callback init(Options :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()}.

-callback setup(Socket :: inet:socket(), State :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handleRequest(Request :: term(), State :: term()) ->
    {ok, RequestId :: external_request_id(), Data :: iodata(), State :: term()}.

-callback handleData(Data :: binary(), State :: term()) ->
    {ok, [Response :: response()], State :: term()} |
    {error,  Reason :: term(), State :: term()}.

-callback terminate(State :: term()) ->
    ok.
