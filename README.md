# erlArango
    arangodb erlang driver
    erlang otp21.2+ arangodb 3.6.2 3.7
    
# Feature
  Efficient, fast and easy to use.
  1. To make this driver as efficient as possible, customizations encapsulate an HTTP1.1 client(agHttpCli) with connection pooling.
       Comparisons between packaged agHttpCli and similar HTTP client tests are available:[Address](https://github.com/SisMaker/httpc_bench)
  2. This driver can use connection pooling or simply establish multiple connections in a single process (non-connection pooling mode) for various data operations.
     Synchronous and asynchronous operations are supported when using connection pooling,
     and you need to save the requestId extra if you want to use asynchronous operations Waiting for the received data to return,
     the API encapsulated by the current driver all USES synchronous operation, and can be modified if asynchronous operation is needed.
     Only synchronous operations are supported for single-process operations.
     In single-process mode, compared with connection pooling mode, data replication between processes can be reduced once.
     For operation of large amount of data, database connection can be established separately in data management process instead of connection pooling.
  3. The connection pooling mode and connectionless pool mode API interface ensures the identity, does not need to be treated differently,
   and is easy to understand and change between connection pooling mode and connectionless pool mode.

# Batch requests are not supported
    https://www.arangodb.com/docs/stable/http/batch-request.html 

# compile
    rebar get-deps; rebar compile or rebar3 compile
    Note: If you build Jiffy on The Windows platform, you will need to set up an additional compilation environment. [See jiffy for details](https://github.com/SisMaker/erlUtils/tree/master/src/docs)

# how to use
    rebar: erl -pa ./ebin -pa ./deps/jiffy/ebin or
    revar3: rebar3 shell
    Non-connection pooling mode
    Make a connection first
        {ok, Socket} = agHttpCli:connect([]).           %% Use default Settings
        %% Then you can then call various apis using Socket as the first argument
        agMgrDb:curDbInfo(Socket).
    
    Connection pooling mode
       application:ensure_all_started(erlArango).        %% start app
       agHttpCli:startPool(poolName, [], []).            %% start pool
       %% Then you can then invoke various apis using poolName as the first argument
       agMgrDb:curDbInfo(poolName).  

       
