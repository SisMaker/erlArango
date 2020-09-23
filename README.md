# erlArango
    arangodb多模数据库erlang驱动程序
    erlang otp21.2+ arangodb 3.6.2 3.7
    
## 特点
  高效，快速，简单易用。
  1. 为了该驱动尽可能的高效,定制化封装了一个带连接池的http1.1的客户端(agHttpCli)
  封装的agHttpCli与同类http客户端测试对比可参考:https://github.com/SisMaker/httpc_bench
  2. 为了更加快速的decode和encode json数据，json库引入了jiffy，经过测试jiffy效率还是很不错的。
  3.  该驱动可以使用连接池，也可以仅仅在单进程(非连接池模式)建立多个连接进行各种数据操作。使用连接池时支持同步与异步操作，如果要使用异步操作需要额外保存requestId
  等待接收数据返回，当前改驱动封装的API均使用同步操作，如果需要异步操作可自行修改。单进程操作时仅支持同步操作。
  单进程模式下相对连接池模式可以减少一次数据在进程间的复制，对于大量数据的操作，可以考虑在数据管理进程单独建立数据库连接，而不用连接池。
  4. 连接池模式和非连接池模式API接口保证了同一性，不用区别对待， 易于理解和连接池模式和非连接池模式相互转换修改。

## 暂不支持批处理请求
    https://www.arangodb.com/docs/stable/http/batch-request.html 
  
## 编译
    rebar get-deps; rebar compile or rebar3 compile
    注意:在windows平台编译jiffy，需要额外搭建相关编译环境，具体可参见:https://github.com/SisMaker/erlUtils/tree/master/src/docs
  
## 使用
    rebar: erl -pa ./ebin -pa ./deps/jiffy/ebin
    revar3: rebar3 shell
    非连接池模式
    先建立连接
        {ok, S} = agHttpCli:connect([]).                 %% 使用默认的配置
        然后就可以使用S作为第一个参数调用各种API了
        agMgrDb:curDbInfo(S).
    
    连接池模式
       application:ensure_all_started(erlArango).        %%启动app
       agHttpCli:startPool(poolName, [], []).            %%初始连接池
       然后就可以使用poolName作为第一个参数调用各种API了  
       agMgrDb:curDbInfo(poolName).  
       
## TODO
    将注释转为edoc格式         
       
