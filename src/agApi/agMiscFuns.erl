-module(agMiscFuns).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/miscellaneous-functions.html

% 各种功能的HTTP接口
% 这是ArangoDB的其他功能的HTTP接口的概述。

% 返回服务器版本号
% GET /_api/version
% 查询参数
%    details（可选）：如果设置为true，则响应将包含一个details属性，其中包含有关所包含组件及其版本的附加信息。详细信息对象的属性名称和内部信息可能会因平台和ArangoDB版本而异。
% 返回服务器名称和版本号。响应是具有以下属性的JSON对象：
% 在所有情况下均返回HTTP 200。
% 服务器：将始终包含arango
%    version：服务器版本字符串。该字符串的格式为“ major”。小的。子 ”。major和minor将是数字，sub 可能包含数字或文本版本。
%    details：一个可选的JSON对象，带有其他详细信息。仅当请求中的details查询参数设置为true时，才返回此值。
%       architecture：CPU体系结构，即64位
%       arm： false-这不在ARM cpu上运行
%       asan：是否已在asan地址清理器打开的情况下进行编译？（应该是错误的）
%       asm-crc32：我们有汇编程序实现的CRC函数吗？
%       assertions：我们是否有在（=>开发人员版本）中编译的断言
%       boost-version：我们绑定哪个boost版本
%       build-date：创建此二进制文件的日期
%       build-repository：引用从其编译的git-ID
%       compiler：我们使用了哪个编译器
%       cplusplus：C ++标准版本
%       debug： 生产二进制文件为false
%       endianness：目前仅支持很少
%       failure-tests： 对于生产二进制文件为false（禁用致命错误的功能）
%       fd-client-event-handler：我们使用哪种方法来处理fd-set，民意调查应该在linux上进行。
%       fd-setsize：如果不轮询，则fd setsize对于文件描述符的最大数目有效
%       full-version-string：完整版本字符串
%       icu-version：我们捆绑哪个版本的ICU
%       jemalloc： 如果我们使用jemalloc，则为true
%       maintenanceer-mode： 如果这是生产二进制文件，则为false
%       openssl-version：我们链接哪个openssl版本？
%       platform：主机os- linux，windows或darwin
%       reactor-type： epoll TODO
%       rockdb-version：此发行版捆绑的rocksdb版本
%       server-version：ArangoDB发行版本
%       sizeof int：整数的字节数
%       sizeof void ：* void指针的字节数
%       sse42：我们是否具有启用SSE 4.2的CPU？
%       unaligned-access：此系统是否支持未对齐的内存访问？
%       v8-version：捆绑的V8 JavaScript引擎版本
%       vpack-version：使用的velocypack实现的版本
%       zlib-version：捆绑的zlib的版本
%       mode：我们作为-[ 服务器，控制台，脚本 ]中的一种运行的模式
%       host：主机ID
srvVersion(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/version">>, [], undefined).

srvVersion(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/version", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 返回运行服务器的引擎类型
% GET /_api/engine
% 返回服务器配置为使用的存储引擎。响应是具有以下属性的JSON对象：
% 在所有情况下均返回HTTP 200。
% 名称：将是mmfiles或rocksdb
srvEngine(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/engine">>, [], undefined).

% 将WAL同步到磁盘。
% PUT /_admin/wal/flush
% 查询参数
%    waitForSync（可选）：在预写日志中尚未同步的数据同步到磁盘之前，是否应该阻塞该操作。
%    waitForCollector（可选）：在预写日志垃圾收集器收集刷新日志中的数据之前，是否应阻塞该操作。请注意，如果存在长时间运行的事务并且将预写日志垃圾收集器无法完成垃圾收集，则将此选项设置为true可能会阻塞很长时间。
% 刷新预写日志。通过刷新当前活动的预写日志文件，可以将其中的数据传输到收集日志和数据文件中。这对于确保收集的所有数据都存在于收集日志和数据文件中（例如，在转储收集的数据时）很有用。
% 返回码
%    200：操作成功返回。
%    405：使用无效的HTTP方法时返回。
flushWal(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_admin/wal/flush">>, [], undefined).

flushWal(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_admin/wal/flush", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 获取当前配置。
% GET /_admin/wal/properties
% 检索预写日志的配置。结果是具有以下属性的JSON对象：
%    allowOversizeEntries：是否可以执行和存储大于单个日志文件的操作
%    logfileSize：每个预写日志文件的大小
%    HistoricLogfiles：要保留的最大历史日志文件数
%    reserveLogfiles：ArangoDB在后台分配的最大保留日志文件数
%    syncInterval：尚未同步的预写日志数据的自动同步时间间隔（以毫秒为单位）
%    valveWait：如果发生写限制，则操作在中止之前将等待的最大等待时间（以毫秒为单位）
%    acceleratorWhenPending：达到该数量时将激活写限制的未处理垃圾收集操作的数量。值为 0表示将不触发写限制。
% 返回码
%    200：操作成功返回。
%    405：使用无效的HTTP方法时返回。
getWalProps(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/wal/properties">>, [], undefined).

% 配置Wal的参数
% PUT /_admin/wal/properties
% 配置预写日志的行为。请求的主体必须是具有以下属性的JSON对象：
%    allowOversizeEntries：是否可以执行和存储大于单个日志文件的操作
%    logfileSize：每个预写日志文件的大小
%    HistoricLogfiles：要保留的最大历史日志文件数
%    reserveLogfiles：ArangoDB在后台分配的最大保留日志文件数
%    valveWait：如果发生写限制，则操作在中止之前将等待的最大等待时间（以毫秒为单位）
%    acceleratorWhenPending：达到该数量时将激活写限制的未处理垃圾收集操作的数量。值为 0表示将不触发写限制。
% 指定以上任何属性都是可选的。未指定的属性将被忽略，并且它们的配置也不会被修改。
% 返回码
%    200：操作成功返回。
%    405：使用无效的HTTP方法时返回。
setWalProps(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_admin/wal/properties">>, BodyStr, undefined).

% 返回有关当前正在运行的事务的信息
% GET /_admin/wal/transactions
% 返回有关当前正在运行的事务的信息。结果是具有以下属性的JSON对象：
%    runningTransactions：当前正在运行的事务数
%    minLastCollected：最近收集的日志文件的最小ID（在每个正在运行的事务的开始处）。如果没有事务在运行，则为null。
%    minLastSealed：最后密封的日志文件的最小ID（在每个运行的事务的开始处）。如果没有事务在运行，则为null。
% 返回码
%    200：操作成功返回。
%    405：使用无效的HTTP方法时返回。
getTransactions(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/wal/transactions">>, [], undefined).

% 获取系统的当前时间
% GET /_admin/time
% 该调用返回一个属性为time的对象。它包含当前系统时间（以Unix时间戳为单位，精度为微秒）。
%    HTTP 200时间已成功返回。
%    error：布尔值标志，指示是否发生错误（在这种情况下为false）
%    code：HTTP状态码
%    time：当前系统时间（以Unix时间戳记为单位，服务器以微秒为单位）
curDbTime(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/time">>, [], undefined).

% 返回当前请求永久链接固定链接
% 发回所发送的内容，标题，帖子正文等。
% POST /_admin/echo
% 请求正文（对象）
% 主体可以是任何类型，只需转发即可。
% 调用返回一个带有服务器请求信息的对象
% HTTP 200 Echo成功返回。
%    authorized：会话是否被授权
%    user：当前发送此请求的用户
%    database：执行此请求的数据库
%    url：原始请求URL
%    protocol：传输，['http'，'https'，'velocystream']之一
%    server：
%       address：此请求发送到的端点的绑定地址
%       port：此请求发送到的端口
%    client：客户端连接的属性
%       address：客户端的IP地址
%       port：tcp连接的客户端端口
%       id：服务器生成的id
%    internals：服务器内部结构的内容
%    prefix：数据库的前缀
%    headers：您发送的HTTP标头的列表
%    requestType：在这种情况下为POST，如果您使用其他HTTP-Verb，您将看到它（获取/删除，...）
%    requestBody：我们发送的POST正文的字符串化版本
%    parameters：包含查询参数的对象
%    cookies：您发送的cookie列表
%    suffix：
%    rawSuffix：
%    path：此请求的相对路径
%    rawRequestBody：已发送字符的数字列表
echo(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/echo">>, [], BodyStr).

% 返回数据库的版本。
% GET /_admin/database/target-version
% 返回此服务器所需的数据库版本。版本将在结果的版本属性中返回。
% 返回码
% 200：在所有情况下均返回。
targetVersion(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/database/target-version">>, [], undefined).

% 启动关机序列
% DELETE /_admin/shutdown
% 此调用将启动干净的关机序列。需要管理权限
% 返回码
% 200：在所有情况下OK都将返回，成功时将在结果缓冲区中返回。
shutDown(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, <<"/_admin/shutdown">>, [], undefined).


% 在服务器上执行脚本。
% POST /_admin/execute
% 请求正文（字符串）
% 要执行的主体。
% 将服务器主体中的javascript代码作为不带参数的函数主体执行。如果您有return语句，那么您产生的返回值将作为内容类型 application / json返回。如果参数returnAsJSON设置为 true，则结果将是一个直接描述返回值的JSON对象，否则将返回JSON.stringify生成的字符串。
% 请注意，只有在使用option启动服务器时，此API端点才会存在--javascript.allow-admin-execute true。
% 此选项的默认值为false，它禁用用户定义代码的执行并完全禁用此API端点。这也是建议的生产设置。
% 返回码
% 200：一切正常或发生超时时返回。在后一种情况下，将返回指示超时的application / json类型的主体。根据returnAsJSON的不同，这是一个json对象或纯字符串。
% 403：如果ArangoDB不在集群模式下运行，则返回。
% 404：如果未为群集操作编译ArangoDB，则返回404。
execute(PoolNameOrSocket, BodyStr) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/execute">>, [], BodyStr).

% 返回服务器的状态信息。
% GET /_admin/status
% 返回有关服务器的状态信息。
% 这是供支持人员手动使用的，决不能用于监视或自动测试。结果如有更改，恕不另行通知。
% 该调用返回具有以下属性的对象：
%    server：总是arango。
%    license：社区或企业。
%    version：服务器版本，以字符串形式。
%    mode：服务器或控制台。
%    host：主机名，请参阅ServerState。
%    serverInfo.role：单个，协调器，主要，代理。
%    serverInfo.writeOpsEnabled：布尔值，如果启用写入，则为true。
%    serverInfo.maintenance：布尔值，如果启用了维护模式，则为true。
%    agency.endpoints：可能的代理商端点的列表。
%    agency，协调员或主要代理商也将拥有
%    serverInfo.persistedId：保留的ide，例如“ CRDN-e427b441-5087-4a9a-9983-2fb1682f3e2a”。
%    协调员或主要人员也将拥有
%    serverInfo.state：服务中
%    serverInfo.address：服务器的地址，例如tcp：// [:: 1]：8530。
%    serverInfo.serverId：服务器端，例如“ CRDN-e427b441-5087-4a9a-9983-2fb1682f3e2a”。
%    协调员还将有
%    coordinator.foxxmaster：foxx主服务器的服务器ID。
%    coordinator.isFoxxmaster：布尔值，如果服务器是foxx主服务器，则为true。
%    代理商也将拥有
%    agent.id：此代理的服务器ID。
%    agent.leaderId：领导者的服务器ID。
%    agent。Leading：布尔值，如果为领先则为 true。
%    agent.endpoint：此代理的端点。
%    agent.term：当前术语编号。
% 返回码
%    200：状态信息返回成功
dbStatus(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/status">>, [], undefined).

