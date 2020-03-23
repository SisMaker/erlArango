-module(agReplication).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/3.6/http/replications.html

% 复制转储命令
% 该库存方法可用于查询ArangoDB数据库的当前设置的集合加上他们的指标。客户可以使用此方法来概述数据库中存在哪些集合。他们可以使用此信息来启动数据的全部或部分同步，例如启动备份或增量数据同步。

% 返回集合及其索引的概述
% GET /_api/replication/inventory
% 查询参数
% includeSystem（可选）：在结果中包括系统集合。默认值为true。
% 全局（可选）：在响应中包括所有数据库。仅适用于_system默认值为false。
% batchId（必填）：此API调用的RocksDB引擎需要有效的batchId
% 返回服务器上可用的集合和索引的数组。复制客户端可以使用此阵列来启动与服务器的初始同步。
% 响应将包含具有collection和state和 tick属性的JSON对象。
% 集合是具有以下子属性的集合数组：
% 参数：集合属性
% 索引：集合索引的数组。主索引和边缘索引不包含在此数组中。
% 该状态属性包含复制记录器的当前状态。它包含以下子属性：
% running：复制记录器当前是否处于活动状态。注意：从ArangoDB 2.2开始，该值将始终为true
% lastLogTick：复制记录器已写入的最后一个滴答的值
% time：服务器上的当前时间
% 复制客户端应注意返回的lastLogTick值。然后，他们可以使用转储方法获取集合的数据直到lastLogTick的值，并在此滴答值之后查询连续复制日志中的日志事件。
% 要在服务器上创建集合的完整副本，复制客户端可以执行以下步骤：
% 调用/ inventory API方法。这将从服务器返回lastLogTick值以及集合和索引的数组。
% 对于/ inventory返回的每个集合，请在本地创建集合，然后调用/ dump将集合数据流式传输到客户端，直到lastLogTick的值 为止。之后，客户端可以在/ inventory报告的集合上创建索引。
% 如果客户端要从记录器服务器连续流式传输复制日志事件，则需要执行以下附加步骤：
% 客户端应首先调用/ logger-follow来获取在客户端调用/ inventory之后记录的第一批复制事件。
% 对/ logger-follow的调用应使用from参数，其值应为/ inventory报告的lastLogTick的值 。调用/ logger-follow将返回 x-arango-replication-lastincluded，其中将包含响应中包含的最后一个滴答值。
% 然后，客户端可以连续调用/ logger-follow以递增地获取上次传输后发生的新复制事件。
% 调用应使用from参数，并带有 上一个响应的x-arango-replication-lastincluded头的值。如果没有更多的复制事件，则响应将为空，客户端可以休眠一会儿，然后再试一次。
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DBserver的ID。相同的请求被同步转发到该DBserver。如果此属性未在协调程序情况下绑定，则是错误的。
% 注意：：global顶层对象使用参数包含一个键databases ，每个键下的一个键代表一个datbase名称，并且值符合上述说明。
% 返回码
% 200：如果请求成功执行，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
getRepInventory(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/inventory", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, Path, [], undefined).

% 处理转储批处理命令
% POST /_api/replication/batch
% 注意：这些调用对用户而言并不有趣。
% 具有以下属性的JSON对象是必需的：
% ttl：新批处理的生存时间（以秒为单位）
% 具有批处理配置的JSON对象。
% 创建一个新的转储批次并返回批次的ID。
% 响应是具有以下属性的JSON对象：
% id：批次的ID
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DBserver的ID。相同的请求被同步转发到该DBserver。如果此属性未在协调程序情况下绑定，则是错误的。
% 返回码
% 200：批量创建成功，返回
% 400：如果ttl值无效，或者在协调器上未指定DBserver属性或该属性非法，则返回。
% 405：使用无效的HTTP方法时返回。
newRepBatch(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, <<"/_api/replication/batch">>, [], BodyStr).


% 删除现有的转储批次
% 处理转储批处理命令
% DELETE /_api/replication/batch/{id}
% 注意：这些调用对用户而言并不有趣。
% 路径参数
% id（必填）：批次的ID。
% 删除现有的转储批处理，从而允许恢复压缩和清理。
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DBserver的ID。相同的请求被同步转发到该DBserver。如果此属性未在协调程序情况下绑定，则是错误的。
% 返回码
% 204：批量删除成功，返回。
% 400：如果找不到批次，则返回。
% 405：使用无效的HTTP方法时返回。
delRepBatch(PoolNameOrSocket, BatchId) ->
   Path = <<"/_api/replication/batch/", (agMiscUtils:toBinary(BatchId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, Path, [], undefined).


% 处理转储批处理命令
% PUT /_api/replication/batch/{id}
% 注意：这些调用对用户而言并不有趣。
% 路径参数
% id（必填）：批次的ID。
% 具有以下属性的JSON对象是必需的：
% ttl：新批次的生存时间（以秒为单位）
% 使用批次的ID和提供的ttl值来扩展现有转储批次的ttl。
% 如果可以成功扩展批次的ttl，则响应为空。
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DBserver的ID。相同的请求被同步转发到该DBserver。如果此属性未在协调程序情况下绑定，则是错误的。
% 返回码
% 204：如果成功扩展了批次的ttl，则返回。
% 400：如果ttl值无效或未找到批次，则返回。
% 405：使用无效的HTTP方法时返回。
% 该转储方法可用于从特定集合中获取数据。由于dump命令的结果可能非常庞大，所以dump可能不会一次返回集合中的所有数据。而是，复制客户端可能会重复调用dump命令，直到没有更多数据要提取为止。dump命令不仅会返回集合中的当前文档，还会返回文档的更新和删除。
% 请注意，dump方法将仅返回文档，日记帐和数据文件中的更新，删除。仅存储在预写日志中的操作将不会返回。为了确保这些操作包含在转储中，必须先清除预写日志。
% 为了获得相同的数据状态，复制客户端应按照提供的顺序使用转储结果的各个部分。
prolongRepBatch(PoolNameOrSocket, BatchId, MapData) ->
   Path = <<"/_api/replication/batch/", (agMiscUtils:toBinary(BatchId))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?Put, Path, [], BodyStr).

% 返回一个集合的全部内容
% GET /_api/replication/dump
% 查询参数
% 集合（必填）：要转储的集合的名称或ID。
% chunkSize（可选）：
% batchId（必填）：仅rocksdb-要使用的快照的ID
% from（可选）：仅mmfiles-结果的下界刻度值。
% 到（可选）：仅mmfiles-结果的上限刻度值。
% includeSystem（可选）：仅mmfiles-在结果中包括系统集合。默认值为true。
% ticks（可选）：仅mmfiles-是否在转储中包括tick值。默认值为true。
% 刷新（可选）：仅mmfiles-是否在转储前刷新WAL。默认值为true。
% 从集合中返回请求范围内的数据。
% 如果不使用from查询参数，则从头开始返回收集事件。使用from参数时，结果将仅包含滴答值高于指定的from值的集合条目（注意：滴答值等于from的日志条目将被排除）。
% 的到查询参数可被用于任选地限制上部结合的结果到一定刻度值。如果使用，结果将仅包含滴答值最大为（包括）到的集合条目。
% 所述CHUNKSIZE查询参数可用于控制结果的大小。必须以字节为单位指定。该CHUNKSIZE值也仅是被兑现。否则，chunkSize值太低可能导致服务器无法仅将一个条目放入结果中并返回它。因此，只有在将条目写入结果后才能查询chunkSize值。如果结果大小大于 chunkSize，则服务器将以与响应中已经存在的条目一样多的条目进行响应。如果结果大小仍小于chunkSize，则如果还有更多数据要返回，则服务器将尝试返回更多数据。
% 如果未指定chunkSize，则将使用某些服务器端默认值。
% 结果的Content-Type是application / x-arango-dump。这是一种易于处理的格式，所有条目都放入响应正文中的单独行中。
% 每行本身是一个JSON对象，至少具有以下属性：
% tick：操作的tick属性
% key：文档/边缘的密钥或删除操作中使用的密钥
% rev：文档/边缘或删除操作的修订版ID
% data：类型2300和2301的实际文档/边缘数据。完整的文档/边缘数据将被返回，甚至进行更新。
% type：条目的类型。类型的可能值为：
% 2300：文档插入/更新
% 2301：边缘插入/更新
% 2302：删除文档/边缘
% 注意：调用此方法时，插入和更新之间没有区别。
% 返回码
% 200：如果成功执行了请求并返回了数据，则返回。标头 x-arango-replication-lastincluded设置为最后返回的文档的刻度。
% 204：如果请求已成功执行，但没有可用内容，则返回。在这种情况下，标题x-arango-replication-lastincluded是0。
% 400：如果from或to值无效，则返回。
% 404：找不到集合时返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
getRepDump(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/dump", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, Path, [], undefined).

% 从远程端点同步数据
% 开始复制
% PUT /_api/replication/sync
% 具有以下属性的JSON对象是必需的：
% 端点：要连接的主端点（例如“ tcp：//192.168.173.13：8529”）。
% database：主数据库上的数据库名称（如果未指定，则默认为本地当前数据库的名称）。
% username：连接到端点时要使用的可选ArangoDB用户名。
% password：连接到端点时使用的密码。
% includeSystem：是否将应用系统收集操作
% 增量：如果设置为true，则将使用增量同步方法来同步集合中的数据。当集合已经在本地存在并且仅需要从远程端点转移剩余的差异时，此方法很有用。在这种情况下，增量同步可以比完全同步更快。默认值为false，这意味着将传输来自远程集合的完整数据。
% strictType：用于集合过滤的可选字符串值。指定时，允许的值包括include或exclude。
% restrictCollections：集合用于在使用的可选阵列 restrictType。如果limitType为include，则仅指定的集合将被同步。如果limitType为exclude，则将同步除指定集合以外的所有集合。
% initialSyncMaxWaitTime：在获取初始收集数据时，初始同步将等待主服务器响应的最长时间（以秒为单位）。此等待时间可用于控制初始同步将在多长时间后放弃等待响应并失败。如果设置为0，则将忽略此值。
% 启动从远程端点到本地ArangoDB数据库的完整数据同步。
% 该同步方法可以通过复制客户端使用一个ArangoDB数据库连接到远程端点，取收集和索引，以及收集数据的远程列表。因此，它将在远程ArangoDB数据库上创建数据状态的本地备份。同步在每个数据库级别进行。
% sync首先会从远程端点获取集合和索引的列表。通过调用远程数据库的清单 API来实现。然后它将清除本地ArangoDB数据库中的数据，并且在启动后会将收集数据从远程数据库传输到本地ArangoDB数据库。它将通过调用远程数据库的转储 API 从远程数据库中提取数据，直到获取所有数据为止。
% 如果成功，响应的主体是具有以下属性的JSON对象：
% 集合：从端点转移来的一系列集合
% lastLogTick：传输开始时端点上的最后一个日志滴答。使用这个值作为从后开始连续同步时的价值。
% 警告：调用此方法将把数据从远程端点上的集合同步到本地ArangoDB数据库。本地集合中的所有数据将被清除，并替换为端点中的数据。
% 请谨慎使用！
% 注意：集群中的协调器不支持此方法。
% 返回码
% 200：如果请求成功执行，则返回。
% 400：配置不完整或格式错误，返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果同步期间发生错误，则返回。
% 501：在集群中的协调器上调用此操作时返回。
startRepSync(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?Put, <<"/_api/replication/sync">>, [], BodyStr).

% 返回集合和索引的集群清单
% 重建集群中的集合和索引的概述
% GET /_api/replication/clusterInventory
% 查询参数
% includeSystem（可选）：在结果中包括系统集合。默认值为true。
% 返回群集上可用的集合和索引的数组。
% 响应将是一个JSON对象数组，每个集合一个。每个集合精确地包含两个关键的“参数”和“索引”。此信息来自计划/收藏/ {DB-名称} / * 的机构，只是对指标属性有搬迁到其调整到arangodump的数据格式。
% 返回码
% 200：如果请求成功执行，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
getRepClusterInventory(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/clusterInventory", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, Path, [], undefined).
