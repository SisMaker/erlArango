-module(agReplication).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/3.6/http/replications.html

% 用于复制的HTTP接口
% 复制
% 这是对ArangoDB的HTTP复制接口的介绍。复制架构和组件中更详细地描述了 复制。
%
% HTTP复制接口有四个主要用途：
%    从服务器获取初始数据（例如用于备份，或用于在启动连续复制应用程序之前进行数据的初始同步）
%    查询主机状态
%    从主服务器获取连续更改（用于更改的增量同步）
%    在从属服务器上管理复制应用程序（启动，停止，配置，查询状态）
% 请注意，如果使用每个数据库的设置（与服务器级复制相反，从v3.3.0开始可用），则必须为每个数据库分别配置复制系统，并且复制多个数据库的数据将需要进行多个操作。

% 复制转储命令
% 该库存方法可用于查询ArangoDB数据库的当前设置的集合加上他们的指标。客户可以使用此方法来概述数据库中存在哪些集合。他们可以使用此信息来启动数据的全部或部分同步，例如启动备份或增量数据同步

% 返回集合及其索引的概述
% GET /_api/replication/inventory
% 查询参数
%     includeSystem（可选）：在结果中包括系统集合。默认值为true。
%     global （可选）：在响应中包括所有数据库。仅适用于_system默认值为false。
%     batchId（必填）：此API调用的RocksDB引擎需要有效的batchId
% 返回服务器上可用的集合和索引的数组。复制客户端可以使用此阵列来启动与服务器的初始同步。
% 响应将包含具有collection和state和 tick属性的JSON对象。
% 集合是具有以下子属性的集合数组：
%     parameters：集合属性
%     indexes：集合索引的数组。主索引和边缘索引不包含在此数组中。
% 该状态属性包含复制记录器的当前状态。它包含以下子属性：
%     running：复制记录器当前是否处于活动状态。注意：从ArangoDB 2.2开始，该值将始终为true
%     lastLogTick：复制记录器已写入的最后一个滴答的值
%     time：服务器上的当前时间
% 复制客户端应注意返回的lastLogTick值。然后，他们可以使用转储方法获取集合的数据直到lastLogTick的值，并在此滴答值之后查询连续复制日志中的日志事件。
% 要在服务器上创建集合的完整副本，复制客户端可以执行以下步骤：
%     调用/ inventory API方法。这将从服务器返回lastLogTick值以及集合和索引的数组。
%     对于/ inventory返回的每个集合，请在本地创建集合，然后调用/ dump将集合数据流式传输到客户端，直到lastLogTick的值 为止。之后，客户端可以在/ inventory报告的集合上创建索引。
% 如果客户端要从记录器服务器连续流式传输复制日志事件，则需要执行以下附加步骤：
%     客户端应首先调用/ logger-follow来获取在客户端调用/ inventory之后记录的第一批复制事件。
%     对/ logger-follow的调用应使用from参数，其值应为/ inventory报告的lastLogTick的值 。调用/ logger-follow将返回 x-arango-replication-lastincluded，其中将包含响应中包含的最后一个滴答值。
%     然后，客户端可以连续调用/ logger-follow以递增地获取上次传输后发生的新复制事件。
%     调用应使用from参数，并带有 上一个响应的x-arango-replication-lastincluded头的值。如果没有更多的复制事件，则响应将为空，客户端可以休眠一会儿，然后再试一次。
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DBserver的ID。相同的请求被同步转发到该DBserver。如果此属性未在协调程序情况下绑定，则是错误的。
% 注意：：global顶层对象使用参数包含一个键databases ，每个键下的一个键代表一个datbase名称，并且值符合上述说明。
% 返回码
%     200：如果请求成功执行，则返回。
%     405：使用无效的HTTP方法时返回。
%     500：如果组装响应时发生错误，则返回500。
getRepInventory(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/inventory", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 创建新的转储批次
% 处理转储批处理命令
% POST /_api/replication/batch
% 注意：这些调用对用户而言并不有趣。
% 具有以下属性的JSON对象是必需的：
%    ttl：新批处理的生存时间（以秒为单位）
% 具有批处理配置的JSON对象。
% 创建一个新的转储批次并返回批次的ID。
% 响应是具有以下属性的JSON对象：
%    id：批次的ID
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DB-Server的ID。相同的请求被同步转发到该DB服务器。如果在Coordinator情况下未绑定此属性，则会出错。
% 返回码
%    200：批量创建成功，返回
%    400：如果ttl值无效，或者在Coordinator上未指定DBserver属性或该属性非法，则返回400 。
%    405：使用无效的HTTP方法时返回。
newRepBatch(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/replication/batch">>, [], BodyStr).

% 删除现有的转储批次固定链接
% 处理转储批处理命令
% DELETE /_api/replication/batch/{id}
% 注意：这些调用对用户而言并不有趣。
% 路径参数
%    id（必填）：批次的ID。
% 删除现有的转储批处理，从而允许恢复压缩和清理。
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DB-Server的ID。相同的请求被同步转发到该DB服务器。如果在Coordinator情况下未绑定此属性，则会出错。
% 返回码
%    204：批量删除成功，返回。
%    400：如果找不到批次，则返回。
%    405：使用无效的HTTP方法时返回。
delRepBatch(PoolNameOrSocket, BatchId) ->
   Path = <<"/_api/replication/batch/", (agMiscUtils:toBinary(BatchId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 延长现有的转储批次固定链接
% 处理转储批处理命令
% PUT /_api/replication/batch/{id}
% 注意：这些调用对用户而言并不有趣。
% 路径参数
%    id（必填）：批次的ID。
% 具有以下属性的JSON对象是必需的：
%    ttl：新批次的生存时间（以秒为单位）
% 使用批次的ID和提供的ttl值来扩展现有转储批次的ttl。
% 如果可以成功扩展批次的ttl，则响应为空。
% 注意：在协调器上，此请求必须具有查询参数DBserver，该参数 必须是DB-Server的ID。相同的请求被同步转发到该DB服务器。如果在Coordinator情况下未绑定此属性，则会出错。
% 返回码
%    204：如果成功扩展了批次的ttl，则返回。
%    400：如果ttl值无效或未找到批次，则返回。
%    405：使用无效的HTTP方法时返回。
% 该转储方法可用于从特定集合中获取数据。由于dump命令的结果可能非常庞大，所以dump可能不会一次返回集合中的所有数据。而是，复制客户端可能会重复调用dump命令，直到没有更多数据要提取为止。dump命令不仅会返回集合中的当前文档，还会返回文档的更新和删除。
% 请注意，dump方法将仅返回文档，日记帐和数据文件中的更新，删除。仅存储在预写日志中的操作将不会返回。为了确保这些操作包含在转储中，必须先清除预写日志。
% 为了获得相同的数据状态，复制客户端应按照提供的顺序使用转储结果的各个部分。
prolongRepBatch(PoolNameOrSocket, BatchId, MapData) ->
   Path = <<"/_api/replication/batch/", (agMiscUtils:toBinary(BatchId))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 返回集合的数据
% 返回一个集合的全部内容
% GET /_api/replication/dump
% 查询参数
%    collection （必填）：要转储的集合的名称或ID。
%    chunkSize（可选）：返回结果的大约最大大小。
%    batchId（必填）：要使用的快照的ID
% 从集合中返回请求范围内的数据。
% 所述CHUNKSIZE查询参数可用于控制结果的大小。必须以字节为单位指定。该CHUNKSIZE值也仅是被兑现。否则，chunkSize值太低可能导致服务器无法仅将一个条目放入结果中并返回它。因此，只有在将条目写入结果后才能查询chunkSize值。如果结果大小大于 chunkSize，则服务器将以与响应中已经存在的条目一样多的条目进行响应。如果结果大小仍小于chunkSize，则如果还有更多数据要返回，则服务器将尝试返回更多数据。
% 如果未指定chunkSize，则将使用某些服务器端默认值。
% 结果的Content-Type是application / x-arango-dump。这是一种易于处理的格式，所有条目都放入响应正文中的单独行中。
% 每行本身是一个JSON对象，至少具有以下属性：
%    tick：操作的tick属性
%    key：文档/边缘的密钥或删除操作中使用的密钥
%    rev：文档/边缘或删除操作的修订版ID
%    data：类型2300和2301的实际文档/边缘数据。完整的文档/边缘数据将被返回，甚至进行更新。
%    type：条目的类型。类型的可能值为：
%       2300：文档插入/更新
%       2301：边缘插入/更新
%       2302：删除文档/边缘
% 注意：调用此方法时，插入和更新之间没有区别。
% 返回码
%    200：如果成功执行了请求并返回了数据，则返回。标头 x-arango-replication-lastincluded设置为最后返回的文档的刻度。
%    204：如果请求已成功执行，但没有可用内容，则返回。在这种情况下，标题x-arango-replication-lastincluded是0。
%    404：找不到集合时返回。
%    405：使用无效的HTTP方法时返回。
%    500：如果组装响应时发生错误，则返回500。
getRepDump(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/dump", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 返回Merkle树以进行收集
% 检索与集合关联的Merkle树
% GET /_api/replication/revisions/tree
% 此基于修订的复制终结点仅适用于RocksDB引擎以及ArangoDB v3.7.0或更高版本中创建的集合。
% 查询参数
%    collection（必填）：要查询的集合的名称或ID。
%    batchId（必填）：要使用的快照的ID
% 返回集合中的Merkle树。
% 结果将是以下格式的JSON / VelocyPack：
%
% {
% version: <Number>,
% branchingFactor: <Number>
% maxDepth: <Number>,
% rangeMin: <String, revision>,
% rangeMax: <String, revision>,
% nodes: [
% { count: <Number>, hash: <String, revision> },
% { count: <Number>, hash: <String, revision> },
% ...
% { count: <Number>, hash: <String, revision> }
% ]
% }
% 目前，只有一个版本1，因此暂时可以忽略此版本。
% 每个<String, revision>值类型都是一个64位值，编码为11个字符的字符串，使用与我们的文档_rev值相同的编码。原因是64位值不一定必须用JavaScript完整表示，因为它会将所有数字作为浮点进行处理，并且只能2^53-1忠实地表示。
% 节点数应对应于具有给定maxDepth和 的完整树branchingFactor。节点按级别顺序树遍历进行布局，因此根节点位于index 0，其子节点位于index ，[1, branchingFactor]依此类推。
% 返回码
% 200：如果成功执行了请求并返回了数据，则返回。
% 401：如果缺少必要的参数，则返回
% 404：找不到集合或快照时返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
% 501：如果使用mmfiles调用或在不支持按版本同步的集合上返回，则返回
getRepTree(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/revisions/tree", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 为集合永久链接重建Merkle树
% 重建与集合关联的Merkle树
% POST /_api/replication/revisions/tree
% 此基于修订的复制终结点仅适用于RocksDB引擎以及ArangoDB v3.7.0或更高版本中创建的集合。
% 查询参数
%    collection（必填）：要查询的集合的名称或ID。
% 重建集合的Merkle树。
% 如果成功，将不会有返回机构。
% 返回码
%    204：如果请求成功执行，则返回。
%    401：如果缺少必要的参数，则返回
%    404：集合或找不到时返回。
%    405：使用无效的HTTP方法时返回。
%    500：如果组装响应时发生错误，则返回500。
%    501：如果使用mmfiles调用或在不支持按版本同步的集合上返回，则返回
resetRepTree(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/revisions/tree", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], undefined).

% 返回要求范围内的修订版ID 永久链接
% 检索请求范围内的文档的修订ID
% PUT /_api/replication/revisions/ranges
% 此基于修订的复制终结点仅适用于RocksDB引擎以及ArangoDB v3.7.0或更高版本中创建的集合。
% 查询参数
%    collection（必填）：要查询的集合的名称或ID。
%    batchId（必填）：要使用的快照的ID
%    resume（可选）：如果先前的请求被截断，则恢复的修订版本
getRepRanges(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/revisions/ranges", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 通过修订返回文档
% 通过修订检索文档
% PUT /_api/replication/revisions/documents
% 此基于修订的复制终结点仅适用于RocksDB引擎以及ArangoDB v3.7.0或更高版本中创建的集合。
% 查询参数
%    collection（必填）：要查询的集合的名称或ID。
%    batchId（必填）：要使用的快照的ID
% 通过修订返回文档
% 请求的主体应为JSON / VelocyPack，并应包含一个字符串编码的修订ID数组：
% [
% <String, revision>,
% <String, revision>,
% ...
% <String, revision>
% ]
% 特别是，修订版本应按其解码值的升序排序。
% 结果将是文档对象的JSON / VelocyPack数组。如果没有对应于特定请求版本的文档，则将在其位置返回一个空对象。
% 如果响应很长，则响应可能会被截断。在这种情况下，响应数组的长度将小于请求数组的长度，并且可以对省略的文档进行后续请求。
% 每个<String, revision>值类型都是一个64位值，编码为11个字符的字符串，使用与我们的文档_rev值相同的编码。原因是64位值不一定必须用JavaScript完整表示，因为它会将所有数字作为浮点进行处理，并且只能2^53-1忠实地表示。
% 返回码
%    200：如果成功执行了请求并返回了数据，则返回。
%    401：如果必要的参数丢失或不正确，则返回
%    404：找不到集合或快照时返回。
%    405：使用无效的HTTP方法时返回。
%    500：如果组装响应时发生错误，则返回500。
%    501：如果使用mmfiles调用或在不支持按版本同步的集合上返回，则返回
getRepDoc(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/revisions/documents", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 从远程端点同步数据永久
% 开始复制
% PUT /_api/replication/sync
% 具有以下属性的JSON对象是必需的：
%    endpoint：要连接的主端点（例如“ tcp：//192.168.173.13：8529”）。
%    database：主数据库上的数据库名称（如果未指定，则默认为本地当前数据库的名称）。
%    username：连接到端点时要使用的可选ArangoDB用户名。
%    password：连接到端点时使用的密码。
%    includeSystem：是否将应用系统收集操作
%    incremental：如果设置为true，则将使用增量同步方法来同步集合中的数据。当集合已经在本地存在并且仅需要从远程端点转移剩余的差异时，此方法很有用。在这种情况下，增量同步可以比完全同步更快。默认值为false，这意味着将传输来自远程集合的完整数据。
%    strictType：用于集合过滤的可选字符串值。指定时，允许的值包括include或exclude。
%    restrictCollections：集合用于在使用的可选阵列 restrictType。如果limitType为include，则仅指定的集合将被同步。如果limitType为exclude，则将同步除指定集合以外的所有集合。
%    initialSyncMaxWaitTime：在获取初始收集数据时，初始同步将等待主服务器响应的最长时间（以秒为单位）。此等待时间可用于控制初始同步将在多长时间后放弃等待响应并失败。如果设置为0，则将忽略此值。
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
%    200：如果请求成功执行，则返回。
%    400：配置不完整或格式错误，返回。
%    405：使用无效的HTTP方法时返回。
%    500：如果同步期间发生错误，则返回。
%    501：在集群中的协调器上调用此操作时返回。
startRepSync(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/sync">>, [], BodyStr).

% 返回集合和索引的集群清单
% 重建集群中的集合和索引的概述
%  GET /_api/replication/clusterInventory
% 查询参数
%     includeSystem（可选）：在结果中包括系统集合。默认值为true。
% 返回群集上可用的集合和索引的数组。
% 响应将是一个JSON对象数组，每个集合一个。每个集合精确地包含两个关键的“参数”和“索引”。此信息来自计划/收藏/ {DB-名称} / * 的机构，只是对指标属性有搬迁到其调整到arangodump的数据格式。
% 返回码
%     200：如果请求成功执行，则返回。
%     405：使用无效的HTTP方法时返回。
%     500：如果组装响应时发生错误，则返回500。
getRepClusterInv(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/clusterInventory", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 复制记录器命令
% 早期版本的ArangoDB允许启动，停止和配置复制记录器。这些命令在ArangoDB 2.2中是多余的，因为所有数据修改操作均写入服务器的预写日志，并且不再由单独的记录器处理。
% 自ArangoDB 2.2以来，剩下的唯一有用的操作是查询记录器的当前状态并获取记录器写入的最新更改。这些操作将从预写日志中返回状态和数据。

% 复制记录器命令
% 返回复制记录器的状态
% GET /_api/replication/logger-state
% 返回服务器复制记录器的当前状态。该状态将包括有关记录器是否正在运行以及有关最后记录的滴答值的信息。此刻度值对于增量获取数据很重要。
% 响应的主体包含具有以下属性的JSON对象：
%     state：当前记录器状态，是带有以下子属性的JSON对象：
%        running：记录仪是否正在运行
%        lastLogTick：记录器记录的最新滴答的滴答值。此值可用于增量获取日志数据。
%        totalEvents：自服务器启动以来记录的事件总数。在记录仪的多次停止和重新启动之间不会重置该值。
%        time：记录器服务器上的当前日期和时间
%     server：具有以下子属性的JSON对象：
%        version：记录器服务器的版本
%        serverId：记录器服务器的ID
%     client：通过连接到记录器的复制客户端返回上一次获取状态。每个客户端均作为具有以下属性的JSON对象返回：
%        syncerId：客户端同步器的ID
%        serverId：客户端的服务器ID
%        lastServedTick：通过logger-follow API 向此客户端提供的最后一个滴答值
%        time：此客户端最后一次调用logger-follow API的日期和时间
% 返回码
%     200：如果可以成功确定记录器状态，则返回。
%     405：使用无效的HTTP方法时返回。
%     500：如果无法确定记录器状态，则返回。
getRepLoggerState(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/logger-state">>, [], undefined).


% 返回日志条目永久链接固定链接
% 从服务器获取日志行
% GET /_api/replication/logger-follow
% 此路由不应再使用。从3.4.0版开始，它被视为已弃用。
% 查询参数
% from（可选）：结果的排他性下界刻度值。
% 到（可选）：结果的包含上限刻度值。
% chunkSize（可选）：返回结果的大约最大大小。
% includeSystem（可选）：在结果中包括系统集合。默认值为true。
% 从服务器的复制日志中返回数据。初始同步数据后，复制客户端可以调用此方法。该方法将从记录器服务器返回所有“最近”的日志条目，并且客户端可以在本地重播和应用这些条目，以使它们进入与记录器服务器相同的数据状态。
% 客户端可以重复调用此方法，以从记录器服务器增量获取所有更改。在这种情况下，它们应提供from值，这样它们将仅自上次获取以来返回日志事件。
% 当不使用from查询参数时，记录器服务器将从其复制日志的开头开始返回日志条目。当使用from 参数时，记录器服务器将仅返回滴答值大于指定的from值的日志条目（注意：滴答值等于from的日志条目将被排除）。增量获取日志数据时，请使用from值。
% 的到查询参数可被用于任选地限制上部结合的结果到一定刻度值。如果使用，结果将仅包含滴答值最大为（包括）到的日志事件。在增量读取中，无需使用to参数。仅在只需要部分变更日志的特殊情况下才有意义。
% 所述CHUNKSIZE查询参数可用于控制结果的大小。必须以字节为单位指定。该CHUNKSIZE值也仅是被兑现。否则，chunkSize值太低可能导致服务器无法仅将一个日志条目放入结果中并将其返回。因此，只有在将日志条目写入结果后才能查询chunkSize值。如果结果大小大于 chunkSize，则服务器将以与响应中已经存在的日志条目一样多的日志条目进行响应。如果结果大小仍小于chunkSize，则如果还有更多数据要返回，则服务器将尝试返回更多数据。
% 如果未指定chunkSize，则将使用某些服务器端默认值。
% 结果的Content-Type是application / x-arango-dump。这是一种易于处理的格式，所有日志事件进入响应正文中的单独行。每个日志事件本身都是一个JSON对象，至少具有以下属性：
% tick：日志事件刻度值
% type：日志事件类型
% 各个日志事件还将具有其他属性，具体取决于事件类型。用于多种事件类型的一些常见属性是：
% cid：事件用于的集合的ID
% tid：包含该事件的交易的ID
% 密钥：文档密钥
% rev：文档修订版ID
% data：原始文档数据
% 有关各个复制事件类型及其数据结构的详细说明，请参见“ 操作类型”。
% 响应还将包含以下HTTP标头：
% x-arango-replication-active：记录器是否处于活动状态。客户可以使用该标志来指示其轮询频率。如果记录器未处于活动状态，并且没有其他可用的复制事件，则客户机中止或进入睡眠状态很长时间，然后稍后重试以检查记录器是否已激活可能是明智的。
% x-arango-replication-lastincluded：结果中最后一个包含值的刻度值。在增量日志取，该值可以被用作从以下请求值。请注意，如果结果为空，则该值为0。客户端不应在下一个请求中将此值用作from的值（否则服务器将从日志的开头再次返回日志事件）。
% x-arango-replication-lasttick：记录服务器已记录的最后一个滴答值（不一定包含在结果中）。通过比较最后的报价和最后包括的报价值，客户端可以大致了解仍有多少事件需要提取。
% x-arango-replication-checkmore：是否已经存在客户端可以立即获取的更多日志数据。如果有更多日志数据可用，则客户端可以再次调用logger-follow，并将其 值调整为从中取出其余日志条目，直到没有更多日志条目为止。
% 如果没有更多的日志数据要获取，则客户端可能决定在再次调用记录器之前先进入睡眠状态。
% 注意：集群中的协调器不支持此方法。
% 返回码
% 200：如果成功执行了请求，则返回该值，并且存在可用于请求范围的日志事件。在这种情况下，响应主体将不会为空。
% 204：如果成功执行了请求，则返回该值，但是对于所请求的范围没有可用的日志事件。在这种情况下，响应主体将为空。
% 400：如果from或to值无效，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
% 501：在集群中的协调器上调用此操作时返回。
% 此路由不应再使用。从3.4.0版开始，它被视为已弃用。 不予实现

% 从服务器返回第一个可用的滴答值
% GET /_api/replication/logger-first-tick
% 返回可以从服务器的复制日志提供的第一个可用的滴答值。在确定某些数据（由刻度值标识）是否仍可用于复制之后，复制客户端可以调用此方法。
% 结果是一个包含属性firstTick的JSON对象。此属性包含服务器的复制日志中可用的最小刻度值。
% 注意：集群中的协调器不支持此方法。
% 返回码
%     200：如果请求成功执行，则返回。
%     405：使用无效的HTTP方法时返回。
%     500：如果组装响应时发生错误，则返回500。
%     501：在集群中的协调器上调用此操作时返回。
getRepLoggerFirstTick(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/logger-first-tick">>, [], undefined).

% 返回日志文件中可用的刻度值范围
% GET /_api/replication/logger-tick-ranges
% 返回所有当前可用的WAL日志文件的刻度值的当前可用范围。刻度值可用于确定某些数据（由刻度值标识）是否仍可用于复制。
% 响应的主体包含一个JSON数组。每个数组成员都是一个描述单个日志文件的对象。每个对象都具有以下属性：
%     datafile：日志文件的名称
%     status：数据文件的状态，以文本形式显示（例如，“密封”，“打开”）
%     tickMin：日志文件中包含的最小刻度值
%     tickMax：日志文件中包含的最大刻度值
% 返回码
%     200：如果刻度范围可以成功确定，则返回。
%     405：使用无效的HTTP方法时返回。
%     500：如果无法确定记录器状态，则返回。
%     501：在集群中的协调器上调用此操作时返回。
getRepLoggerTickRanges(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/logger-tick-ranges">>, [], undefined).

% 复制应用程序命令
% applier命令允许远程启动，停止和查询ArangoDB数据库复制应用程序的状态和配置。

% 复制应用程序命令
% applier命令允许远程启动，停止和查询ArangoDB数据库复制应用程序的状态和配置。
% 获取当前的复制配置
% GET /_api/replication/applier-config
% 查询参数
%    global （可选）：如果设置为true，则返回所有数据库的全局复制应用程序的配置。如果设置为false，则返回所选数据库中复制应用程序的配置（默认）。
% 返回复制应用程序的配置。
% 响应的主体是带有配置的JSON对象。配置中可能存在以下属性：
% 端点：要连接的记录器服务器（例如“ tcp：//192.168.173.13：8529”）。
% database：要连接的数据库的名称（例如“ _system”）。
% username：连接到端点时要使用的可选ArangoDB用户名。
% password：连接到端点时使用的密码。
% maxConnectRetries：应用程序连续进行的最大连接尝试次数。如果应用程序无法通过此尝试次数建立与端点的连接，它将停止运行。
% connectTimeout：尝试连接到端点时的超时（以秒为单位）。该值用于每次连接尝试。
% requestTimeout：对端点的单个请求的超时（以秒为单位）。
% chunkSize：请求的日志传输包的最大大小，该大小在联系端点时使用。
% autoStart：是否在（下一个及之后）服务器上自动启动复制应用程序
% adaptivePolling：复制应用程序是否将使用自适应轮询。
% includeSystem：是否将应用系统收集操作
% autoResync：如果主服务器无法提供从属服务器请求的日志数据，或者在复制开始且找不到滴答值时，从属服务器是否应与主服务器执行全自动重新同步。
% autoResyncRetries：启用和启用自动重新同步后，将连续执行的重新同步重试次数。将其设置为0将有效地禁用autoResync。将其设置为其他值将限制重试的次数。如果重新同步总是失败，这有助于防止无休止的重试。
% initialSyncMaxWaitTime：在获取初始收集数据时，初始同步将等待主服务器响应的最长时间（以秒为单位）。此等待时间可用于控制初始同步将在多长时间后放弃等待响应并失败。即使将autoResync设置为true时，该值也适用于连续复制，因为当主服务器无法提供从属服务器所需的日志数据时，此值可能会重新启动初始同步。如果设置为0，则将忽略此值。
% connectionRetryWaitTime：如果出现连接问题，应用程序在重试连接到主服务器之前将有意空闲的时间（以秒为单位）。如果设置为0，则将忽略此值。
% idleMinWaitTime：如果主服务器已经发送了所有日志数据，那么在从主服务器获取更多日志数据之前，应用程序将有意空闲的最短等待时间（以秒为单位）。该等待时间可用于控制复制应用程序向主服务器发送HTTP日志获取请求的频率，以防主服务器上没有写入活动。如果设置为0，则将忽略此值。
% idleMaxWaitTime：如果主服务器已经发送了所有日志数据并且之前进行了日志获取尝试而没有更多日志数据，那么在从主服务器获取更多日志数据之前，应用程序将有意空闲的最大等待时间（以秒为单位） 。该等待时间可用于控制复制应用程序向主服务器发送HTTP日志获取请求的最大频率，以防主服务器上长时间没有写入活动。仅当选项adaptivePolling设置为true时，才使用此配置值。如果设置为0，则将忽略此值。
% requireFromPresent：如果设置为true，那么复制应用程序将在启动时检查主服务器上是否仍存在从中开始复制或恢复复制的起始时间。否则，将丢失数据。如果 requireFromPresent为true，则复制应用程序将中止并显示相应的错误消息。如果设置为false，那么复制应用程序仍将启动，并忽略数据丢失。
% verbose：如果设置为true，那么将为复制应用程序执行的所有操作发出一条日志行。这仅应用于调试复制问题。
% restrictType：为配置restrictCollections
% restrictCollections：集合的可选阵列包括或排除的基础上，设定restrictType
% 返回码
% 200：如果请求成功执行，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
getRepApplierConfig(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/applier-config">>, [], undefined).

getRepApplierConfig(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/applier-config", QueryBinary/binary>>, [], undefined).

% 设置申请者的配置值
% PUT /_api/replication/applier-config
% 查询参数
% 全局（可选）：如果设置为true，则为所有数据库调整全局复制应用程序的配置。如果设置为false，则调整所选数据库中复制应用程序的配置（默认）。
% 具有以下属性的JSON对象是必需的：
% 端点：要连接的记录器服务器（例如“ tcp：//192.168.173.13：8529”）。必须指定端点。
% database：端点上数据库的名称。如果未指定，则默认为当前本地数据库名称。
% username：连接到端点时要使用的可选ArangoDB用户名。
% password：连接到端点时使用的密码。
% maxConnectRetries：应用程序连续进行的最大连接尝试次数。如果应用程序无法通过此尝试次数建立与端点的连接，它将停止运行。
% connectTimeout：尝试连接到端点时的超时（以秒为单位）。该值用于每次连接尝试。
% requestTimeout：对端点的单个请求的超时（以秒为单位）。
% chunkSize：请求的日志传输包的最大大小，该大小在联系端点时使用。
% autoStart：是否在（下一个及之后）服务器上自动启动复制应用程序
% adaptivePolling：如果设置为true，则复制应用程序将进入睡眠状态，睡眠时间会越来越长，以防端点上的记录器服务器没有更多的复制事件可应用。因此，对于只有很少更改的情况，使用自适应轮询对于减少应用程序和记录器服务器的工作量很有用。不利之处在于，使用自适应轮询时，复制应用程序检测到记录服务器上有新的复制事件的时间可能会更长。
% 将adaptivePolling设置为false将使复制者以固定的时间间隔与记录器服务器联系，而不管记录器服务器是频繁提供更新还是很少提供更新。
% includeSystem：是否将应用系统收集操作
% autoResync：如果主服务器无法提供从属服务器请求的日志数据，或者在复制开始且找不到滴答值时，从属服务器是否应与主服务器执行全自动重新同步。
% autoResyncRetries：启用和启用自动重新同步后，将连续执行的重新同步重试次数。将其设置为0 将有效地禁用autoResync。将其设置为其他值将限制重试的次数。如果重新同步总是失败，这有助于防止无休止的重试。
% initialSyncMaxWaitTime：在获取初始收集数据时，初始同步将等待主服务器响应的最长时间（以秒为单位）。此等待时间可用于控制初始同步将在多长时间后放弃等待响应并失败。即使将autoResync设置为true时，该值也适用于连续复制，因为当主服务器无法提供从属服务器所需的日志数据时，此值可能会重新启动初始同步。如果设置为0，则将忽略此值。
% connectionRetryWaitTime：如果出现连接问题，应用程序在重试连接到主服务器之前将有意空闲的时间（以秒为单位）。如果设置为0，则将忽略此值。
% idleMinWaitTime：如果主服务器已经发送了所有日志数据，那么在从主服务器获取更多日志数据之前，应用程序将有意空闲的最短等待时间（以秒为单位）。该等待时间可用于控制复制应用程序向主服务器发送HTTP日志获取请求的频率，以防主服务器上没有写入活动。如果设置为0，则将忽略此值。
% idleMaxWaitTime：如果主服务器已经发送了所有日志数据并且之前进行了日志获取尝试而没有更多日志数据，那么在从主服务器获取更多日志数据之前，应用程序将有意空闲的最大等待时间（以秒为单位） 。该等待时间可用于控制复制应用程序向主服务器发送HTTP日志获取请求的最大频率，以防主服务器上长时间没有写入活动。仅当选项adaptivePolling设置为true时，才使用此配置值 。如果设置为0，则将忽略此值。
% requireFromPresent：如果设置为true，那么复制应用程序将在启动时检查主服务器上是否仍存在从中开始复制或恢复复制的起始时间。否则，将丢失数据。如果 requireFromPresent为true，则复制应用程序将中止并显示相应的错误消息。如果设置为false，那么复制应用程序仍将启动，并忽略数据丢失。
% verbose：如果设置为true，那么将为复制应用程序执行的所有操作发出一条日志行。这仅应用于调试复制问题。
% restrictType：为配置restrictCollections ; 必须包含或排除
% restrictCollections：集合的阵列要包括或排除的基础上，设定restrictType
% 设置复制应用程序的配置。仅当应用程序未运行时才能更改配置。更新后的配置将立即保存，但仅在应用程序的下一次启动时生效。
% 如果成功，响应的主体是具有更新配置的JSON对象。
% 返回码
% 200：如果请求成功执行，则返回。
% 400：如果配置不完整或格式错误，或者当前正在运行复制应用程序，则返回400。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
setRepApplierConfig(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/applier-config">>, [], BodyStr).

setRepApplierConfig(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/applier-config", QueryBinary/binary>>, [], BodyStr).

% 启动复制应用程序
% PUT /_api/replication/applier-start
% 查询参数
% 全局（可选）：如果设置为true，则为所有数据库启动全局复制应用程序。如果设置为false，则在所选数据库中启动复制应用程序（默认）。
% from（可选）：从其开始应用的远程lastLogTick值。如果未指定，则使用上一次申请者运行中最后保存的刻度。如果没有保存以前的申请者状态，则申请者将从记录器服务器日志的开头开始。
% 启动复制程序。如果复制应用程序已经在运行，它将立即返回。
% 如果复制应用程序尚未运行，则将检查该应用程序配置，如果复制程序已完成，则将在后台线程中启动该应用程序。这意味着即使应用程序在运行时遇到任何错误，也不会在对此方法的响应中报告这些错误。
% 要在启动应用程序后检测复制应用程序错误，请改用 / _api / replication / applier-state API。
% 返回码
% 200：如果请求成功执行，则返回。
% 400：如果复制申请人未完全配置或配置无效，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
startRepApplier(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/applier-start">>, [], undefined).

startRepApplier(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/replication/applier-start", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 停止复制
% PUT /_api/replication/applier-stop
% 查询参数
% 全局（可选）：如果设置为true，则停止所有数据库的全局复制应用程序。如果设置为false，则在所选数据库中停止复制应用程序（默认）。
% 停止复制程序。如果复制应用程序未运行，它将立即返回。
% 返回码
% 200：如果请求成功执行，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
stopRepApplier(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/applier-stop">>, [], undefined).

stopRepApplier(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/applier-stop", QueryBinary/binary>>, [], undefined).

% 输出复制的当前状态
% GET /_api/replication/applier-state
% 查询参数
% 全局（可选）：如果设置为true，则返回所有数据库的全局复制应用程序的状态。如果设置为false，则返回所选数据库中复制应用程序的状态（默认）。
% 返回复制应用程序的状态，无论该应用程序当前是否正在运行。
% 响应是具有以下属性的JSON对象：
% state：具有以下子属性的JSON对象：
% running：申请者是否处于活动状态并正在运行
% lastAppliedContinuousTick：应用者已应用的连续复制日志中的最后一个滴答值。
% lastProcessedContinuousTick：申请人已处理的连续复制日志中的最后一个滴答值。
% 通常，最后应用和最后处理的滴答值应相同。对于事务操作，复制应用程序将首先处理传入的日志事件，然后再应用它们，因此处理的滴答值可能会高于所应用的滴答值。在申请人遇到事务的事务提交日志事件之前，情况将一直如此。
% lastAvailableContinuousTick：远程服务器可以为所有数据库提供的最后一个滴答值。
% ticksBehind：仅当应用程序当前正在运行时，此属性才存在。它将提供申请者已应用/看到的内容与远程服务器提供的最后一个日志滴答值之间的日志滴答数。如果该值为零，则两个服务器都处于同步状态。如果该值不为零，则远程服务器具有尚未提取和处理应用程序的其他数据，或者远程服务器可能具有其他不适用于该应用程序的数据。
% 客户端应用程序可以使用它来确定大致距离远程服务器后面的应用程序，并且可以定期检查该值是增加（应用程序落后）还是减小（应用程序赶上）。
% 请注意，由于远程服务器将仅为其所有数据库保留最后一个日志滴答值，但是复制可能只限于应用程序中的某些数据库，因此使用全局应用程序时，此值更有意义。此外，由于对由于复制配置而无法复制的系统集合的写入，远程服务器提供的最后一个日志滴答可能会增加。因此，在某些情况下，报告的值可能会夸大现实。
% time：应用服务器上的时间。
% totalRequests：应用程序向端点发出的请求总数。
% totalFailedConnects：应用程序进行的失败连接尝试总数。
% totalEvents：应用程序已处理的日志事件总数。
% totalOperationsExcluded：由于restrictCollections而被排除的日志事件总数。
% progress：一个JSON对象，其中包含有关复制应用程序进度的详细信息。如果有报告进度，则它包含以下子属性：
% 消息：进度的文字描述
% 时间：记录进度的日期和时间
% failedConnects：当前失败的连接尝试次数
% lastError：一个JSON对象，其中包含有关应用程序上发生的最后一个错误的详细信息。如果发生错误，它包含以下子属性：
% errorNum：数字错误代码
% errorMessage：文本错误描述
% 时间：发生错误的日​​期和时间
% 如果没有发生错误，lastError将为空。
% server：具有以下子属性的JSON对象：
% version：应用服务器的版本
% serverId：应用服务器的ID
% 端点：应用程序连接到的端点（如果应用程序处于活动状态）或将连接到端点（如果应用程序当前处于非活动状态）
% database：应用程序连接到的数据库的名称（如果应用程序处于活动状态）或将连接（如果应用程序当前处于非活动状态）
% 请注意，所有返回的“刻度”值都没有特定的单位。刻度值仅在相互比较时才有意义。较高的报价值表示比较低的报价值“时间更晚”。
% 返回码
% 200：如果请求成功执行，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
getRepApplierState(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/applier-state">>, [], undefined).

getRepApplierState(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/applier-state", QueryBinary/binary>>, [], undefined).

% 将角色更改为奴隶
% PUT /_api/replication/make-slave
% 具有以下属性的JSON对象是必需的：
% 端点：要连接的主端点（例如“ tcp：//192.168.173.13：8529”）。
% database：主数据库上的数据库名称（如果未指定，则默认为本地当前数据库的名称）。
% username：连接到主服务器时使用的可选ArangoDB用户名。
% password：连接到主服务器时使用的密码。
% includeSystem：是否将应用系统收集操作
% strictType：用于集合过滤的可选字符串值。指定时，允许的值包括include或exclude。
% restrictCollections：集合用于在使用的可选阵列restrictType。如果limitType为include，则仅指定的集合将被同步。如果limitType为exclude，则将同步除指定集合以外的所有集合。
% maxConnectRetries：应用程序连续进行的最大连接尝试次数。如果应用程序无法通过此尝试次数建立与端点的连接，它将停止运行。
% connectTimeout：尝试连接到端点时的超时（以秒为单位）。该值用于每次连接尝试。
% requestTimeout：对端点的单个请求的超时（以秒为单位）。
% chunkSize：请求的日志传输包的最大大小，该大小在联系端点时使用。
% adaptivePolling：复制应用程序是否将使用自适应轮询。
% autoResync：如果主服务器无法提供从属服务器请求的日志数据，或者在复制开始且找不到刻度值时，从属服务器是否应与主服务器执行自动重新同步。
% autoResyncRetries：启用和启用自动重新同步后，将连续执行的重新同步重试次数。将其设置为0将有效地禁用autoResync。将其设置为其他值将限制重试的次数。如果重新同步总是失败，这有助于防止无休止的重试。
% initialSyncMaxWaitTime：在获取初始收集数据时，初始同步将等待主服务器响应的最长时间（以秒为单位）。此等待时间可用于控制初始同步将在多长时间后放弃等待响应并失败。即使将autoResync设置为true时，该值也适用于连续复制，因为当主服务器无法提供从属服务器所需的日志数据时，此值可能会重新启动初始同步。如果设置为0，则将忽略此值。
% connectionRetryWaitTime：如果出现连接问题，应用程序在重试连接到主服务器之前将有意空闲的时间（以秒为单位）。如果设置为0，则将忽略此值。
% idleMinWaitTime：如果主服务器已经发送了所有日志数据，那么在从主服务器获取更多日志数据之前，应用程序将有意空闲的最短等待时间（以秒为单位）。该等待时间可用于控制复制应用程序向主服务器发送HTTP日志获取请求的频率，以防主服务器上没有写入活动。如果设置为0，则将忽略此值。
% idleMaxWaitTime：如果主服务器已经发送了所有日志数据并且之前进行了日志获取尝试而没有更多日志数据，那么在从主服务器获取更多日志数据之前，应用程序将有意空闲的最大等待时间（以秒为单位） 。该等待时间可用于控制复制应用程序向主服务器发送HTTP日志获取请求的最大频率，以防主服务器上长时间没有写入活动。仅当选项adaptivePolling设置为true时，才使用此配置值 。如果设置为0，则将忽略此值。
% requireFromPresent：如果设置为true，则复制应用程序将在其连续复制开始时检查转储阶段的开始时间是否仍存在于主服务器上。否则，将丢失数据。如果 requireFromPresent为true，则复制应用程序将中止并显示相应的错误消息。如果设置为false，那么复制应用程序仍将启动，并忽略数据丢失。
% verbose：如果设置为true，那么将为复制应用程序执行的所有操作发出一条日志行。这仅应用于调试复制问题。
% 启动从远程端点到本地ArangoDB数据库的完整数据同步，然后启动连续复制。该操作在每个数据库级别上进行。
% 同步之前，将删除所有本地数据库数据。
% 如果成功，响应的主体是具有以下属性的JSON对象：
% state：具有以下子属性的JSON对象：
% running：申请者是否处于活动状态并正在运行
% lastAppliedContinuousTick：应用者已应用的连续复制日志中的最后一个滴答值。
% lastProcessedContinuousTick：申请人已处理的连续复制日志中的最后一个滴答值。
% 通常，最后应用和最后处理的滴答值应相同。对于事务操作，复制应用程序将首先处理传入的日志事件，然后再应用它们，因此处理的滴答值可能会高于所应用的滴答值。在申请人遇到事务的事务提交日志事件之前，情况将一直如此。
% lastAvailableContinuousTick：远程服务器可以提供的最后一个滴答值。
% ticksBehind：仅当应用程序当前正在运行时，此属性才存在。它将提供申请者已应用/看到的内容与远程服务器提供的最后一个日志滴答值之间的日志滴答数。如果该值为零，则两个服务器都处于同步状态。如果该值不为零，则远程服务器具有尚未提取和处理应用程序的其他数据，或者远程服务器可能具有其他不适用于该应用程序的数据。
% 客户端应用程序可以使用它来确定大致距离远程服务器后面的应用程序，并且可以定期检查该值是增加（应用程序落后）还是减小（应用程序赶上）。
% 请注意，由于远程服务器将仅为其所有数据库保留最后一个日志滴答值，但是复制可能只限于应用程序中的某些数据库，因此使用全局应用程序时，此值更有意义。此外，由于对由于复制配置而无法复制的系统集合的写入，远程服务器提供的最后一个日志滴答可能会增加。因此，在某些情况下，报告的值可能会夸大现实。
% time：应用服务器上的时间。
% totalRequests：应用程序向端点发出的请求总数。
% totalFailedConnects：应用程序进行的失败连接尝试总数。
% totalEvents：应用程序已处理的日志事件总数。
% totalOperationsExcluded：由于restrictCollections而被排除的日志事件总数。
% progress：一个JSON对象，其中包含有关复制应用程序进度的详细信息。如果有报告进度，则它包含以下子属性：
% 消息：进度的文字描述
% 时间：记录进度的日期和时间
% failedConnects：当前失败的连接尝试次数
% lastError：一个JSON对象，其中包含有关应用程序上发生的最后一个错误的详细信息。如果发生错误，它包含以下子属性：
% errorNum：数字错误代码
% errorMessage：文本错误描述
% 时间：发生错误的日​​期和时间
% 如果没有发生错误，lastError将为空。
% server：具有以下子属性的JSON对象：
% version：应用服务器的版本
% serverId：应用服务器的ID
% 端点：应用程序连接到的端点（如果应用程序处于活动状态）或将连接到端点（如果应用程序当前处于非活动状态）
% database：应用程序连接到的数据库的名称（如果应用程序处于活动状态）或将连接（如果应用程序当前处于非活动状态）
% 请注意，所有返回的“刻度”值都没有特定的单位。刻度值仅在相互比较时才有意义。较高的报价值表示比较低的报价值“时间更晚”。
% 警告：调用此方法会将来自远程主服务器上的集合中的数据同步到本地ArangoDB数据库。本地集合中的所有数据将被清除，并替换为主数据库中的数据。
% 请谨慎使用！
% 还请记住，此命令可能需要很长时间才能完成并返回。这是因为它将首先与主机进行完全的数据同步，这将花费与数据量大致成比例的时间。
% 注意：集群中的协调器不支持此方法。
% 返回码
% 200：如果请求成功执行，则返回。
% 400：配置不完整或格式错误，返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果在同步过程中或开始连续复制时发生错误，则返回。
% 501：在集群中的协调器上调用此操作时返回。
changeRepMakeSlave(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/replication/make-slave">>, [], BodyStr).

%其他复制命令
%返回服务器ID
%获取此服务器的唯一标识符
%GET /_api/replication/server-id
%返回服务器ID。其他复制API方法也返回该ID，该方法是确定服务器ID的简便方法。
%响应的主体是带有属性serverId的JSON对象。服务器ID作为字符串返回。
%返回码
%200：如果请求成功执行，则返回。
%405：使用无效的HTTP方法时返回。
%500：如果组装响应时发生错误，则返回500。
getRepServerId(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/replication/server-id">>, [], undefined).

% WAL 固定链接操作中可用的返回刻度范围
% 返回预写日志中可用的刻度线范围
% GET /_api/wal/range
% 返回所有WAL文件的当前刻度值范围。刻度值可用于确定某些数据（由刻度值标识）是否仍可用于复制。
% 响应的主体包含一个JSON对象。
% tickMin：可用的最小刻度
% tickMax：可用的最大刻度
% time：服务器时间，以字符串形式，格式为“ YYYY-MM-DDTHH：MM：SSZ”
% server：具有字段version和serverId的对象
% 返回码
% 200：如果刻度范围可以成功确定，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果无法确定服务器操作状态，则返回。
% 501：在集群中的协调器上调用此操作时返回。
getWalRange(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/wal/range">>, [], undefined).

% 返回最后一个可用的刻度值固定链接
% 返回最后一个可用的刻度值
% GET /_api/wal/lastTick
% 返回可以从服务器的复制日志提供的最后一个可用的滴答值。这对应于最新成功操作的滴答声。
% 结果是一个包含属性tick，time和server的JSON对象。
% tick：包含最后可用的刻度，时间
% time：服务器时间，以字符串形式，格式为“ YYYY-MM-DDTHH：MM：SSZ”
% server：具有字段version和serverId的对象
% 注意：集群中的协调器不支持此方法。
% 返回码
% 200：如果请求成功执行，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
% 501：在集群中的协调器上调用此操作时返回。
getWalLastTick(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/wal/lastTick">>, [], undefined).

% 获取最近的操作
% GET /_api/wal/tail
% 查询参数
% 全局（可选）：是否应包括所有数据库的操作。设置为false时， 仅包括当前数据库的操作。值true仅在_system数据库上有效。默认值为false。
% from（可选）：结果的排他性下界刻度值。在连续调用此API时，应将其设置为x-arango-replication-lastincluded标头返回的值（除非标头包含0）。
% 到（可选）：结果的包含上限刻度值。
% lastScanned（可选）：应该设置为x-arango-replication-lastscanned标头的值，或者第一次尝试设置为0。这使rocksdb引擎可以通过多个响应分解大型事务。
% chunkSize（可选）：返回结果的大约最大大小。
% syncerId（可选）：用于尾随结果的客户端ID。服务器将使用它来保留操作，直到客户端取回它们为止。必须为正整数。 请注意，必须使用this或serverId来获取使用rocksdb存储引擎读取的所有操作。
% serverId（可选）：客户端计算机的ID。如果未设置syncerId，则服务器将使用它来保留操作，直到客户端获取它们为止。必须为正整数。 请注意，必须使用this或syncerId才能有机会读取rocksdb存储引擎的所有操作。
% clientInfo（可选）：客户端的简短描述，仅用于提供信息。
% barrierId（可选）：用于保留WAL条目的障碍的ID。请注意，这仅对于MMFiles存储引擎是必需的
% 从服务器的预写日志（也称为复制日志）中返回数据。初始同步数据后，复制客户端可以调用此方法。该方法将从服务器返回所有“最近”记录的操作。客户端可以在本地重播和应用这些操作，以便它们获得与服务器相同的数据状态。
% 客户端可以重复调用此方法以从服务器增量获取所有更改。在这种情况下，它们应提供from值，这样它们将仅自上次获取以来返回日志事件。
% 当不使用from查询参数时，服务器将从其复制日志的开头开始返回日志条目。当使用from 参数时，服务器将仅返回滴答值大于指定的from值的日志条目（注意：滴答值等于from的日志条目将被排除）。增量获取日志数据时，请使用from值。
% 的到查询参数可被用于任选地限制上部结合的结果到一定刻度值。如果使用，结果将仅包含滴答值最大为（包括）到的日志事件。在增量读取中，无需使用to参数。仅在只需要部分变更日志的特殊情况下才有意义。
% 所述CHUNKSIZE查询参数可用于控制结果的大小。必须以字节为单位指定。该CHUNKSIZE值也仅是被兑现。否则，chunkSize值太低可能导致服务器无法仅将一个日志条目放入结果中并将其返回。因此，只有在将日志条目写入结果后才能查询chunkSize值。如果结果大小大于 chunkSize，则服务器将以与响应中已经存在的日志条目一样多的日志条目进行响应。如果结果大小仍小于chunkSize，则如果还有更多数据要返回，则服务器将尝试返回更多数据。
% 如果未指定chunkSize，则将使用某些服务器端默认值。
% 结果的Content-Type是application / x-arango-dump。这是一种易于处理的格式，所有日志事件进入响应正文中的单独行。每个日志事件本身都是一个JSON对象，至少具有以下属性：
% tick：日志事件刻度值
% type：日志事件类型
% 各个日志事件还将具有其他属性，具体取决于事件类型。用于多种事件类型的一些常见属性是：
% cuid：事件所属的View或collection的全局唯一ID
% db：事件用于的数据库名称
% tid：包含该事件的交易的ID
% data：原始文档数据
% 有关各个复制事件类型及其数据结构的详细说明，请参见“ 操作类型”。
% 响应还将包含以下HTTP标头：
% x-arango-replication-active：记录器是否处于活动状态。客户可以使用该标志来指示其轮询频率。如果记录器未处于活动状态，并且没有其他可用的复制事件，则客户机中止或进入睡眠状态很长时间，然后稍后重试以检查记录器是否已激活可能是明智的。
% x-arango-replication-lastincluded：结果中最后一个包含值的刻度值。在增量日志取，该值可以被用作从以下请求值。请注意，如果结果为空，则该值为0。客户端不应在下一个请求中将此值用作from的值（否则服务器将从日志的开头再次返回日志事件）。
% x-arango-replication-lastscanned：计算操作日志时服务器扫描的最后一个滴答声。这可能包括服务器由于各种原因未退还给您的操作（即，该值已被过滤或跳过）。您可以在lastScanned标头中使用此值，以使rocksdb引擎分解多个响应上的请求。
% x-arango-replication-lasttick：服务器已在其预写日志中记录的最后一个滴答值（不一定包含在结果中）。通过比较最后的报价和最后包括的报价值，客户端可以大致了解仍有多少事件需要提取。
% 如果服务器从from参数中指定的刻度开始返回所有刻度值，则x-arango-replication-frompresent设置为true。如果将此设置为false，则服务器不再具有这些操作，并且客户端可能错过了操作。
% x-arango-replication-checkmore：是否已经存在客户端可以立即获取的更多日志数据。如果有更多日志数据可用，则客户端可以再次调用logger-follow，并将其 值调整为从中取出其余日志条目，直到没有更多日志条目为止。
% 如果没有更多的日志数据要获取，则客户端可能决定在再次调用记录器之前先进入睡眠状态。
% 注意：集群中的协调器不支持此方法。
% 返回码
% 200：如果成功执行了请求，则返回该值，并且存在可用于请求范围的日志事件。在这种情况下，响应主体将不会为空。
% 204：如果成功执行了请求，则返回该值，但是对于所请求的范围没有可用的日志事件。在这种情况下，响应主体将为空。
% 400：如果from或to值无效，则返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果组装响应时发生错误，则返回500。
% 501：在集群中的协调器上调用此操作时返回。
getWalTail(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/wal/tail">>, [], undefined).

getWalTail(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/wal/tail", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

