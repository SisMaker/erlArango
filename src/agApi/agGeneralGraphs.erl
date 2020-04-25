-module(agGeneralGraphs).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/gharial.html

% 通用图
% 本章介绍了多集合图形模块的REST接口。它允许您定义分布在多个边缘和文档集合中的图形。无需在查询中包括引用的集合，此模块将为您处理。

% 管理图表
% 列出图形模块已知的所有图形。
% GET /_api/gharial
% 列出此数据库中存储的所有图形。
% 如果模块可用并且可以列出图形，则返回HTTP 200。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% 图表：
% graph：有关新创建的图的信息
% name：图形名称
% edgeDefinitions：图形关系的定义数组。每个都有以下类型：
% orphanCollections：其他顶点集合的数组。这些集合中的文档在此图中没有边。
% numberOfShards：为图中的每个新集合创建的分片数。
% _id：此图的内部ID值。
% _rev：此图的修订版。可用于确保不覆盖对该图的并发修改。
% ReplicationFactor：图形中每个新集合使用的复制因子。
% isSmart：标记图形是否为SmartGraph（仅限企业版）。
% smartGraphAttribute：智能图例中的分片属性名称（仅企业版）
graphList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/gharial">>, [], undefined).

% 创建一个图
% 在图形模块中创建一个新图形。
% POST /_api/gharial
% 查询参数
%    waitForSync（可选）：定义请求是否应该等待，直到所有内容都同步到光盘。将更改成功响应代码。
% 具有以下属性的JSON对象是必需的：
% name：图形名称。
% edgeDefinitions：图形关系的定义数组。每个都有以下类型：
%      collection : 边集合名
%      from: from 顶点集合名数组、
%      to :  to  顶点集合名数组
% orphanCollections：其他顶点集合的数组。这些集合中的文档在此图中没有边。
% isSmart：定义创建的图形是否应该是智能的。这仅在企业版中有效。
% options：一个JSON对象，用于定义用于在此图中创建集合的选项。它可以包含以下属性：
%    smartGraphAttribute：仅在企业版中有效，如果isSmart为true，则为必需。用于聪明地分割图的顶点的属性名称。此SmartGraph中的每个顶点都必须具有此属性。以后无法修改。
%    numberOfShards：此图中每个集合使用的分片数。以后无法修改。
%    replicationFactor：最初为此图创建集合时使用的复制因子。可以设置为"satellite"创建SatelliteGraph，将忽略 numberOfShards，minReplicationFactor和writeConcern。
%    writeConcern：对图中的新集合进行关注。它确定在不同的DB服务器上同步每个分片需要多少个副本。如果集群中的副本数量很少，那么分片将拒绝写入。但是，具有足够最新副本的分片写入将同时成功。writeConcern的值 不能大于ReplicationFactor。（仅集群）
% 图的创建需要图的名称和其边缘的定义。 另请参见边缘定义。
% 如果可以创建图形并且为_graphs集合启用了waitForSync 或在请求中指定了图形，则返回HTTP 201。响应主体包含已存储的图形配置。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关新创建的图的信息。
% 如果可以创建图形并且为该_graphs集合禁用了waitForSync，并且未在请求中给出，则返回HTTP 202。响应主体包含已存储的图形配置。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关新创建的图的信息。
% HTTP 400如果请求的格式错误，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了创建图，您至少需要具有以下特权：
% Administrate 访问数据库。
% Read Only
% 访问此图中使用的每个集合。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 409如果存储图形时发生冲突，则返回。如果已经存储了具有该名称的图形，或者存在一个边缘定义具有相同的边缘集合但在任何其他图形中使用了不同的签名，则可能会发生这种情况 。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
newGraph(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/gharial">>, [], BodyStr).

newGraph(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 获取图表
% GET /_api/gharial/{graph}
% 路径参数
% 图（必填）：图的名称。
% 选择给定图的信息。将返回边缘定义以及孤立集合。或如果图形不存在，则返回404。
% HTTP 200如果可以找到该图，则返回该图。结果将具有以下格式：
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关新创建的图的信息
% HTTP 404如果找不到具有该名称的图形，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
getGraph(PoolNameOrSocket, GraphName) ->
   Path = <<"/_api/gharial/", GraphName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 删除现有图
% DELETE /_api/gharial/{graph}
% 路径参数
% 图（必填）：图的名称。
% 查询参数
%     dropCollections（可选）：也删除该图的集合。只有在其他图形中未使用集合时，集合才会被删除。
% 按名称删除现有图形对象。（可选）也可以删除其他图未使用的所有集合。
% 201：如果可以删除该图并为该_graphs集合启用了waitForSync ，或者在请求中给出了该图，则返回该值。
% 202：如果可以删除该图并且为该_graphs集合禁用了waitForSync，并且未在请求中给出，则返回该值。
% HTTP 403如果您的用户权限不足，则返回。为了删除图，您至少需要具有以下特权：
% Administrate 访问数据库。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 404如果找不到具有该名称的图形，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
delGraph(PoolNameOrSocket, GraphName) ->
   Path = <<"/_api/gharial/", GraphName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delGraph(PoolNameOrSocket, GraphName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 列出此图中使用的所有顶点集合。
% GET /_api/gharial/{graph}/vertex
% 路径参数
% graph（必填）：图的名称。
% 列出此图中的所有顶点集合。
% 如果可以列出集合，则返回HTTP 200。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% collections：此图中所有顶点集合的列表。在edgeDefinitions和孤儿中包括集合。
% HTTP 404如果找不到具有该名称的图形，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
vertexCollList(PoolNameOrSocket, GraphName) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex">>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 向图中添加一个额外的顶点集合。
% POST /_api/gharial/{graph}/vertex
% 路径参数
% graph（必填）：图的名称。
% 将顶点集合添加到图形的孤立集合中。如果该集合不存在，则将创建它。
% 如果可以创建集合并为该_graphs集合启用waitForSync 或在请求中指定了HTTP，则返回HTTP 201。响应主体包含已存储的图形配置。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关修改后的图的信息。
% 如果可以创建集合并且为该_graphs集合禁用了waitForSync 或在请求中指定了HTTP，则返回HTTP 202。响应主体包含已存储的图形配置。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关新创建的图的信息
% HTTP 400如果请求的格式无效，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了修改图，您至少需要具有以下特权：
% Administrate 访问数据库。
% Read Only 访问此图中使用的每个集合。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 404如果找不到具有该名称的图形，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。

%% MapData = #{"collection" => "otherVertices"}
addVertexColl(PoolNameOrSocket, GraphName, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex">>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 从图形中删除额外顶点集合。
% DELETE /_api/gharial/{graph}/vertex/{collection}
% 路径参数
% graph（必填）：图的名称。
% collection（必填）：顶点集合的名称。
% 查询参数
% dropCollection（可选）：也删除集合。只有在其他图形中未使用集合时，集合才会被删除。
% 如果未在其他任何图中使用顶点集合，则从图中删除顶点集合，并有选择地删除该集合。如果边缘定义中不再使用这些顶点集合，则只能删除不再属于这些定义的顶点集合。
% HTTP 200如果成功从图形中删除了顶点集合并且waitForSync为true，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关新创建的图的信息
% HTTP 202如果请求成功，但waitForSync为false，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% graph：有关新创建的图的信息
% HTTP 400如果顶点集合仍在边缘定义中使用，则返回。在这种情况下，它仍不能从图形中删除，必须先从边定义中删除。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了删除顶点，您至少需要具有以下特权：
% Administrate 访问数据库。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 404如果找不到具有该名称的图形，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
delVertexColl(PoolNameOrSocket, GraphName, CollName) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delVertexColl(PoolNameOrSocket, GraphName, CollName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 列出所有边缘定义
% GET /_api/gharial/{graph}/edge
% 路径参数
% graph（必填）：图的名称。
% 列出此图中的所有边集合。
% 如果可以列出边缘定义，则返回HTTP 200。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% collections：此图中所有顶点集合的列表。在edgeDefinitions和孤儿中包括集合。
% HTTP 404如果找不到具有该名称的图形，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
edgeDefList(PoolNameOrSocket, GraphName) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge">>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 向图添加新的边定义
% POST /_api/gharial/{graph}/edge
% 路径参数
% graph（必填）：图的名称。
% 具有以下属性的JSON对象是必需的：
%     collection：要使用的边缘集合的名称。
%     from：一个或多个可以包含源顶点的顶点集合。
%     to：一个或多个可以包含目标顶点的顶点集合。
% 向图形添加其他边定义。
% 这个边缘清晰度必须包含一个集合，并且每个的阵列从和到顶点集合。仅当未在任何其他图形中使用该定义或将其与完全相同的定义一起使用时，才可以添加边定义。在一个图中不可能存储从“ v1”到“ v2”的定义“ e”，而在另一张图中存储从“ v2”到“ v1”的定义“ e”。
% HTTP 201如果可以成功添加定义并且为_graphs集合启用了waitForSync，则返回。响应主体包含已存储的图形配置。
%    error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%    code：响应代码。
%    graph：有关修改后的图的信息。
% HTTP 202如果可以成功添加定义并且对该_graphs集合禁用了waitForSync，则返回。响应主体包含已存储的图形配置。
%    error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%    code：响应代码。
%    graph：有关修改后的图的信息。
% HTTP 400如果无法添加定义，则返回。这可能是因为其格式错误，或者是在其他具有不同签名的图形中使用了该定义。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了修改图，您至少需要具有以下特权：
%    Administrate 访问数据库。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% HTTP 404如果找不到具有该名称的图形，则返回。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
addEdgeDef(PoolNameOrSocket, GraphName, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge">>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 替换现有的边缘定义
% PUT /_api/gharial/{graph}/edge/{definition}#definition
% 路径参数
%    graph（必填）：图的名称。
%    definition（必填）：定义中使用的边集合的名称。
%
% 查询参数
%    waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%    dropCollections（可选）：也删除集合。只有在其他图形中未使用集合时，集合才会被删除。
% 具有以下属性的JSON对象是必需的：
%    collection：要使用的边缘集合的名称。
%    from：一个或多个可以包含源顶点的顶点集合。
%    to：一个或多个可以包含目标顶点的顶点集合。
% 更改一个特定的边定义。这将修改数据库中所有已知图中所有出现的该定义。
% HTTP 201如果请求成功并且waitForSync为true，则返回。
%    error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%    code：响应代码。
%    graph：有关修改后的图的信息。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%    error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%    code：响应代码。
%    graph：有关修改后的图的信息。
% HTTP 400如果在图形中未找到具有此名称的边定义，或者新定义的格式不正确且无法使用，则返回。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了删除顶点，您至少需要具有以下特权：
%    Administrate 访问数据库。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% HTTP 404如果找不到具有该名称的图形，则返回。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% EdgeDefName 名字 要在定义列表中存在 MapData中的collection 也要存在 只是替换 from to 中的集合
replaceEdgeDef(PoolNameOrSocket, GraphName, EdgeDefName, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", EdgeDefName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceEdgeDef(PoolNameOrSocket, GraphName, EdgeDefName, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", EdgeDefName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 从图形中删除边缘定义
% DELETE /_api/gharial/{graph}/edge/{definition}#definition
% 路径参数
%     graph（必填）：图的名称。
%     definition（必填）：定义中使用的边集合的名称。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     dropCollections（可选）：也删除集合。只有在其他图形中未使用集合时，集合才会被删除。
% 从图形中删除一个边缘定义。这只会删除边缘集合，顶点集合保持不变，并且仍可以在查询中使用。
% HTTP 201如果可以从图形中删除边缘定义并且waitForSync为true，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     graph：有关修改后的图的信息。
% HTTP 202如果可以从图形中删除边缘定义并且waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     graph：有关修改后的图的信息。
% HTTP 403如果您的用户权限不足，则返回。为了删除顶点，您至少需要具有以下特权：
%     Administrate 访问数据库。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 404如果找不到该名称的图形，或者在图形中找不到此名称的边定义，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
delEdgeDef(PoolNameOrSocket, GraphName, EdgeDefName) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", EdgeDefName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delEdgeDef(PoolNameOrSocket, GraphName, EdgeDefName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", EdgeDefName/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 处理顶点

% 创建一个新顶点
% POST /_api/gharial/{graph}/vertex/{collection}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必填）：顶点应插入的顶点集合的名称。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     returnNew（可选）：定义响应是否应包含文档的完整新版本。
% 请求正文（对象）
% 主体必须是要存储的JSON对象。
% 将顶点添加到给定的集合。
% HTTP 201如果可以添加顶点并且waitForSync为true，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     vertex：顶点的内部属性。
%     new：完整的新编写的顶点文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     vertex：存储顶点时生成的内部属性。不包括请求正文中给定的任何属性。
%     new：完整的新编写的顶点文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
% HTTP 403如果您的用户权限不足，则返回。为了将顶点插入到图中，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 404如果找不到具有该名称的图形，则返回。或者，如果找到了图，但是此集合不是图的一部分。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
newVertex(PoolNameOrSocket, GraphName, CollName, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

newVertex(PoolNameOrSocket, GraphName, CollName, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 获取现有顶点
% GET /_api/gharial/{graph}/vertex/{collection}/{vertex}
% 路径参数
%    graph（必填）：图的名称。
%    collection（必需）：顶点所属的顶点集合的名称。
%    vertex（必填）：顶点的_key属性。
% 查询参数
%    rev（可选）：必须包含修订。如果设置了此选项，则仅在具有此修订版本的情况下才返回文档。另请参见if-match标头作为替代方法。
% 标头参数
%    if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则返回文档。否则，返回HTTP 412。或者，您可以在查询参数rev中提供Etag 。
%    if-none-match（可选）：如果给出了“ If-None-Match”标头，则它必须恰好包含一个Etag。仅当文档的版本与给定的Etag不同时，才返回文档。否则，返回HTTP 304。
% 从给定的集合中获取一个顶点。
% HTTP 200如果可以找到顶点，则返回。
%    error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%    code：响应代码。
%    vertex：完整的顶点。
% HTTP 304如果给出了if-none-match标头，并且当前存储的顶点仍具有此修订值，则返回。因此，在上一次调用者获取顶点之间没有更新。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了更新图中的顶点，您至少需要具有以下特权：
%    Read Only 访问数据库。
%    Read Only 访问给定的集合。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%    找不到具有该名称的图。
%    该集合不属于图形。
%    顶点不存在。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
%    error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%    code：响应代码。
%    errorNum：发生错误的ArangoDB错误号。
%    errorMessage：为此错误创建的消息。
getVertex(PoolNameOrSocket, GraphName, CollName, VertexKey) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, Headers, undefined).

% 更新现有顶点
% PATCH /_api/gharial/{graph}/vertex/{collection}/{vertex}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必需）：顶点所属的顶点集合的名称。
%     vertex（必填）：顶点的_key属性。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     keepNull（可选）：定义是否应存储设置为null的值。默认情况下（true），给定的documents（s）属性将设置为null。如果此参数为false，则将属性从文档中删除。
%     returnOld（可选）：定义是否应在响应对象内返回已删除文档的表示。
%     returnNew（可选）：定义是否应在响应对象中返回新文档的表示形式。
% 标头参数
%     if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则文档将被更新。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% 请求正文（对象）
% 主体必须包含一个JSON对象，该对象完全包含应被覆盖的属性，所有其他属性保持不变。
% 更新集合中特定顶点的数据。
% HTTP 200如果可以更新顶点且waitForSync为true，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     vertex：顶点的内部属性。
%     new：完整的新编写的顶点文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖顶点文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 202如果请求成功，则返回，并且waitForSync为false。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     vertex：顶点的内部属性。
%     new：完整的新编写的顶点文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖顶点文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 403如果您的用户权限不足，则返回。为了更新图中的顶点，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%     找不到具有该名称的图。
%     该集合不属于图形。
%     要更新的顶点不存在。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
updateVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, MapData, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, Headers, BodyStr).

% 替换现有的顶点
% PUT /_api/gharial/{graph}/vertex/{collection}/{vertex}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必需）：顶点所属的顶点集合的名称。
%     vertex（必填）：顶点的_key属性。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     keepNull（可选）：定义是否应存储设置为null的值。默认情况下，密钥不会从文档中删除。
%     returnOld（可选）：定义是否应在响应对象内返回已删除文档的表示。
%     returnNew（可选）：定义是否应在响应对象中返回新文档的表示形式。
% 标头参数
%     if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则文档将被更新。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% 请求正文（对象）
% 主体必须是要存储的JSON对象。
% 替换集合中顶点的数据。
% HTTP 200如果可以替换顶点且waitForSync为true，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     vertex：顶点的内部属性。
%     new：完整的新编写的顶点文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖顶点文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 202如果可以替换顶点且waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     vertex：顶点的内部属性。
%     new：完整的新编写的顶点文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖顶点文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 403如果您的用户权限不足，则返回。为了替换图中的顶点，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%     找不到具有该名称的图。
%     该集合不属于图形。
%     要替换的顶点不存在。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 412如果给出了 if -match标头，则返回，但是存储的文档版本不同。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
replaceVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, MapData, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, Headers, BodyStr).

% 从图中删除顶点
% DELETE /_api/gharial/{graph}/vertex/{collection}/{vertex}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必需）：顶点所属的顶点集合的名称。
%     vertex（必填）：顶点的_key属性。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     returnOld（可选）：定义是否应在响应对象内返回已删除文档的表示。
% 标头参数
%     if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则文档将被更新。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% 从集合中删除一个顶点。
% HTTP 200如果可以删除顶点，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     removed：设置为true，如果删除成功。
%     old：完整的删除的顶点文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     removed：设置为true，如果删除成功。
%     old：完整的删除的顶点文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
%HTTP 403如果您的用户权限不足，则返回。为了删除图中的顶点，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%     找不到具有该名称的图。
%     该集合不属于图形。
%     要删除的顶点不存在。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%      code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
delVertex(PoolNameOrSocket, GraphName, CollName, VertexKey) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delVertex(PoolNameOrSocket, GraphName, CollName, VertexKey, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/vertex/", CollName/binary, "/", (agMiscUtils:toBinary(VertexKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, Headers, undefined).

% 处理边缘
% 在现有图形中创建边
% POST /_api/gharial/{graph}/edge/{collection}
% 路径参数
% graph（必填）：图的名称。
% collection（必填）：边缘所属的边缘集合的名称。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     returnNew（可选）：定义响应是否应包含文档的完整新版本。
% 具有以下属性的JSON对象是必需的：
%     _from：此边的源顶点。必须在使用的边定义内有效。
%     _to：此边的目标顶点。必须在使用的边定义内有效。
% 在集合中创建新边缘。在主体内，边缘必须包含一个_from和_to值，以引用图中的有效顶点。此外，边缘在所使用的边缘集合的定义中必须有效 。
% HTTP 201如果可以创建边缘并且waitForSync为true，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     edge：边缘的内部属性。
%     new：完整的新编写的边缘文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     edge：边缘的内部属性。
%     new：完整的新编写的边缘文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
% HTTP 400如果输入文档无效，则返回。例如，这可以是如果_from或_to丢失。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了将边插入图形中，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% 在以下任何一种情况下返回的HTTP 404：
%     找不到具有该名称的图形。
%     该边缘集合不是图形的一部分。
%     无论是_from或_to顶点不存在。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
newEdge(PoolNameOrSocket, GraphName, CollName, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

newEdge(PoolNameOrSocket, GraphName, CollName, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 获得边
% GET /_api/gharial/{graph}/edge/{collection}/{edge}
% 路径参数
% graph（必填）：图的名称。
% collection（必填）：边缘所属的边缘集合的名称。
% edge（必填）：边缘的_key属性。
% 查询参数
% rev（可选）：必须包含修订。如果设置了此选项，则仅在具有此修订版本的情况下才返回文档。另请参见if-match标头作为替代方法。
% 标头参数
% if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则返回文档。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% if-none-match（可选）：如果给出了“ If-None-Match”标头，则它必须恰好包含一个Etag。仅当文档的版本与给定的Etag不同时，才返回文档。否则，返回HTTP 304。
% 从给定的集合中获取一条边。
% HTTP 200如果可以找到边缘，则返回。
% error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
% code：响应代码。
% edge：完整的边缘。
% HTTP 304如果给出了if-none-match标头，并且当前存储的边缘仍具有此修订值，则返回。因此，调用者上一次获取边缘之间没有更新。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 403如果您的用户权限不足，则返回。为了更新图中的顶点，您至少需要具有以下特权：
% Read Only 访问数据库。
% Read Only 访问给定的集合。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
% 找不到具有该名称的图。
% 该集合不属于图形。
% 边不存在。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
getEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, Headers, undefined).

% 修改现有边
% PATCH /_api/gharial/{graph}/edge/{collection}/{edge}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必填）：边缘所属的边缘集合的名称。
%     edge（必填）：顶点的_key属性。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     keepNull（可选）：定义是否应存储设置为null的值。默认情况下（true），给定的documents（s）属性将设置为null。如果此参数为false，则将从文档中删除该属性。
%     returnOld（可选）：定义是否应在响应对象内返回已删除文档的表示。
%     returnNew（可选）：定义是否应在响应对象中返回新文档的表示形式。
% 标头参数
%     if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则文档将被更新。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% 请求正文（对象）
%     主体必须包含一个JSON对象，该对象完全包含应被覆盖的属性，所有其他属性保持不变。
%     更新集合中特定边的数据。
% HTTP 200如果可以更新边缘，并且waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     edge：边缘的内部属性。
%     new：完整的新编写的边缘文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖边缘文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     edge：边缘的内部属性。
%     new：完整的新编写的边缘文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖边缘文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 403如果您的用户权限不足，则返回。为了更新图中的边，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%     找不到具有该名称的图。
%     该集合不属于图形。
%     要更新的边缘不存在。
%     无论是_from或_to顶点不存在（如果更新）。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
updateEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, MapData, Headers, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, Headers, BodyStr).


% 替换现有边的内容
% PUT /_api/gharial/{graph}/edge/{collection}/{edge}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必填）：边缘所属的边缘集合的名称。
%     edge（必填）：顶点的_key属性。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     keepNull（可选）：定义是否应存储设置为null的值。默认情况下，密钥不会从文档中删除。
%     returnOld（可选）：定义是否应在响应对象内返回已删除文档的表示。
%     returnNew（可选）：定义是否应在响应对象中返回新文档的表示形式。
% 标头参数
%     if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则文档将被更新。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% 具有以下属性的JSON对象是必需的：
%     _from：此边的源顶点。必须在使用的边定义内有效。
%     _to：此边的目标顶点。必须在使用的边定义内有效。
% 替换集合中边的数据。
% HTTP 201如果请求成功但waitForSync为true，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     edge：边的内部属性
%     new：完整的新编写的边缘文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖边缘文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     edge：边的内部属性
%     new：完整的新编写的边缘文档。包括请求主体中的所有书面属性以及ArangoDB生成的所有内部属性。仅在returnNew为true时存在。
%     old：完整的覆盖边缘文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 403如果您的用户权限不足，则返回。为了替换图中的边，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%     找不到具有该名称的图。
%     该集合不属于图形。
%     替换的边不存在。
%     无论是_from或_to顶点不存在。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
replaceEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, MapData) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, MapData, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, Headers, BodyStr).

% 从图形中删除边
% DELETE /_api/gharial/{graph}/edge/{collection}/{edge}
% 路径参数
%     graph（必填）：图的名称。
%     collection（必填）：边缘所属的边缘集合的名称。
%     edge（必填）：边缘的_key属性。
% 查询参数
%     waitForSync（可选）：定义请求是否应等待直到同步到磁盘。
%     returnOld（可选）：定义是否应在响应对象内返回已删除文档的表示。
% 标头参数
%     if-match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则文档将被更新。否则，返回HTTP 412。或者，您可以在URL的属性rev中提供Etag。
% 从集合中删除边缘。
% HTTP 200如果可以删除边缘，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     removed：设置为true，如果删除成功。
%     old：完整的已删除边缘文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 202如果请求成功，但waitForSync为false，则返回。
%     error：如果有错误，则标记（true），否则（false）。这个回应是错误的。
%     code：响应代码。
%     removed：设置为true，如果删除成功。
%     old：完整的已删除边缘文档。包括此操作之前存储的所有属性。仅在returnOld为true时存在。
% HTTP 403如果您的用户权限不足，则返回。为了删除图中的顶点，您至少需要具有以下特权：
%     Read Only 访问数据库。
%     Write 访问给定的集合。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%      errorMessage：为此错误创建的消息。
% 在以下情况下返回HTTP 404：
%     找不到具有该名称的图。
%     该集合不属于图形。
%     要删除的边缘不存在。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
% HTTP 412如果给出了if-match标头，则返回，但是存储的文档版本不同。
%     error：如果有错误，则标记（true），否则（false）。这个回应是真的。
%     code：响应代码。
%     errorNum：发生错误的ArangoDB错误号。
%     errorMessage：为此错误创建的消息。
delEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey) ->
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delEdge(PoolNameOrSocket, GraphName, CollName, EdgeKey, Headers, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/gharial/", GraphName/binary, "/edge/", CollName/binary, "/", (agMiscUtils:toBinary(EdgeKey))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, Headers, undefined).


