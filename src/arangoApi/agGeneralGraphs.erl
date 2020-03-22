-module(agGeneralGraphs).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/gharial.html

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
getGharial(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_api/gharial">>, [], undefined).


% 在图形模块中创建一个新图形。
% POST /_api/gharial
% 查询参数
% waitForSync（可选）：定义请求是否应该等待，直到所有内容都同步到光盘。将更改成功响应代码。
% 具有以下属性的JSON对象是必需的：
% name：图形名称。
% edgeDefinitions：图形关系的定义数组。每个都有以下类型：
% orphanCollections：其他顶点集合的数组。这些集合中的文档在此图中没有边。
% isSmart：定义创建的图形是否应该是智能的。这仅在企业版中有效。
% options：一个JSON对象，用于定义用于在此图中创建集合的选项。它可以包含以下属性：
% smartGraphAttribute：仅在企业版中有效，如果isSmart为true，则为必需。用于聪明地分割图的顶点的属性名称。此SmartGraph中的每个顶点都必须具有此属性。以后无法修改。
% numberOfShards：此图中每个集合使用的分片数。以后无法修改。
% 复制因子：最初为此图创建集合时使用的复制因子。
% writeConcern：对图中的新集合进行关注。它确定在不同的DBServer上同步每个分片需要多少个副本。如果集群中的副本数量很少，那么分片将拒绝写入。但是，具有足够最新副本的分片写入将同时成功。writeConcern的值 不能大于ReplicationFactor。（仅集群）
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
% Read Only 访问此图中使用的每个集合。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
% HTTP 409如果存储图形时发生冲突，则返回。如果已经存储了具有该名称的图形，或者存在一个边缘定义具有相同的边缘集合但在任何其他图形中使用了不同的签名，则可能会发生这种情况 。
% error：如果有错误，则标记（true），否则（false）。这个回应是真的。
% code：响应代码。
% errorNum：发生错误的ArangoDB错误号。
% errorMessage：为此错误创建的消息。
newGharial(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, <<"/_api/gharial">>, [], BodyStr).

newGharial(PoolNameOrSocket, MapData, WaitForSync) ->
   Path =
      case WaitForSync of
      true ->
         <<"/_api/gharial?waitForSync=true">>;
      _ ->
         <<"/_api/gharial?waitForSync=false">>
   end,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, Path, [], BodyStr).
