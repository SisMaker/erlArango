-module(agViews).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/views.html

% 视图的HTTP接口
% 观看次数
% 这是ArangoDB Views的HTTP接口的简介。
%
% 视图
% 视图由文档组成。它由其标识符唯一标识。它也有一个唯一的名称，客户端应该使用它来标识和访问它。视图可以重命名。这将更改视图名称，但不会更改视图标识符。视图具有创建视图时用户指定的类型。
%
% 当前唯一可用的视图类型是ArangoSearch。
%
% 查看标识符
% 视图标识符使您可以引用数据库中的视图。它是一个字符串值，在数据库中是唯一的。ArangoDB当前使用64位无符号整数值在内部维护View ID。当将View ID返回给客户端时，ArangoDB会将它们放入字符串中，以确保不支持不支持大整数的客户端不裁剪View ID。客户端在本地存储或使用它们时，应将ArangoDB返回的View ID视为不透明字符串。
%
% 查看名称
% 视图名称标识数据库中的视图。它是一个字符串，在数据库中是唯一的。与View标识符不同，它是由View的创建者提供的。视图名称必须仅由字母，数字和_（下划线）和-（破折号）字符组成。请参阅ArangoDB中的命名约定，以获取有关有效视图名称的更多信息。
%
% 视图的地址
% ArangoDB中的所有视图都有唯一的标识符和唯一的名称。ArangoDB在内部使用视图的唯一标识符来查找视图。但是，此标识符由ArangoDB管理，并且用户无法对其进行控制。为了允许用户使用自己的名称，每个视图还具有一个由用户指定的唯一名称。要从用户角度访问视图，应使用视图名称，即：
%
% http://server:port/_api/view/<view-name>
% 例如：假设视图标识符为7254820，视图名称为demo，则该视图的URL为：
% http://localhost:8529/_api/view/demo

% 创建一个ArangoSearch视图
% POST /_api/view#arangosearch
% 具有以下属性的JSON对象是必需的：
%    name：视图的名称。
%    type：视图的类型。必须等于“ arangosearch”。此选项是不变的。
%    links：期望一个对象，其属性键为要链接的集合的名称，链接属性为属性值。有关 详细信息，请参见 ArangoSearch查看链接属性。
%    primarySort：可以定义一个主要排序顺序以启用AQL优化。如果查询遍历View的所有文档，想要按属性值对它们进行排序，并且要按（最左边的）字段对它们进行排序，以及它们的排序方向与primarySort定义匹配，则SORT操作将被优化。此选项是不变的。
%       需要一个对象数组，每个对象指定一个字段（属性路径）和一个排序方向（"asc升序，"desc"降序）： [ { "field": "attr", "direction": "asc"}, … ]
%    cleanupIntervalStep：在删除ArangoSearch数据目录中的未使用文件之间至少等待这么多次提交（默认值：2，以禁用使用：0）。对于合并策略经常合并段的情况（即大量的commit + consolidate），较低的值将导致浪费大量磁盘空间。对于合并策略很少合并段（即，很少的插入/删除）的情况，较高的值将影响性能，而不会带来任何附加好处。
%       背景： 每次执行“提交”或“合并”操作时，都会在磁盘上创建View内部数据结构的新状态。一旦不再有任何用户可用，就会释放旧状态/快照。但是，释放状态/快照的文件保留在磁盘上，只能通过“清理”操作将其删除。
%    commitIntervalMsec：在提交View数据存储更改和使文档对查询可见之前，至少要等待这么多毫秒（默认值：1000，禁用使用：0）。对于插入/更新很多的情况，较低的值（直到提交）将导致索引无法解决这些问题，并且内存使用量将继续增长。对于插入/更新次数很少的情况，较高的值将影响性能并浪费每个提交调用的磁盘空间，而没有任何其他好处。
%       背景： 对于数据检索，ArangoSearch视图遵循“最终一致”的概念，即，最终ArangoDB中的所有数据将通过相应的查询表达式进行匹配。引入了ArangoSearch View“提交”操作的概念，以控制直到相应的查询表达式实际反映出文档添加/删除的时间的上限。一旦“提交”操作完成，在“提交”操作开始之前添加/删除的所有文档将由后续ArangoDB事务中调用的查询反映，正在进行的ArangoDB事务仍将继续返回可重复读取状态。
%    integrationIntervalMsec：在应用'consolidationPolicy'合并View数据存储与可能释放文件系统上的空间之间至少等待这么多毫秒（默认值：10000，禁用使用：0）。对于存在大量数据修改操作的情况，较高的值可能会使数据存储区消耗更多的空间和文件句柄。对于少量数据修改操作的情况，较低的值将影响性能，因为没有可用于合并的细分受众群。
%       背景： 对于数据修改，ArangoSearch视图遵循“版本化数据存储”的概念。因此，一旦不再有旧数据的用户，就可以删除旧版本的数据。清理和压缩操作的频率由“ consolidationIntervalMsec”控制，并通过“ consolidationPolicy”选择压缩候选对象。
%    consolidationPolicy：巩固应用策略为选择哪段应该合并（默认：{}）
%       背景： 随着每ArangoDB交易进行的插入文档的一个或多个ArangoSearch内部段被创建。同样，对于已删除的文档，包含此类文档的段会将这些文档标记为“已删除”。随着时间的流逝，这种方法会导致创建许多小的和稀疏的段。“合并”操作选择一个或多个段，并将其所有有效文档复制到单个新段中，从而使搜索算法的性能更佳，并且一旦不再使用旧段，就可以释放额外的文件句柄。
%       子属性：
%           type（字符串，可选）：根据“合并”操作的类型定义的几种可能的可配置公式选择候选候选者。当前支持的类型是：
%           "tier"（默认值）：根据段字节大小和活动文档计数（由定制属性指定）进行合并。如果使用此类型，则下面的segments*和minScore属性可用。
%           "bytes_accum"：当并且仅 {threshold} > (segment_bytes + sum_of_merge_candidate_segment_bytes) / all_segment_bytes 当即所有候选段字节大小的总和小于总段字节大小乘以{threshold}。如果使用此类型，则下面的threshold属性可用。
%           threshold（数字，可选）：范围内的值[0.0, 1.0]
%           segmentsBytesFloor（数字，可选）：定义值（以字节为单位），以将所有较小的段视为与合并选择相等（默认值：2097152）
%           segmentsBytesMax（数字，可选）：所有合并段的最大允许大小（以字节为单位）（默认值：5368709120）
%           segmentsMax（数字，可选）：将被评估为合并候选者的最大细分数（默认值：10）
%           segmentsMin（数字，可选）：将被评估为合并候选者的最小细分数（默认值：1）
%           minScore（数字，可选）：（默认值：0）
%
%    writebufferIdle：池中缓存的最大写程序（段）数（默认值：64，使用0禁用，不可变）
%    writebufferActive：执行事务的并发活动写程序（段）的最大数量。其他编写器（段）等待当前活动的编写器（段）完成（默认值：0，使用0禁用，不可变）
%    writebufferSizeMax：在触发写入器（段）刷新之前，每个写入器（段）的最大内存字节大小。0value会关闭所有写入器（缓冲区）的此限制，并且将根据为刷新线程定义的值（ArangoDB服务器启动选项）定期刷新数据。0由于潜在的高内存消耗，应谨慎使用该值（默认值：33554432，使用0禁用，不可变）
% 创建一个具有给定名称和属性的新视图（如果尚不存在）。
% 返回码
%    400：如果name或type属性丢失或无效，则 返回HTTP 400错误。
%    409：如果已经存在一个名为name的视图，则返回HTTP 409错误。
newView(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/view">>, [], BodyStr).

% 返回一个视图
% GET /_api/view/{view-name}
% 路径参数
%    view-name（必填）：视图的名称。
% 结果是一个对象，简要描述了具有以下属性的视图：
%    id：视图的标识符
%    name：视图的名称
%    type：视图的类型为字符串
% 返回码
%    404：如果视图名称未知，则返回HTTP 404。
getViewInfo(PoolNameOrSocket, ViewName) ->
   Path = <<"/_api/view/", ViewName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 返回所有视图
% GET /_api/view
% 返回一个对象，该对象包含数据库中所有视图的列表，无论它们的类型如何。它是具有以下属性的对象数组：
% ID
% 名称
% 类型
% 返回码
% 200：视图列表
getViewList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/view">>, [], undefined).

% 读取指定视图的属性
% GET /_api/view/{view-name}/properties
% 路径参数
% view-name（必填）：视图的名称。
% 返回一个对象，其中包含由view-name标识的View的定义。
% 结果是一个具有特定视图完整描述的对象，包括与视图类型相关的属性。
% 返回码
% 400：如果缺少视图名称，则返回HTTP 400。
% 404：如果视图名称未知，则返回HTTP 404。
getViewProps(PoolNameOrSocket, ViewName) ->
   Path = <<"/_api/view/", ViewName/binary, "/properties">>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 更改ArangoSearch视图的所有属性
% PUT /_api/view/{view-name}/properties#ArangoSearch
% 路径参数
%    view-name（必填）：视图的名称。
% 具有以下属性的JSON对象是必需的：
%    links：期望一个对象，其属性键为要链接的集合的名称，链接属性为属性值。有关 详细信息，请参见 ArangoSearch查看链接属性。
%    cleanupIntervalStep：在删除ArangoSearch数据目录中的未使用文件之间至少等待这么多次提交（默认值：2，以禁用使用：0）。对于合并策略经常合并段的情况（即大量的commit + consolidate），较低的值将导致浪费大量磁盘空间。对于合并策略很少合并段（即，很少的插入/删除）的情况，较高的值将影响性能，而不会带来任何附加好处。
%       背景： 每次执行“提交”或“合并”操作时，都会在磁盘上创建View内部数据结构的新状态。一旦不再有任何用户可用，就会释放旧状态/快照。但是，释放状态/快照的文件保留在磁盘上，只能通过“清理”操作将其删除。
%    commitIntervalMsec：在提交View数据存储更改和使文档对查询可见之前，至少要等待这么多毫秒（默认值：1000，禁用使用：0）。对于插入/更新很多的情况，较低的值（直到提交）将导致索引无法解决这些问题，并且内存使用量将继续增长。对于插入/更新次数很少的情况，较高的值将影响性能并浪费每个提交调用的磁盘空间，而没有任何其他好处。
%       背景： 对于数据检索，ArangoSearch视图遵循“最终一致”的概念，即，最终ArangoDB中的所有数据将通过相应的查询表达式进行匹配。引入了ArangoSearch View“提交”操作的概念，以控制直到相应的查询表达式实际反映出文档添加/删除的时间的上限。一旦“提交”操作完成，在“提交”操作开始之前添加/删除的所有文档将由后续ArangoDB事务中调用的查询反映，正在进行的ArangoDB事务仍将继续返回可重复读取状态。
%    integrationIntervalMsec：在应用'consolidationPolicy'合并View数据存储与可能释放文件系统上的空间之间至少等待这么多毫秒（默认值：10000，禁用使用：0）。对于存在大量数据修改操作的情况，较高的值可能会使数据存储区消耗更多的空间和文件句柄。对于少量数据修改操作的情况，较低的值将影响性能，因为没有可用于合并的细分受众群。
%       背景： 对于数据修改，ArangoSearch视图遵循“版本化数据存储”的概念。因此，一旦不再有旧数据的用户，就可以删除旧版本的数据。清理和压缩操作的频率由“ consolidationIntervalMsec”控制，并通过“ consolidationPolicy”选择压缩候选对象。
%    consolidationPolicy：巩固应用策略为选择哪段应该合并（默认：{}）
%       背景： 随着每ArangoDB交易进行的插入文档的一个或多个ArangoSearch内部段被创建。同样，对于已删除的文档，包含此类文档的段会将这些文档标记为“已删除”。随着时间的流逝，这种方法会导致创建许多小的和稀疏的段。“合并”操作选择一个或多个段，并将其所有有效文档复制到单个新段中，从而使搜索算法的性能更佳，并且一旦不再使用旧段，就可以释放额外的文件句柄。
%       子属性：
%          type（字符串，可选）：根据“合并”操作的类型定义的几种可能的可配置公式选择候选候选者。当前支持的类型是：
%          "tier"（默认值）：根据段字节大小和活动文档计数（由定制属性指定）进行合并。如果使用此类型，则下面的segments*和minScore属性可用。
%          "bytes_accum"：当并且仅 {threshold} > (segment_bytes + sum_of_merge_candidate_segment_bytes) / all_segment_bytes 当即所有候选段字节大小的总和小于总段字节大小乘以{threshold}。如果使用此类型，则下面的threshold属性可用。
%          threshold（数字，可选）：范围内的值[0.0, 1.0]
%          segmentsBytesFloor（数字，可选）：定义值（以字节为单位），以将所有较小的段视为与合并选择相等（默认值：2097152）
%          segmentsBytesMax（数字，可选）：所有合并段的最大允许大小（以字节为单位）（默认值：5368709120）
%          segmentsMax（数字，可选）：将被评估为合并候选者的最大细分数（默认值：10）
%          segmentsMin（数字，可选）：将被评估为合并候选者的最小细分数（默认值：1）
%          minScore（数字，可选）：（默认值：0）
% 通过替换它们来更改视图的属性。
% 成功后，将返回具有以下属性的对象：
%    id：视图的标识符
%    name：视图的名称
%    type：视图类型
% 所有其他ArangoSearch View实施特定的属性
% 返回码
%    400：如果缺少视图名称，则返回HTTP 400。
%    404：如果视图名称未知，则返回HTTP 404。
changeViewAllProps(PoolNameOrSocket, ViewName, MapData) ->
   Path = <<"/_api/view/", ViewName/binary, "/properties">>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 部分更改ArangoSearch视图的属性
% PATCH /_api/view/{view-name}/properties#ArangoSearch
% 路径参数
%    view-name（必填）：视图的名称。
% 具有以下属性的JSON对象是必需的：
%    links：期望一个对象，其属性键为要链接的集合的名称，链接属性为属性值。有关 详细信息，请参见 ArangoSearch查看链接属性。
%    cleanupIntervalStep：在删除ArangoSearch数据目录中的未使用文件之间至少等待这么多次提交（默认值：2，以禁用使用：0）。对于合并策略经常合并段的情况（即大量的commit + consolidate），较低的值将导致浪费大量磁盘空间。对于合并策略很少合并段（即，很少的插入/删除）的情况，较高的值将影响性能，而不会带来任何附加好处。
%       背景： 每次执行“提交”或“合并”操作时，都会在磁盘上创建View内部数据结构的新状态。一旦不再有任何用户可用，就会释放旧状态/快照。但是，释放状态/快照的文件保留在磁盘上，只能通过“清理”操作将其删除。
%    commitIntervalMsec：在提交View数据存储更改和使文档对查询可见之前，至少要等待这么多毫秒（默认值：1000，禁用使用：0）。对于插入/更新很多的情况，较低的值（直到提交）将导致索引无法解决这些问题，并且内存使用量将继续增长。对于插入/更新次数很少的情况，较高的值将影响性能并浪费每个提交调用的磁盘空间，而没有任何其他好处。
%       背景： 对于数据检索，ArangoSearch视图遵循“最终一致”的概念，即，最终ArangoDB中的所有数据将通过相应的查询表达式进行匹配。引入了ArangoSearch View“提交”操作的概念，以控制直到相应的查询表达式实际反映出文档添加/删除的时间的上限。一旦“提交”操作完成，在“提交”操作开始之前添加/删除的所有文档将由后续ArangoDB事务中调用的查询反映，正在进行的ArangoDB事务仍将继续返回可重复读取状态。
%    integrationIntervalMsec：在应用'consolidationPolicy'合并View数据存储与可能释放文件系统上的空间之间至少等待这么多毫秒（默认值：10000，禁用使用：0）。对于存在大量数据修改操作的情况，较高的值可能会使数据存储区消耗更多的空间和文件句柄。对于少量数据修改操作的情况，较低的值将影响性能，因为没有可用于合并的细分受众群。
%       背景： 对于数据修改，ArangoSearch视图遵循“版本化数据存储”的概念。因此，一旦不再有旧数据的用户，就可以删除旧版本的数据。清理和压缩操作的频率由“ consolidationIntervalMsec”控制，并通过“ consolidationPolicy”选择压缩候选对象。
%    consolidationPolicy：巩固应用策略为选择哪段应该合并（默认：{}）
%       背景： 随着每ArangoDB交易进行的插入文档的一个或多个ArangoSearch内部段被创建。同样，对于已删除的文档，包含此类文档的段会将这些文档标记为“已删除”。随着时间的流逝，这种方法会导致创建许多小的和稀疏的段。“合并”操作选择一个或多个段，并将其所有有效文档复制到单个新段中，从而使搜索算法的性能更佳，并且一旦不再使用旧段，就可以释放额外的文件句柄。
%       子属性：
%          type（字符串，可选）：根据“合并”操作的类型定义的几种可能的可配置公式选择候选候选者。当前支持的类型是：
%          "tier"（默认值）：根据段字节大小和活动文档计数（由定制属性指定）进行合并。如果使用此类型，则下面的segments*和minScore属性可用。
%          "bytes_accum"：当并且仅 {threshold} > (segment_bytes + sum_of_merge_candidate_segment_bytes) / all_segment_bytes 当即所有候选段字节大小的总和小于总段字节大小乘以{threshold}。如果使用此类型，则下面的threshold属性可用。
%          threshold（数字，可选）：范围内的值[0.0, 1.0]
%          segmentsBytesFloor（数字，可选）：定义值（以字节为单位），以将所有较小的段视为与合并选择相等（默认值：2097152）
%          segmentsBytesMax（数字，可选）：所有合并段的最大允许大小（以字节为单位）（默认值：5368709120）
%          segmentsMax（数字，可选）：将被评估为合并候选者的最大细分数（默认值：10）
%          segmentsMin（数字，可选）：将被评估为合并候选者的最小细分数（默认值：1）
%          minScore（数字，可选）：（默认值：0）
% 通过更新指定的属性来更改视图的属性。
% 成功后，将返回具有以下属性的对象：
%    id：视图的标识符
%    name：视图的名称
%    type：视图类型
% 所有其他ArangoSearch View实施特定的属性
% 返回码
%    400：如果缺少视图名称，则返回HTTP 400。
%    404：如果视图名称未知，则返回HTTP 404。
changeViewPartProps(PoolNameOrSocket, ViewName, MapData) ->
   Path = <<"/_api/view/", ViewName/binary, "/properties">>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

% 重命名视图
% PUT /_api/view/{view-name}/rename
% 路径参数
% view-name（必填）：要重命名的View的名称。
% 重命名视图。需要具有属性的对象
% 名称：新名称
% 它返回具有属性的对象
%     id：视图的标识符。
%     name：视图的新名称。
%     type：视图类型。
% 注意：此方法在群集中不可用。
% 返回码
%     400：如果缺少视图名称，则返回HTTP 400。
%     404：如果视图名称未知，则返回HTTP 404。
renameView(PoolNameOrSocket, ViewName, NewViewName) ->
   Path = <<"/_api/view/", ViewName/binary, "/rename">>,
   NameStr = jiffy:encode(NewViewName),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], <<"{\"name\":", NameStr/binary, "}">>).

% 删除视图
%     DELETE /_api/view/{view-name}
% 路径参数
%     view-name（必填）：要放置的视图的名称。
% 删除由view-name标识的View 。
% 如果成功删除了View，则返回具有以下属性的对象：
% 错误：假
% id：放置的视图的标识符
% 返回码
% 400：如果缺少视图名称，则返回HTTP 400。
% 404：如果视图名称未知，则返回HTTP 404。
delView(PoolNameOrSocket, ViewName) ->
   Path = <<"/_api/view/", ViewName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).
