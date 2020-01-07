-module(agCollections).
-include("erlArango.hrl").

-compile(export_all).

% 创建一个集合
% POST /_api/collection
% 查询参数
% waitForSyncReplication（可选）：默认为1，这意味着如果所有副本都创建了集合，则服务器将仅向客户端报告成功。如果您想要更快的服务器响应并且不关心完全复制，则设置为0。
% forceReplicationFactor（可选）：默认为1，这意味着服务器将在创建时检查是否有足够的副本，否则将进行紧急救助。设置为0可禁用此额外检查。
%
% 具有以下属性的JSON对象是必需的：
%    name：集合的名称。
%    waitForSync：如果为true，则在从文档创建，更新，替换或删除操作返回之前，将数据同步到磁盘。（默认值：false）
%    doCompact：是否压缩集合（默认为true），此选项仅对MMFiles存储引擎有意义。
%    journalSize：日志或数据文件的最大大小，以字节为单位。该值必须至少为1048576（1 MiB）。（默认值为配置参数）此选项仅对MMFiles存储引擎有意义。
%    isSystem：如果为true，则创建一个系统集合。在这种情况下，collection-name 应该以下划线开头。最终用户通常应仅创建非系统集合。在非常特殊的情况下，可能需要API实现者来创建系统集合，但通常情况下会使用常规集合。（默认为false）
%    isVolatile：如果为true，则收集数据仅保留在内存中，而不是持久的。卸载集合将导致集合数据被丢弃。停止或重新启动服务器也将导致集合中的数据完全丢失。
%                设置此选项将使生成的集合比常规集合快一点，因为ArangoDB不会对磁盘​​执行任何同步，也不会为数据文件计算任何CRC校验和（因为没有数据文件）。
%                因此，此选项应仅用于高速缓存类型的集合，而不能用于否则无法重新创建的数据。（默认值为false）此选项仅对MMFiles存储引擎有意义。
%    keyOptions：密钥生成的其他选项。如果指定，则keyOptions 应该是包含以下属性的JSON数组：
%                type：指定密钥生成器的类型。当前可用的生成器是传统生成器，自动递增生成器，uuid和填充生成器 。
%                      在传统的密钥生成器生成升序数字键。在自动增量密钥发生器生成升序数字键，所述inital偏移和间隔可以被配置成在填充密钥发生器以上升辞书排序顺序生成的固定长度（16个字节）的密钥。
%                      这是与RocksDB 引擎配合使用的理想选择，该引擎将稍微有利于按字典顺序升序插入的键。密钥生成器可以在单服务器或群集中使用。该UUID密钥生成器生成通用的唯一128位密钥，
%                      这些密钥以十六进制人类可读格式存储。此密钥生成器可用于单服务器或群集中，以生成“看似随机的”密钥。此密钥生成器产生的密钥未按字典顺序排序。
%                allowUserKeys：如果设置为true，则允许在文档的_key属性中提供自己的键值 。如果设置为false，那么密钥生成器将仅负责生成密钥，并且在文档的_key属性中提供自己的密钥值被视为错误。
%                      增量：自动增量密钥生成器的增量值。不适用于其他密钥生成器类型。
%                offset：自动增量密钥生成器的初始偏移值。不适用于其他密钥生成器类型。
%    type：（默认值为2）：要创建的集合的类型。类型的以下值有效：
%          2：文件收集
%          3：边缘收集
%    indexBuckets：使用哈希表将索引拆分为的存储桶数。缺省值为16，并且此数字必须为2的幂且小于或等于1024。
%             对于非常大的集合，应该增加此值，以避免在必须先构建或调整哈希表时产生长时间的停顿，因为存储桶是单独调整大小的，可以初始并行构建。
%             例如，对于具有1亿个文档的集合，64可能是一个明智的值。当前，只有边缘索引才尊重该值，但是其他索引类型可能会在将来的ArangoDB版本中使用。下次加载集合时，将应用更改（请参见下文）。此选项仅对MMFiles存储引擎有意义。
%    numberOfShards：（默认值为1）：在集群中，此值确定要为集合创建的分片数。在单服务器设置中，此选项没有意义。
%    shardKeys：（默认值为[“ _key”]）：在集群中，此属性确定用于确定文档目标分片的文档属性。文档根据其分片键属性的值发送到分片。对文档中所有分片键属性的值进行哈希处理，然后使用哈希值确定目标分片。
%             注意：分键属性值一旦设置就无法更改。在单个服务器设置中，此选项没有意义。
%    plicationFactor：（默认值为1）：在集群中，此属性确定每个分片在不同的DBServer上保留多少个副本。值1表示仅保留一个副本（无同步复制）。k的值表示保留k-1个副本。任何两个副本驻留在不同的DBServer上。
%             它们之间的复制是同步的，也就是说，在报告写入操作成功之前，对“ leader”副本的每个写入操作都会复制到所有“ follower”副本。
%             如果服务器发生故障，则会自动检测到故障，并且其中一台拥有副本的服务器将接管业务，通常不会报告错误。
%    DistributionShardsLike：（默认值为“”）：在企业版集群中，此属性将新创建的集合的分片详细信息绑定到指定的现有集合中。 注意：使用此参数会对原型集合产生影响。
%             在删除分片模仿集合之前，不能再删除它。同样，仅模拟集合的备份和还原将生成有关丢失分片原型的警告（可以被覆盖）。
%    shardingStrategy：此属性指定用于集合的分片策略的名称。从ArangoDB 3.4开始，创建新集合时可以选择不同的分片策略。所选的shardingStrategy 值将对集合保持固定，此后无法更改。
%             这对于使集合保持其分片设置并始终使用相同的初始分片算法查找已分发到分片的文档非常重要。
%    可用的分片策略为：
%    community-compat：版本3.4之前的ArangoDB社区版使用的默认分片
%    enterprise-compat：版本3.4之前的ArangoDB企业版使用的默认分片
%    enterprise-smart-edge-compat：版本3.4之前的ArangoDB Enterprise Edition中的智能边缘集合使用的默认分片
%    hash：从版本3.4开始用于新集合的默认分片（不包括智能边缘集合）
%    enterprise-hash-smart-edge：从版本3.4开始，用于新智能边缘集合的默认分片
%    如果未指定分片策略，则所有集合的默认值将为哈希，所有智能边缘集合的默认值将为enterprise-hash-smart-edge（需要ArangoDB 企业版）。手动覆盖分片策略尚不能提供好处，但是稍后可能会添加其他分片策略。
%
%    smartJoinAttribute：在Enterprise Edition集群中，此属性确定集合的属性，该属性必须包含所引用的智能联接集合的分片键值。此外，此集合中文档的分片键必须包含此属性的值，后跟冒号，然后是文档的实际主键。
%    此功能只能在企业版中使用，并且需要将集合的 distributedShardsLike属性设置为另一个集合的名称。它还要求将集合的shardKeys属性设置为单个shard key属性，并在末尾添加一个附加的“：”。进一步的限制是，无论何时在集合中存储或更新文档，smartJoinAttribute中存储的值都必须是字符串。
%
%    用给定名称创建一个新集合。该请求必须包含具有以下属性的对象。
%    400：如果缺少集合名称，则返回HTTP 400。
%    404：如果集合名称未知，则返回HTTP 404。
newColl(PoolName, Param) ->
   BodyStr = jsx:encode(Param),
   agHttpCli:callAgency(PoolName, ?Post, <<"/_api/collection">>, [], BodyStr, infinity).

% 删除收藏
% DELETE /_api/collection/{collection-name}
delColl(PoolName, CoolName, IsSystem) ->
   case IsSystem of
      true ->
         Path = <<"/_api/collection/", CoolName/binary, "?isSystem=true">>,
         agHttpCli:callAgency(PoolName, ?Delete, Path, [], undefined, infinity);
      _ ->
         Path = <<"/_api/collection/", CoolName/binary>>,
         agHttpCli:callAgency(PoolName, ?Delete, Path, [], undefined, infinity)
   end.

%% 截断集合
%% PUT /_api/collection/{collection-name}/truncate
clearColl(PoolName, CoolName) ->
   Path = <<"/_api/collection/", CoolName/binary, "/truncate">>,
   agHttpCli:callAgency(PoolName, ?Put, Path, [], undefined, infinity).

%% 返回有关集合的信息
%% GET /_api/collection/{collection-name}
%% 结果是一个对象，该对象描述具有以下属性的集合：
%% id：集合的标识符。
%% name：集合的名称。
%% status：集合状态为数字。
%%    1：新出生的收藏
%%    2：已卸载
%%    3：已加载
%%    4：在卸载过程中
%%    5：已删除
%%    6：加载
%%    其他每个状态都表示集合已损坏。
%% type：集合的类型，以数字表示。
%%    2：文件收集（正常情况）
%%    3：边缘收集
%% isSystem：如果为true，则该集合为系统集合。
collInfo(PoolName, CoolName) ->
   Path = <<"/_api/collection/", CoolName/binary>>,
   agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity).

%% 阅读集合的属性
%% 读取指定集合的​​属性
%% GET /_api/collection/{collection-name}/properties
collProperties(PoolName, CoolName) ->
   Path = <<"/_api/collection/", (CoolName)/binary, "/properties">>,
   agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity).

%% 返回集合中的文档数量
%% 计算集合中的文档
%% 请注意，这将始终将集合加载到内存中。
%% GET /_api/collection/{collection-name}/count
collCount(PoolName, CoolName) ->
   Path = <<"/_api/collection/", CoolName/binary, "/count">>,
   agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity).

%% 返回统计信息收集
%% 获取集合的统计信息
%% GET /_api/collection/{collection-name}/figures
%% 除上述内容外，结果还包含文档数量和有关集合的其他统计信息。 注意：这将始终将集合加载到内存中
collFigures(PoolName, CoolName) ->
   Path = <<"/_api/collection/", CoolName/binary, "/figures">>,
   agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity).

%% 返回负责文档的分片
%% 退还负责文件的分片
%% PUT /_api/collection/{collection-name}/responsibleShard
%% 路径参数
%% collection-name（必填）：集合的名称。
%% 请求正文（json）
%% 主体必须包含一个JSON对象，且至少将分片键属性设置为某些值。
%% 返回负责给定文档（如果存在）或如果存在该文档将负责的分片的ID。
%% 该请求必须正文必须包含一个JSON文档，其中至少将集合的分片键属性设置为某些值。
%% 响应是一个具有shardId属性的JSON对象，其中将包含负责的分片的ID。
%% 注意：此方法仅在群集协调器中可用。
collResponsibleShard(PoolName, CoolName, Param) ->
   Path = <<"/_api/collection/", CoolName/binary, "/responsibleShard">>,
   BodyStr = jsx:encode(Param),
   agHttpCli:callAgency(PoolName, ?Get, Path, [], BodyStr, infinity).

%% 返回集合永久链接的分片 ID
%% 返回集合的分片ID
%% 默认情况下，返回带有集合的分片ID的JSON数组。
%% 如果details参数设置为true，它将返回一个以分片ID作为对象属性键的JSON对象，并将每个分片的负责服务器映射到它们。在详细的响应中，领导者碎片将排在阵列的首位。
%% 注意：此方法仅在群集协调器中可用。
%% GET /_api/collection/{collection-name}/shards
%% Query Parameters
%% details (optional)：如果设置为true，则返回值还将包含集合碎片的负责服务器。
collShards(PoolName, CoolName, IsDetails) ->
   case IsDetails of
      true ->
         Path = <<"/_api/collection/", CoolName/binary, "/shards?details=true">>,
         agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity);
      _ ->
         Path = <<"/_api/collection/", CoolName/binary, "/shards">>,
         agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity)
   end.

%% 返回集合修订版ID 永久链接
%% 检索馆藏修订ID
%% 除上述内容外，结果还将包含集合的修订版ID。修订ID是服务器生成的字符串，客户端可以使用该字符串检查自上次修订检查以来集合中的数据是否已更改。
%% 版本：集合修订版本ID作为字符串。
%% GET /_api/collection/{collection-name}/revision
collRevision(PoolName, CoolName) ->
   Path = <<"/_api/collection/", CoolName/binary, "/revision">>,
   agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity).

%% 返回集合永久链接的校验和
%% 返回指定集合的​​校验和
%% 查询参数
%% withRevisions（可选）：是否在校验和计算中包括文档修订版ID。
%% withData（可选）：是否在校验和计算中包括文档主体数据。
%% 将计算集合中元数据（键和可选的修订ID）以及文档数据的校验和。
%% 校验和可用于比较不同ArangoDB实例上的两个集合是否包含相同的内容。集合的当前修订版也会返回，因此可以确保针对相同数据状态计算校验和。
%% 默认情况下，校验和将仅根据集合中包含的文档的_key系统属性来计算。对于边缘集合，系统属性_from和_to也将包含在计算中。
%% 通过将可选查询参数withRevisions设置为true，则校验和中包括修订版ID（_rev系统属性）。
%% 通过为可选查询参数withData提供值为true的值，用户定义的文档属性也将包括在计算中。 注意：包括用户定义的属性将使校验和变慢。
%% 响应是具有以下属性的JSON对象：
%% checksum：计算得出的校验和（以数字形式）。
%% 版本：集合修订版本ID作为字符串。
%% 注意：此方法在群集中不可用。
%% GET /_api/collection/{collection-name}/checksum
collChecksum(PoolName, CoolName, IsWithRevisions, IsWithData) ->
   case IsWithRevisions orelse IsWithData of
      false ->
         Path = <<"/_api/collection/", CoolName/binary, "/checksum">>,
         agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity);
      _ ->
         Path = <<"/_api/collection/", CoolName/binary, "/checksum?withRevisions=", (erlang:atom_to_binary(IsWithRevisions, utf8))/binary, "&withData=", (erlang:atom_to_binary(IsWithRevisions, utf8))/binary>>,
         agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity)
   end.

%% 读取所有收藏夹
%% 返回所有集合
%% 查询参数
%%
%%excludeSystem（可选）：是否应从结果中排除系统集合。
%% GET /_api/collection
collList(PoolName, CoolName, IsExcludeSystem) ->
   case IsExcludeSystem of
      false ->
         Path = <<"/_api/collection/", CoolName/binary, "/checksum">>,
         agHttpCli:callAgency(PoolName, ?Get, <<"/_api/collection">>, [], undefined, infinity);
      _ ->
         Path = <<"/_api/collection/?excludeSystem=true">>,
         agHttpCli:callAgency(PoolName, ?Get, Path, [], undefined, infinity)
   end.

%% 加载收藏
%% 请求主体对象可以选择包含以下属性：
%% count：如果设置，则控制返回值是否应包括集合中的文档数。将count设置为 false可以加快加载集合的速度。为默认值 数为真。
%% PUT /_api/collection/{collection-name}/load
collLoad(PoolName, CoolName, IsCount) ->
   case IsCount of
      false ->
         Path = <<"/_api/collection/", CoolName/binary, "/load">>,
         agHttpCli:callAgency(PoolName, ?Put, Path, [],  <<"{\"count\":false}">>, infinity);
      _ ->
         Path = <<"/_api/collection/", CoolName/binary, "/load">>,
         agHttpCli:callAgency(PoolName, ?Put, Path, [], undefined, infinity)
   end.

