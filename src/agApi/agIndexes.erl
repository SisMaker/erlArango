-module(agIndexes).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/indexes.html

% 索引的HTTP接口
% 索引
% 这是对ArangoDB索引的HTTP接口的一般介绍。有各种索引类型的特殊部分。
% 索引
% 索引用于允许快速访问文档。对于每个集合，总是有主索引，它是文档关键字（_key属性）的哈希索引 。不能删除或更改该索引。 边缘集合还将具有一个自动创建的边缘索引，该索引无法修改。该索引可通过_from和_to属性快速访问文档。
% 可以通过定义应建立索引的属性名称来创建大多数用户土地索引。一些索引类型只允许索引一个属性（例如全文索引），而其他索引类型只允许索引多个属性。
% _id任何索引类型均不支持在用户定义的索引中使用system属性。

% 索引句柄
% 索引句柄唯一地标识数据库中的索引。它是一个字符串，由集合名称和由/分隔的索引标识符组成。如果索引被声明为唯一，那么对索引属性的访问应该很快。如果索引属性仅包含很少的不同值，则性能会降低。

% 主索引
% 将自动为每个集合创建一个主索引。它索引文档的主键，这些主键存储在_keysystem属性中。主索引是唯一的，可用于_key和_id属性的查询。无法显式创建或删除主索引。

% 边缘索引
% 将自动为边缘集合创建边缘索引。它包含顶点文档之间的连接，并在查询顶点的连接边时调用。无法显式创建或删除边缘索引。边缘索引是唯一的。

% 哈希指数固定链接
% 哈希索引是未排序的索引，可用于通过相等查找查找单个文档。
%
% 跳过清单索引固定链接
% 跳过列表是可用于查找单个文档或文档范围的排序索引。

% 永久索引
% 持久索引是可用于查找单个文档或文档范围的排序索引。与其他索引相反，持久索引的内容存储在磁盘上，因此在加载集合时不需要从文档在内存中重建。

% TTL（生存时间）索引
% TTL索引可用于自动从集合中删除过期的文档。过期的文档最终将由后台线程删除。

% 全文索引
% 全文索引可用于在文档中查找单词或单词前缀。全文索引只能在一个属性上设置，并且将对文档中包含的所有具有该属性文本值的单词进行索引。仅索引（指定）最小长度的单词。使用libicu提供的单词边界分析来完成单词标记化，该分析考虑了服务器启动时提供的所选语言。单词以小写形式索引。该索引支持完全匹配查询（全字）和前缀查询。

% 索引地址
% ArangoDB中的所有索引都有唯一的句柄。该索引句柄标识一个索引，并由ArangoDB管理。所有索引都在URI下找到
% http://server:port/_api/index/index-handle
% 例如：假设索引句柄为demo / 63563528，则该索引的URL为：
% http://localhost:8529/_api/index/demo/63563528

% 返回索引
% GET /_api/index/{index-id}
% 路径参数
%    index-id（必需）：索引标识符。
% 结果是一个描述索引的对象。它至少具有以下属性：
%    id：索引的标识符
%    type：索引类型
% 所有其他属性都取决于类型。例如，某些索引提供 唯一或稀疏标志，而另一些则不提供。一些索引还在结果的selectivityEstimate属性中提供了选择性估计。
% 返回码
%    200：如果索引存在，则返回HTTP 200。
%    404：如果索引不存在，则 返回HTTP 404。
getIndexInfo(PoolNameOrSocket, IndexId) ->
   Path = <<"/_api/index/", (agMiscUtils:toBinary(IndexId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 创建一个索引
% POST /_api/index#general
% 查询参数
%    collection （必填）：集合名称。
% 请求正文（json）
% 在集合collection中创建一个新索引。期望包含索引详细信息的对象。
% 必须在 索引详细信息的type属性中指定要创建的索引的类型。根据索引类型，可能需要在请求中指定其他其他属性才能创建索引。
% 索引要求索引详细信息的fields属性中的被索引属性。根据索引类型，可以为一个或多个属性建立索引。在后一种情况下，需要一个字符串数组。
% 用户定义的索引不支持索引系统属性_id。使用_id作为索引属性手动创建索引将失败，并显示错误。
% （可选）索引名称可以在name属性中指定为字符串。索引名称与集合名称具有相同的限制。如果未指定任何值，将自动生成一个值。
% 某些索引可以创建为唯一或非唯一变体。通过在索引详细信息中指定唯一标志，可以控制大多数索引的唯一性。将其设置为true将创建唯一索引。将其设置为false或忽略unique属性将创建一个非唯一索引。
% 注意：以下索引类型不支持唯一性，并且对这些类型使用unique属性可能会导致错误：
% 地理索引
% 全文索引
% 注意：集群中不支持非分片键上的唯一索引。
%
% 可以选择在稀疏变量中创建哈希，跳过列表和持久索引。如果索引详细信息中的sparse属性设置为true，则将创建一个稀疏索引。稀疏索引不会索引未设置任何索引属性或为null的文档。
%
% 类型为hash或skiplist的数组索引支持可选的重复数据删除属性。它控制将来自同一文档的重复索引值插入唯一数组索引是否会导致唯一约束错误。默认值为true，因此每个非唯一索引值的单个实例将插入每个文档的索引中。无论此属性的值如何，尝试将值插入到索引中已存在的索引始终将失败。
%
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    400：如果发布了无效的索引描述或使用了目标索引不支持的属性，则返回HTTP 400。
%    404：如果集合未知，则返回HTTP 404。
newIndex(PoolNameOrSocket, CollName, MapData) ->
   Path = <<"/_api/index?collection=", CollName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 删除索引
% DELETE /_api/index/{index-id}
% 路径参数
%    index-id（必需）：索引ID。
% 删除带有index-id的索引。
% 返回码
%    200：如果可以删除索引，则返回HTTP 200。
%    404：如果index-id未知，则返回HTTP 404。
delIndex(PoolNameOrSocket, IndexId) ->
   Path = <<"/_api/index/", (agMiscUtils:toBinary(IndexId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 返回集合的所有索引
% GET /_api/index
% 查询参数
%    collection （必填）：集合名称。
% 返回一个对象，该对象的属性索引包含给定集合的所有索引描述的数组。在标识符中还可以使用与索引句柄作为键的对象相同的信息。
% 返回码
%    200：返回一个JSON对象，其中包含该集合的索引列表。
getIndexList(PoolNameOrSocket, CollName) ->
   Path = <<"/_api/index?collection=", CollName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 使用哈希索引
% 如果存在合适的哈希索引，/_api/simple/by-example则将使用该索引执行示例查询。

% 创建一个哈希索引
% POST /_api/index#hash
% 查询参数
%     collection（必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%    type：必须等于“ hash”。
%    fields：属性路径的数组。
%    unique：如果为true，则创建一个唯一索引。
%    sparse：如果为true，则创建一个稀疏索引。
%    deduplicate：如果为false，则关闭数组值的重复数据删除。
% 如果不存在，则为集合collection-name创建哈希索引。该调用需要一个包含索引详细信息的对象。
% 在稀疏索引中，所有不包含至少一个指定索引属性（即field）或在任何指定索引属性中都为null的文档将从索引中排除。如果设置了唯一标志，则不会对此类文档建立索引，也不会将其用于唯一性检查。
% 在非稀疏索引中，将为这些文档建立索引（对于不存在索引的属性，将使用null值），并且如果设置了唯一标志，则将对它们进行唯一性检查。
% 注意：集群中不支持非分片键上的唯一索引。
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    400：如果集合中已经包含文档，并且您尝试创建唯一哈希索引以使某些文档违反唯一性，则返回HTTP 400。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfHash(PoolNameOrSocket, CollName, MapData) ->
   case MapData of
      #{type := <<"hash">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      #{<<"type">> := <<"hash">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      _ ->
         {error, param}
   end.

% 返回与给定示例匹配的集合的所有文档
% PUT /_api/simple/by-example
% 此路由不应再使用。从3.4.0版开始，不推荐使用简单查询的所有端点。它们被AQL查询取代。
% 直到ArangoDB版本3.2.13和3.3.7，此API相当昂贵。一种更轻量的替代方法是使用HTTP Cursor API。从版本3.2.14和3.3.8开始，此性能影响不再是问题，因为API的内部实现已更改。
% 具有以下属性的JSON对象是必需的：
% collection：要查询的集合的名称。
% example：示例文档。
% skip：查询中要跳过的文档数（可选）。
% limit：要返回的最大文档数。该跳跃 是在之前应用极限的限制。（可选的）
% batchSize：一次往返从服务器传输到客户端的最大结果文档数。如果未设置此属性，则将使用服务器控制的默认值。甲BATCHSIZE的值 0是不允许的。
% 这将找到与给定示例匹配的所有文档。
% 返回包含结果的游标，有关详细信息，请参见HTTP Cursor。
% 返回码
% 201：查询执行成功返回。
% 400：如果正文不包含查询的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。


% 返回与给定示例匹配的集合的一个文档
% PUT /_api/simple/first-example
% 此路由不应再使用。从3.4.0版开始，不推荐使用简单查询的所有端点。它们被AQL查询取代。
% 直到ArangoDB版本3.2.13和3.3.7，此API相当昂贵。一种更轻量的替代方法是使用HTTP Cursor API。从版本3.2.14和3.3.8开始，此性能影响不再是问题，因为API的内部实现已更改。
% 具有以下属性的JSON对象是必需的：
% collection：要查询的集合的名称。
% example：示例文档。
% 这将返回与给定示例匹配的第一个文档。
% 如果没有文档与示例匹配，则返回包含文档或HTTP 404的结果。
% 如果集合中有多个文档与指定的示例匹配，则仅返回其中一个文档，并且不确定返回哪个匹配的文档。
% 返回码
% 200：成功执行查询后返回。
% 400：如果正文不包含查询的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。

% 使用跳过列表索引
% 如果存在合适的跳过列表索引，则/_api/simple/range其他操作将使用该索引来执行查询。

% 创建一个跳过列表
% POST /_api/index#skiplist
% 查询参数
%       collection（必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%       type：必须等于“ skiplist”。
%       fields：属性路径的数组。
%       unique：如果为true，则创建一个唯一索引。
%       sparse：如果为true，则创建一个稀疏索引。
%       deduplicate：如果为false，则关闭数组值的重复数据删除。
% 为集合collection-name创建一个跳过列表索引（如果尚不存在）。该调用需要一个包含索引详细信息的对象。
% 在稀疏索引中，所有不包含至少一个指定索引属性（即field）或在任何指定索引属性中都为null的文档将从索引中排除。如果设置了唯一标志，则不会对此类文档建立索引，也不会将其用于唯一性检查。
% 在非稀疏索引中，将为这些文档建立索引（对于不存在索引的属性，将使用null值），并且如果设置了唯一标志，则将对它们进行唯一性检查。
% 注意：集群中不支持非分片键上的唯一索引。
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    400：如果集合中已经包含文档，并且您尝试以存在违反唯一性的文档的方式创建唯一的跳过列表索引，则返回HTTP 400。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfSkipList(PoolNameOrSocket, CollName, MapData) ->
   case MapData of
      #{type := <<"skiplist">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      #{<<"type">> := <<"skiplist">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      _ ->
         {error, param}
   end.

% 使用持久索引
% 如果存在合适的持久索引，则/_api/simple/range其他操作将使用该索引执行查询。
%
% 创建一个持久索引
% POST /_api/index#persistent
% 查询参数
%     collection（必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%     type：必须等于“ persistent”。
%     fields：属性路径的数组。
%     unique：如果为true，则创建一个唯一索引。
%     sparse：如果为true，则创建一个稀疏索引。
% 为集合collection-name创建一个持久索引（如果尚不存在）。该调用需要一个包含索引详细信息的对象。
% 在稀疏索引中，所有不包含至少一个指定索引属性（即field）或在任何指定索引属性中都为null的文档将从索引中排除。如果设置了唯一标志，则不会对此类文档建立索引，也不会将其用于唯一性检查。
% 在非稀疏索引中，将为这些文档建立索引（对于不存在索引的属性，将使用null值），并且如果设置了唯一标志，则将对它们进行唯一性检查。
% 注意：集群中不支持非分片键上的唯一索引。
% 返回码
%     200：如果索引已经存在，则返回HTTP 200。
%     201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%     400：如果集合中已经包含文档，并且您尝试以存在违反唯一性的文档的方式创建唯一的持久索引，那么将返回HTTP 400。
%     404：如果集合名称未知，则返回HTTP 404。
newIndexOfPersistent(PoolNameOrSocket, CollName, MapData) ->
   case MapData of
      #{type := <<"persistent">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      #{<<"type">> := <<"persistent">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      _ ->
         {error, param}
   end.

% 使用TTL（生存时间）索引
%
% 创建一个TTL（生存时间）索引
% POST /_api/index#ttl
% 查询参数
% collection（必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%     type：必须等于“ ttl”。
%     fields：一个具有唯一属性路径的数组。
%     expireAfter：文档创建后经过的时间（以秒为单位），之后文档被视为“过期”。
% 为集合collection-name创建TTL索引（如果尚不存在）。该调用需要一个包含索引详细信息的对象。
% 返回码
%     200：如果索引已经存在，则返回HTTP 200。
%     201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%     400：如果集合已经包含另一个TTL索引，则返回HTTP 400，因为每个集合最多可以有一个TTL索引。
%     404：如果集合名称未知，则返回HTTP 404。
newIndexOfTtl(PoolNameOrSocket, CollName, MapData) ->
   case MapData of
      #{type := <<"ttl">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      #{<<"type">> := <<"ttl">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      _ ->
         {error, param}
   end.

% 创建地理索引
% POST /_api/index#geo
% 查询参数
%       collection （必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%       type：必须等于“ geo”。
%       fields：具有一个或两个属性路径的数组。
% 如果它是一个具有一个属性路径location的数组，那么将使用location作为坐标的路径在所有文档上创建地理空间索引。该属性的值必须是具有至少两个double值的数组。数组必须包含纬度（第一个值）和经度（第二个值）。所有没有属性路径或值不适合的文档都将被忽略。
% 如果它是具有两个属性路径latitude和经度的数组，则将使用纬度 和经度在所有文档上创建地理空间索引作为路径的纬度和经度。属性纬度和属性经度的值必须加倍。所有没有属性路径或值不适合的文档都将被忽略。
%
% geoJson：如果在某个位置上构建了地理空间索引，并且geoJson为true，则数组内的顺序为经度和纬度。这对应于http://geojson.org/geojson-spec.html#positions中描述的格式
% 在集合collection-name中创建地理空间索引（如果尚不存在）。期望包含索引详细信息的对象。
% 地理位置索引总是稀疏的，这意味着不包含索引属性或索引属性中具有非数字值的文档将不会被索引。
%
% 返回码
%    200：如果索引已经存在，则返回HTTP 200。
%    201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%    404：如果集合名称未知，则返回HTTP 404。
newIndexOfGeo(PoolNameOrSocket, CollName, MapData) ->
   case MapData of
      #{type := <<"geo">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      #{<<"type">> := <<"geo">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      _ ->
         {error, param}
   end.

%返回给定位置附近集合的所有文档
%PUT /_api/simple/near
%此路由不应再使用。从3.4.0版开始，不推荐使用简单查询的所有端点。它们被AQL查询取代。
%具有以下属性的JSON对象是必需的：
%collection：要查询的集合的名称。
%latitude：坐标的纬度。
%经度：坐标的经度。
%distance：如果给定，则用于将距离返回给定坐标的属性键。（可选的）。如果指定，则以米为单位返回距离。
%skip：查询中要跳过的文档数。（可选的）
%limit：要返回的最大文档数。该跳跃是在之前应用极限的限制。默认值为100。（可选）
%geo：如果给定，则为要使用的地理索引的标识符。（可选的）
%默认值将在给定坐标附近最多找到100个文档。返回的数组根据距离排序，最近的文档在返回数组中排在最前面。如果附近有等距离的文档，则从该组文档中随机选择文档，直到达到限制。
%为了使用Near运算符，必​​须为集合定义一个地理索引。该索引还定义了哪个属性保存文档的坐标。如果您有多个地理空间索引，则可以使用geo字段来选择特定的索引。
%返回包含结果的游标，有关详细信息，请参见HTTP Cursor。
%注意：自ArangoDB 2.6起，不推荐使用近乎简单的查询。在将来的ArangoDB版本中可能会删除此API。使用Near运算符从集合中检索文档的首选方法是使用NEAR函数发出AQL查询，如下所示：
%FOR doc IN NEAR(@@collection, @latitude, @longitude, @limit)
%RETURN doc`
%返回码
%201：查询执行成功返回。
%400：如果正文不包含查询的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
%404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。

% 查找坐标周围半径内的文档
% 返回给定半径内集合的所有文档
% PUT /_api/simple/within
% 此路由不应再使用。从3.4.0版开始，不推荐使用简单查询的所有端点。它们被AQL查询取代。
% 具有以下属性的JSON对象是必需的：
% collection：要查询的集合的名称。
% latitude：坐标的纬度。
% 经度：坐标的经度。
% radius：最大半径（以米为单位）。
% distance：如果给定，则用于将距离返回给定坐标的属性键。（可选的）。如果指定，则以米为单位返回距离。
% limit：要返回的最大文档数。该跳跃是在之前应用极限的限制。默认值为100。（可选）
% geo：如果给定，则为要使用的地理索引的标识符。（可选的）
% 这将找到围绕坐标（纬度，经度）的给定半径内的所有文档。返回的列表按距离排序。
% 为了使用内部运算符，必​​须为集合定义一个地理索引。该索引还定义了哪个属性保存文档的坐标。如果您有多个地理空间索引，则可以使用geo字段来选择特定的索引。
% 返回包含结果的游标，有关详细信息，请参见HTTP Cursor。
% 注意：从ArangoDB 2.6开始不推荐使用内部简单查询。在将来的ArangoDB版本中可能会删除此API。使用Near运算符从集合中检索文档的首选方法是使用WITHIN函数发出AQL查询，如下所示：
% FOR doc IN WITHIN(@@collection, @latitude, @longitude, @radius, @distanceAttributeName)
% RETURN doc
% 返回码
% 201：查询执行成功返回。
% 400：如果正文不包含查询的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。

% 创建全文索引
% POST /_api/index#fulltext
% 查询参数
%       collection （必填）：集合名称。
% 具有以下属性的JSON对象是必需的：
%       type：必须等于“全文”。
%       fields：属性名称的数组。当前，数组仅限于一个属性。
%       minLength：要索引的单词的最小字符长度。如果未指定，则默认为服务器定义的值。因此，建议在创建索引时显式设置此值。
% 为集合collection-name创建全文索引（如果尚不存在）。该调用需要一个包含索引详细信息的对象。
% 返回码
%       200：如果索引已经存在，则返回HTTP 200。
%       201：如果索引尚不存在并且可以创建，则 返回HTTP 201。
%       404：如果集合名称未知，则返回HTTP 404。
newIndexOfFulltext(PoolNameOrSocket, CollName, MapData) ->
   case MapData of
      #{type := <<"fulltext">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      #{<<"type">> := <<"fulltext">>} ->
         Path = <<"/_api/index?collection=", CollName/binary>>,
         BodyStr = jiffy:encode(MapData),
         agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr);
      _ ->
         {error, param}
   end.

% 全文索引查询
% 通过全文查询返回集合的文档
% PUT /_api/simple/fulltext
% 此路由不应再使用。从3.4.0版开始，不推荐使用简单查询的所有端点。它们被AQL查询取代。
% 具有以下属性的JSON对象是必需的：
% collection：要查询的集合的名称。
% attribute：包含文本的属性。
% 查询：全文查询。请参阅全文查询 以获取详细信息。
% skip：查询中要跳过的文档数（可选）。
% limit：要返回的最大文档数。该跳跃 是在之前应用极限的限制。（可选的）
% index：要使用的全文索引的标识符。
% 这将发现从集合匹配指定的全文查询的所有文档的查询。
% 为了使用全文运算符，必​​须为集合和指定的属性定义全文索引。
% 返回包含结果的游标，有关详细信息，请参见HTTP Cursor。
% 注意：从ArangoDB 2.6起不推荐使用全文本简单查询。在将来的ArangoDB版本中可能会删除此API。使用Near运算符从集合中检索文档的首选方法是使用FULLTEXT AQL函数发出AQL查询 ，如下所示：
% FOR doc IN FULLTEXT(@@collection, @attributeName, @queryString, @limit)
% RETURN doc
% 返回码
% 201：查询执行成功返回。
% 400：如果正文不包含查询的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。



