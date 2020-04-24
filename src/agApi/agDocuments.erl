-module(agDocuments).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/document.html

% 文档的HTTP接口

% 基础和术语
% 文件，密钥，句柄和修订
% ArangoDB中的文档是JSON对象。这些对象可以嵌套（任何深度），并且可以包含列表。每个文档都有一个唯一的 主键，可以在其集合中对其进行标识。此外，每个文档都由其文档句柄 跨同一数据库中所有集合唯一标识。同一文档的不同修订版（由其句柄标识）可以通过其文档修订版来区分 。任何交易都只能看到文档的单个修订版。
% 这是一个示例文件：
% {
%    "_id" : "myusers/3456789",
%    "_key" : "3456789",
%    "_rev" : "14253647",
%    "firstName" : "John",
%    "lastName" : "Doe",
%    "address" : {
%       "street" : "Road To Nowhere 1",
%       "city" : "Gotham"
%    },
%    "hobbies" : [
%    {"name": "swimming", "howFavorite": 10},
%    {"name": "biking", "howFavorite": 6},
%    {"name": "programming", "howFavorite": 4}
%    ]
% }
% 所有文档都包含特殊属性： 文档句柄以字符串形式存储在中_id， 文档主键存储在中 _key， 文档修订版本存储在中 _rev。_key创建文档时，用户可以指定属性的值。创建文档后_id，_key值是不可变的。该_rev值由ArangoDB自动维护。
% 文件把手
% 文档句柄唯一标识数据库中的文档。它是一个字符串，由集合名称和文档键（_key属性）组成，以分隔/。
%
% 文件金钥
% 文档密钥可以唯一地标识存储在其集合中的文档。查询特定文档时，客户端可以并且应该使用该文档密钥。文档密钥存储在_key每个文档的属性中。键值由ArangoDB自动在集合的主索引中建立索引。因此，通过其键查找文档是快速的操作。创建文档后，文档的_key值是不变的。默认情况下，如果未指定_key属性，则ArangoDB将自动生成文档密钥，否则使用用户指定的_key。
%
% 通过使用keyOptions属性创建集合，可以在每个集合级别更改此行为。
%
% 使用keyOptions它可以完全禁止用户指定的键，或者强制使用特定机制来自动生成_key 值。
%
% 文件修订
% ArangoDB中的每个文档都有一个修订版本，存储在system属性中 _rev。它由服务器完全管理，对用户只读。
%
% 它的值应视为不透明的，不保证其格式和属性，但在文档更新后其值将有所不同。更具体地说，_rev值在单个服务器设置中的所有文档和所有集合中都是唯一的。在群集设置中，_rev即使在同一毫秒内编写，一个分片内也可以确保两个不同的文档修订版具有不同的字符串。
%
% 该_rev属性可以用作查询的前提，以避免 丢失更新情况。也就是说，如果客户端从服务器获取文档，在本地对其进行修改（但未更改_rev属性），然后将其发送回服务器以更新文档，但是与此同时，该文档已被其他操作更改，则修订不会匹配，服务器将取消该操作。如果没有这种机制，客户端将不知不觉地覆盖对文档所做的更改。
%
% 当更新或替换现有文档时，ArangoDB会将该文档的新版本写入预写日志文件（与存储引擎无关）。写入新版本的文档后，旧版本仍然存在，至少在磁盘上。删除现有文档（版本）时也是如此：文档的旧版本加上删除操作将在磁盘上保留一段时间。
%
% 因此，在磁盘上可能同时_key存在同一文档的多个修订版（由相同的值标识）。但是，无法访问过时的修订。文档成功更新或删除后，用户将无法再执行查询或其他数据检索操作。此外，一段时间后，内部将删除旧修订。这是为了避免磁盘使用量不断增长。
%
% 从用户的角度来看，每个时间点仅存在一个文档修订版本_key。没有内置的系统来自动保留对文档所做的所有更改的历史记录，并且无法通过该_rev值恢复文档的旧版本。
%
% 文件标签
% ArangoDB尝试尽可能遵守现有的HTTP标准。为此，单文档查询的结果将HTTP标头Etag设置为使用双引号括起来的文档修订版。
%
% 文档的基本操作（创建，读取，存在，替换，更新，删除）被映射到标准HTTP方法（POST，GET， HEAD，PUT，PATCH和DELETE）。
%
% 如果修改文档，则可以使用“ 如果匹配”字段来检测冲突。可以使用HTTP方法HEAD检查文档的修订。
%
% 单个请求中包含多个文档
% 从ArangoDB 3.0开始，基本文档API已扩展为不仅可以处理单个文档，而且可以在单个请求中处理多个文档。这对于性能至关重要，尤其是在集群情况下，在这种情况下，单个请求可能涉及集群中的多个网络跃点。另一个优点是，它减少了HTTP协议的开销以及客户端和服务器之间的各个网络往返。在单个请求中执行多个文档操作的总体思路是使用JSON对象数组代替单个文档。因此，必须将前提条件的文档密钥，句柄和修订内容嵌入到给定的各个文档中。多个文档操作仅限于单个文档或边集合。看到有关详细信息的API描述。
%
% 请注意，GET，HEAD和DELETE HTTP操作通常不允许传递消息正文。因此，它们不能用于在一个请求中执行多个文档操作。但是，还有其他端点可以在一个请求中请求和删除多个文档。请参阅批量文档操作。
%
% 文件的URI
% 可以使用其唯一的URI检索任何文档：
%
% http://server:port/_api/document/<document-handle>
% 例如，假设文档句柄为demo/362549736，则该文档的URL为：
%
% http://localhost:8529/_api/document/demo/362549736
% 上面的URL架构未明确指定 数据库名称 ，因此_system将使用默认数据库。要明确指定数据库上下文，请使用以下URL模式：
%
% http://server:port/_db/<database-name>/_api/document/<document-handle>
% 例：
%
% http://localhost:8529/_db/mydb/_api/document/demo/362549736
% 注意：以下示例为了简短起见使用短URL格式。
%
% 请求文档时，文档修订版本将在“ Etag” HTTP标头中返回。
%
% 如果使用GET获取文档，并且要检查是否有较新的修订版本，则可以使用If-None-Match标头。如果文档未更改，则返回HTTP 412（前提条件失败）错误。
%
% 如果要查询，替换，更新或删除文档，则可以使用If-Match标头。如果文档已更改，则操作将中止，并返回HTTP 412错误。

% 读取单个文档
% GET /_api/document/{collection}/{key}
% 路径参数
% collection（必填）：要从中读取文档的集合的名称。
% key （必填）：文档密钥。
% 标头参数
%     If-None-Match（可选）：如果给出了“ If-None-Match”标头，则它必须恰好包含一个Etag。如果文档版本与给定的Etag不同，则返回文档。否则，返回HTTP 304。
%     If-Match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则返回文档。否则，返回HTTP 412。
% 返回由document-id标识的文档。返回的文档包含三个特殊属性：_id包含文档标识符，_key包含唯一标识给定集合中的文档的键，_rev包含修订版。
% 返回码
% 200：如果找到文档，则返回
% 304：如果给出“ If-None-Match”标题并且文档具有相同版本，则返回
% 404：如果找不到文档或集合，则返回
% 412：如果给出“ If-Match”标头并且找到的文档具有不同版本，则返回412。响应还将在_rev属性中包含找到的文档的当前修订。此外，将返回属性_id和_key。
getDoc(PoolNameOrSocket, CollName, Key) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getDoc(PoolNameOrSocket, CollName, Key, Headers) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, Headers, undefined).

% 读取单个文档头
% HEAD /_api/document/{collection}/{key}
% 路径参数
% collection （必填）：要从中读取文档的集合的名称。
% key （必填）：文档密钥。
% 标头参数
% If-None-Match（可选）：如果给出了“ If-None-Match”标头，则它必须恰好包含一个Etag。如果当前文档修订版不等于指定的Etag，则返回HTTP 200响应。如果当前文档修订版与指定的Etag相同，则返回HTTP 304。
% If-Match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则返回文档。否则，返回HTTP 412。
% 类似于GET，但仅返回标头字段，而不返回正文。您可以使用此调用来获取文档的当前版本，或检查文档是否已删除。
% 返回码
% 200：如果找到文档，则返回
% 304：如果给出“ If-None-Match”标题并且文档具有相同版本，则返回
% 404：如果找不到文档或集合，则返回
% 412：如果给出“ If-Match”标头并且找到的文档具有不同版本，则返回412。响应还将在Etag标头中包含找到的文档的当前版本。
getDocHead(PoolNameOrSocket, CollName, Key) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgHead, Path, [], undefined).

getDocHead(PoolNameOrSocket, CollName, Key, Headers) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgHead, Path, Headers, undefined).

% 创建文档
% POST /_api/document/{collection}
% 路径参数
%    collection （必填）：要在其中创建文档的集合的名称。
% 查询参数
% collection （可选）：集合的名称。这仅是为了向后兼容。在ArangoDB版本<3.0中，URL路径为/ _api / document，并且此查询参数是必需的。这种组合仍然有效，但是建议的方法是在URL路径中指定集合。
% waitForSync（可选）：等待文档已同步到磁盘。
% returnNew（可选）：另外 ，在结果的new属性下返回完整的新文档。
% returnOld（可选）：另外 ，在结果的old属性下返回完整的旧文档。仅在使用覆盖选项时可用。
% silent（可选）：如果设置为true，则将返回一个空对象作为响应。创建的文档将不返回任何元数据。此选项可用于节省一些网络流量。
% overwrite（可选）：如果设置为true，则插入将成为替换插入。如果已经存在具有相同_key的文档，则不会由于违反唯一约束而拒绝新文档，而是将替换旧文档。
% overwriteMode（可选）：此选项取代覆盖并提供以下模式：
%    "ignore"：如果已经存在具有指定_key值的文档，则不会执行任何操作，也不会执行任何写操作。在这种情况下，插入操作将返回成功。此模式不支持使用来返回旧文档版本RETURN OLD。使用时 RETURN NEW，如果文档已经存在，则将返回null。
%    "replace"：如果已经存在具有指定_key值的文档，则该文档将被指定的文档值覆盖。当未指定overwrite模式但覆盖 标志设置为true时，也将使用此模式。
%    "update"：如果已经存在具有指定_key值的文档，则将使用指定的文档值对其进行修补（部分更新）。可以通过keepNull和 mergeObjects参数进一步控制覆盖模式。
%    "conflict"：如果已经存在具有指定_key值的文档，则返回唯一的违反约束错误，以使插入操作失败。如果未设置覆盖模式，并且覆盖标志为false或未设置，这也是默认行为。
% keepNull（可选）：如果要使用update-insert命令删除现有属性，则可以将URL查询参数keepNull与 false一起使用。这将修改patch命令的行为，以从属性文档中删除属性文件值为null的现有文档。此选项仅控制更新插入行为。
% mergeObjects（可选）：控制如果现有文档和更新插入文档中都存在对象（不是数组），是否将合并对象。如果设置为false，则修补程序文档中的值将覆盖现有文档的值。如果设置为true，则对象将被合并。默认值为true。此选项仅控制更新插入行为。
% 请求正文（json）
% 单个文档的JSON表示形式。
% 从正文中给定的文档创建一个新文档，除非已经有给定_key的文档。如果未提供_key，则会自动生成一个新的唯一_key。
% 正文中可能始终给定的_id和_rev属性被忽略，URL部分或查询参数集合分别计数。
% 如果成功创建了文档，则Location标头将包含新创建的文档的路径。该Etag的头字段包含了文档的修订。两者都仅在单个文档的情况下设置。
% 如果silent未设置为true，则响应的主体包含具有以下属性的JSON对象：
% _id包含新创建的文档的文档标识符
% _key包含文档密钥
% _rev包含文档修订版
% 如果collection参数waitForSync为false，则该调用将在文档被接受后立即返回。直到文档同步到磁盘后，它才会等待。
% 可选地，即使已为整个集合禁用了waitForSync标志，查询参数waitForSync也可用于强制将文档创建操作同步到磁盘。因此，waitForSync查询参数可用于强制执行此特定操作的同步。要使用此功能，请将waitForSync参数设置为true。如果 未指定waitForSync参数或将其设置为false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync值为true。
% 如果查询参数returnNew为true，则对于每个生成的文档，将在结果中的new属性下返回完整的新文档。
% 返回码
% 201：如果文档创建成功并且waitForSync为true，则返回 。
% 202：如果文档创建成功并且waitForSync为false，则返回 。
% 400：如果正文不包含一个文档的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。
% 409：如果在索引属性中具有相同限定词的文档与现有文档发生冲突并因此违反了该唯一约束，则在单个文档的情况下返回409。在这种情况下，响应主体包含一个错误文档。
newDoc(PoolNameOrSocket, CollName, MapData) ->
   Path = <<"/_api/document/", CollName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

newDoc(PoolNameOrSocket, CollName, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 替换文档
% PUT /_api/document/{collection}/{key}
% 路径参数
% collection（必需）：要在其中替换文档的集合的名称。
% key（必填）：文档密钥。
% 查询参数
%     waitForSync（可选）：等待文档已同步到磁盘。
%     ignoreRevs（可选）：默认情况下，或者如果将其设置为true，则忽略给定文档中的_rev属性。如果将其设置为false，则将正文文档中给定的_rev属性作为前提。仅当当前版本是指定的版本时，才替换该文档。
%     returnOld（可选）：在结果的old属性下，还返回更改后的文档的完整先前修订。
%     returnNew（可选）： 在结果的new属性下还返回完整的新文档。
%     silent（可选）：如果设置为true，则将返回一个空对象作为响应。对于已替换的文档，不会返回任何元数据。此选项可用于节省一些网络流量。
% 标头参数
%     If-Match（可选）：您可以使用if-match HTTP标头有条件地根据目标修订版ID替换文档。
% 请求正文（json）
% 单个文档的JSON表示形式。
% 将指定的文档替换为正文中的文档，前提是存在这样的文档并且不违反任何前提条件。
% 该_key属性的值以及用作分片键的属性均不得更改。
% 如果指定了If-Match标头，并且数据库中文档的修订版与给定的修订版不相等，则将违反先决条件。
% 如果未给出If-Match且ignoreRevs为false，并且主体中存在_rev属性，并且其值与数据库中文档的修订版不匹配，则将违反先决条件。
% 如果违反了前提条件，则返回HTTP 412。
% 如果文档存在并且可以更新，则返回HTTP 201或HTTP 202（取决于waitForSync，请参见下文），Etag标头字段包含文档的新修订版，而Location标头包含完整的URL，在该URL下，可以查询文件。
% 仅限群集：替换文档可能包含集合的预定义分片键的值。分片键的值被视为提高性能的提示。如果分片键值不正确，ArangoDB可能会回答“ 未找到”错误。
% 可选地，即使已为整个集合禁用了waitForSync标志，查询参数waitForSync仍可用于强制将文档替换操作同步到磁盘。因此，waitForSync查询参数可用于强制特定操作的同步。要使用此功能，请将waitForSync参数设置为true。如果未指定waitForSync参数或将其设置为 false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync值为true。
% 如果silent未设置为true，则响应的主体包含一个JSON对象，其中包含有关标识符和修订版的信息。属性 _id包含更新的文档的已知文档ID，_key 包含用于唯一标识给定集合中的文档的键，而属性_rev包含新文档的修订版。
% 如果查询参数returnOld为true，那么将在结果的old属性下返回文档的完整先前修订版。
% 如果查询参数returnNew为true，那么将在结果中的new属性下返回完整的新文档。
% 如果该文档不存在，则返回HTTP 404，并且响应的正文包含错误文档。
% 返回码
% 201：如果成功替换了文档并且waitForSync为true，则返回 。
% 202：如果成功替换了文档并且waitForSync为false，则返回 。
% 400：如果正文不包含文档的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果找不到集合或文档，则返回。
% 412：如果违反了前提条件，则返回。该响应还将在_rev 属性中包含找到的文档的当前修订。此外，将返回属性_id和_key。
replaceDoc(PoolNameOrSocket, CollName, Key, MapData) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceDoc(PoolNameOrSocket, CollName, Key, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceDoc(PoolNameOrSocket, CollName, Key, MapData, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, Headers, BodyStr).

% 更新文档
% PATCH /_api/document/{collection}/{key}
% 路径参数
%     collection（必填）：要在其中更新文档的集合的名称。
%     key（必填）：文档密钥。
% 查询参数
% keepNull（可选）：如果要使用patch命令删除现有属性，则可以将URL查询参数keepNull与false一起使用。这将修改patch命令的行为，以从属性文档中删除属性文件值为null的现有文档。
% mergeObjects（可选）：控制如果现有文档和修补程序文档中都存在对象（不是数组），是否将合并对象。如果设置为false，则修补程序文档中的值将覆盖现有文档的值。如果设置为true，则对象将被合并。默认值为 true。
% waitForSync（可选）：等待文档已同步到磁盘。
% ignoreRevs（可选）：默认情况下，或者如果将其设置为true，则忽略给定文档中的_rev属性。如果将其设置为false，则将正文文档中给定的_rev属性作为前提。仅当当前版本是指定的版本时，文档才会更新。
% returnOld（可选）：在结果的old属性下，还返回更改后的文档的完整先前修订。
% returnNew（可选）： 在结果的new属性下还返回完整的新文档。
% silent（可选）：如果设置为true，则将返回一个空对象作为响应。更新后的文档将不返回任何元数据。此选项可用于节省一些网络流量。
% 标头参数
%     If-Match（可选）：您可以使用if-match HTTP标头有条件地根据目标修订版ID更新文档。
% 请求正文（json）
% 文档更新的JSON表示形式作为对象。
% 部分更新document-id标识的文档。请求的正文必须包含具有要修补的属性的JSON文档（补丁文档）。修补程序文档中的所有属性（如果尚不存在）将被添加到现有文档中，如果存在的话将被覆盖在现有文档中。
% 该_key属性的值以及用作分片键的属性均不得更改。
% 在补丁文档中将属性值设置为null会导致默认情况下为该属性保存null值。
% 如果指定了If-Match标头，并且数据库中文档的修订版与给定的修订版不相等，则将违反先决条件。
% 如果未给出If-Match且ignoreRevs为false，并且主体中存在_rev属性，并且其值与数据库中文档的修订版不匹配，则将违反先决条件。
% 如果违反了前提条件，则返回HTTP 412。
% 如果文档存在并且可以更新，则返回HTTP 201或HTTP 202（取决于waitForSync，请参见下文），Etag标头字段包含文档的新修订版（双引号），而Location标头包含一个可以查询文档的完整URL。
% 仅限群集：修补程序文档可能包含集合的预定义分片键的值。分片键的值被视为提高性能的提示。如果分片键值不正确，ArangoDB可能会以未发现的错误回答
% 可选地，即使已为整个集合禁用了waitForSync标志，查询参数waitForSync仍可用于强制将更新的文档操作同步到磁盘。因此，waitForSync查询参数可用于强制特定操作的同步。要使用此功能，请将waitForSync参数设置为true。如果未指定waitForSync参数或将其设置为 false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync值为true。
% 如果silent未设置为true，则响应的主体包含一个JSON对象，其中包含有关标识符和修订版的信息。属性 _id包含更新的文档的已知文档ID，_key 包含用于唯一标识给定集合中的文档的键，而属性_rev包含新文档的修订版。
% 如果查询参数returnOld为true，那么将在结果的old属性下返回文档的完整先前修订版。
% 如果查询参数returnNew为true，那么将在结果中的new属性下返回完整的新文档。
% 如果该文档不存在，则返回HTTP 404，并且响应的正文包含错误文档。
% 返回码
% 201：如果文档更新成功并且waitForSync为true，则返回 。
% 202：如果文档更新成功且waitForSync为false，则返回 。
% 400：如果正文不包含文档的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果找不到集合或文档，则返回。
% 412：如果违反了前提条件，则返回。该响应还将在_rev 属性中包含找到的文档的当前修订。此外，将返回属性_id和_key。
updateDoc(PoolNameOrSocket, CollName, Key, MapData) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateDoc(PoolNameOrSocket, CollName, Key, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateDoc(PoolNameOrSocket, CollName, Key, MapData, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, Headers, BodyStr).

% 删除文档
% DELETE /_api/document/{collection}/{key}
% 路径参数
% collection（必填）：要在其中删除文档的集合的名称。
% key（必填）：文档密钥。
% 查询参数
% waitForSync（可选）：等待删除操作已同步到磁盘。
% returnOld（可选）：在结果的old属性下，还返回更改后的文档的完整先前修订。
% silent（可选）：如果设置为true，则将返回一个空对象作为响应。对于已删除的文档，不会返回任何元数据。此选项可用于节省一些网络流量。
% 标头参数
% If-Match（可选）：您可以使用if-match HTTP标头有条件地根据目标修订版ID删除文档。
% 如果silent未设置为true，则响应的主体包含一个JSON对象，其中包含有关标识符和修订版的信息。属性 _id包含已删除文档的已知文档ID，_key 包含用于唯一标识给定集合中文档的键，属性_rev包含文档修订版。
% 如果未指定waitForSync参数或将其设置为false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync 的价值真。
% 如果查询参数returnOld为true，那么将在结果的old属性下返回文档的完整先前修订版。
% 返回码
% 200：如果文档已成功删除并且waitForSync为true，则返回 。
% 202：如果文档已成功删除并且waitForSync为false，则返回 。
% 404：如果找不到集合或文档，则返回。在这种情况下，响应主体包含一个错误文档。
% 412：如果“如果-match”标头或返回转，并给出找到的文件有不同的版本。响应还将在_rev属性中包含找到的文档的当前修订。此外，将返回属性_id和_key。
delDoc(PoolNameOrSocket, CollName, Key) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delDoc(PoolNameOrSocket, CollName, Key, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delDoc(PoolNameOrSocket, CollName, Key, QueryPars, Headers) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, Headers, undefined).

% 批量文件操作
% ArangoDB支持批量处理文档。批量操作影响 单个集合。使用此API变体可使客户端分摊整批文档中的单个请求的开销。不能保证批量操作可以串行执行，ArangoDB 可以并行执行这些操作。这可以转化为大幅的性能提升，尤其是在集群部署中。
% 如果在处理一个操作期间发生错误，ArangoDB将继续处理其余操作。错误将作为错误文档在响应正文中内联返回（有关更多详细信息，请参见下文）。此外，X-Arango-Error-Codes标头将包含发生的错误代码及其多重性的映射，例如 1205:10,1210:17，这意味着在10种情况下，错误1205 非法文档句柄；在17种情况下，错误1210 违反了唯一约束。。
% 通常，批量操作期望输入数组，并且结果主体将包含相同长度的JSON数组。

% 读取单个文档
% PUT /_api/document/{collection}#get
% 路径参数
%     collection（必需）：要从中读取文档的集合的名称。
% 查询参数
%     onlyget（必需）：该参数必须为true，否则将执行替换操作！
%     ignoreRevs（可选）：如果该值为true（默认值）：如果搜索文档包含_rev字段的值，则仅当文档具有相同的修订值时才返回该文档。否则，将返回前提条件失败错误。
% 返回正文对象中由_key标识的文档。请求的正文必须包含字符串（要查找的_key值）或搜索文档的JSON数组。
% 搜索文档必须至少包含_key字段的值。一种用于值_rev 可被指定以验证文档是否具有相同的修订值，除非ignoreRevs设置为false。
% 仅限群集：搜索文档可能包含集合的预定义分片键的值。分片键的值被视为提高性能的提示。如果分片键值不正确，ArangoDB可能会回答“ 未找到”错误。
% 返回的文档数组包含三个特殊属性：_id包含文档标识符，_key包含唯一标识给定集合中的文档的键，_rev包含修订版。
% 返回码
% 200：如果没有发生错误则返回
% 400：如果正文不包含文档数组的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果找不到集合，则返回。
% 对于该操作的返回 列表 如果文档不存在 或者_rev条件不满足 则返回列表的中包含相关的错误 可能需要在使用的时候过滤正确和非正确的返回文档
getDocs(PoolNameOrSocket, CollName, KeyOrMapDataList) ->
   QueryBinary = agMiscUtils:spellQueryPars([{onlyget, true}]),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(KeyOrMapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

getDocs(PoolNameOrSocket, CollName, KeyOrMapDataList, QueryPars) ->
   LastQueryPars =
      case lists:keyfind(onlyget, 1, QueryPars) of
         {onlyget, true} ->
            QueryPars;
         _ ->
            lists:keystore(onlyget, 1, QueryPars, {onlyget, true})
      end,
   QueryBinary = agMiscUtils:spellQueryPars(LastQueryPars),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(KeyOrMapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 创建多个文档
% POST /_api/document/{collection}#multiple
% 路径参数
%     collection（必填）：要在其中创建文档的集合的名称。
% 查询参数
%     collection（可选）：集合的名称。这仅是为了向后兼容。在ArangoDB版本<3.0中，URL路径为/ _api / document，并且此查询参数是必需的。这种组合仍然有效，但是建议的方法是在URL路径中指定集合。
%     waitForSync（可选）：等待文档已同步到磁盘。
%     returnNew（可选）：另外 ，在结果的new属性下返回完整的新文档。
%     returnOld（可选）：另外 ，在结果的old属性下返回完整的旧文档。仅在使用覆盖选项时可用。
%     silent（可选）：如果设置为true，则将返回一个空对象作为响应。创建的文档将不返回任何元数据。此选项可用于节省一些网络流量。
%     overwrite（可选）：如果设置为true，则插入将成为替换插入。如果已经存在具有相同_key的文档，则不会由于违反唯一约束而拒绝新文档，而是将替换旧文档。
% 请求正文（json）
% 要创建的文档数组。
% 从正文中给定的文档创建新文档，除非已经有给定_key的文档。如果未提供_key，则会自动生成一个新的唯一_key。
% 结果主体将包含一个长度与输入数组相同的JSON数组，并且每个条目都包含对应输入的运算结果。如果发生错误，则该条目是一个文档，其属性error设置为true，errorCode设置为发生的错误代码。
% 正文中可能始终给定的_id和_rev属性被忽略，URL部分或查询参数集合分别计数。
% 如果silent未设置为true，则响应的主体包含具有以下属性的JSON对象数组：
% _id包含新创建的文档的文档标识符
% _key包含文档密钥
% _rev包含文档修订版
% 如果collection参数waitForSync为false，则在文档被接受后，调用将立即返回。直到文档同步到磁盘后，它才会等待。
% 可选地，即使已为整个集合禁用了waitForSync标志，查询参数waitForSync也可用于强制将文档创建操作同步到磁盘。因此，waitForSync查询参数可用于强制执行此特定操作的同步。要使用此功能，请将waitForSync参数设置为true。如果 未指定waitForSync参数或将其设置为false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync值为true。
% 如果查询参数returnNew为true，则对于每个生成的文档，将在结果中的new属性下返回完整的新文档。
% 应与一些附加的HTTP头的文件已经发生了错误的X阿朗戈-差错代码：被设置，其中包含一个地图，与它们的重数一起发生的错误代码，如在1205：10,1210：17，其意味着在10种情况下发生了错误1205“非法文档句柄”，在17种情况下发生了错误1210“违反唯一约束”。
% 返回码
% 201：如果waitForSync为true，并且已处理操作，则返回。
% 202：如果waitForSync为false并且已处理操作，则返回。
% 400：如果正文不包含文档数组的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果collection指定的collection未知，则返回。在这种情况下，响应主体包含一个错误文档。
% 按照MapDataList的顺序返回执行结果 正确或者错误
newDocs(PoolNameOrSocket, CollName, MapDataList) ->
   Path = <<"/_api/document/", CollName/binary>>,
   BodyStr = jiffy:encode(MapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

newDocs(PoolNameOrSocket, CollName, MapDataList, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 替换多个文件
% PUT /_api/document/{collection}
% 路径参数
%     collection（必填）：此URL参数是替换文档的集合的名称。
% 查询参数
%     waitForSync（可选）：等待新文档同步到磁盘。
%     ignoreRevs（可选）：默认情况下，或者如果将其设置为true，则忽略给定文档中的_rev属性。如果将其设置为false，则将正文文档中给定的_rev属性作为前提。仅当当前版本是指定的版本时，才替换该文档。
%     returnOld（可选）：在结果的old属性下还返回更改后的文档的完整先前修订。
%     returnNew（可选）： 在结果的new属性下还返回完整的新文档。
% 请求正文（json）
% 文档数组的JSON表示形式。
% 用主体中的文档替换指定集合中的多个文档，替换后的文档由 主体文档中的_key属性指定。
% 该_key属性的值以及用作sharding keys的属性均不得更改。
% 如果ignoreRevs为false，并且正文中的文档中存在_rev属性，并且其值与数据库中相应文档的修订版不匹配，则将违反先决条件。
% 仅限群集：替换文档可能包含集合的预定义分片键的值。分片键的值被视为提高性能的提示。如果分片键值不正确，ArangoDB可能会回答“ 未找到”错误。
% 可选地，即使已为整个集合禁用了waitForSync标志，查询参数waitForSync仍可用于强制将文档替换操作同步到磁盘。因此，waitForSync查询参数可用于强制特定操作的同步。要使用此功能，请将waitForSync参数设置为true。如果未指定waitForSync参数或将其设置为 false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync值为true。
% 响应的主体包含一个与输入数组长度相同的JSON数组，其中包含有关标识符和替换文档的修订版的信息。在每个条目中，属性 _id包含每个更新文档的已知文档ID， _key包含用于唯一标识给定集合中的文档的键，而属性_rev包含新文档修订版。如果发生错误或违反先决条件，则会构建一个错误对象，该错误对象的属性error设置为true，属性 errorCode设置为错误代码。
% 如果查询参数returnOld为true，则对于每个生成的文档，将在结果的old属性下返回文档的完整先前修订版。
% 如果查询参数returnNew为true，则对于每个生成的文档，将在结果中的new属性下返回完整的新文档。
% 请注意，如果违反任何前提条件或某些文档发生错误，则返回代码仍为201或202，但是会设置其他HTTP标头X-Arango-Error-Codes，其中包含错误代码的映射，与它们的多重性一起发生，如：1200：17,1205：10，这意味着在17种情况下发生了错误1200“修订冲突”，在10种情况下发生了错误1205“非法文档句柄”。
% 返回码
% 201：如果waitForSync为true，并且已处理操作，则返回。
% 202：如果waitForSync为false并且已处理操作，则返回。
% 400：如果正文不包含文档数组的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果找不到集合，则返回。
replaceDocs(PoolNameOrSocket, CollName, MapDataList) ->
   Path = <<"/_api/document/", CollName/binary>>,
   BodyStr = jiffy:encode(MapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

replaceDocs(PoolNameOrSocket, CollName, MapDataList, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 更新多个文件
% PATCH /_api/document/{collection}
% 路径参数
% collection（必填）：要在其中更新文档的集合的名称。
% 查询参数
% keepNull（可选）：如果要使用patch命令删除现有属性，则可以将URL查询参数keepNull与false一起使用。这将修改patch命令的行为，以从属性文档中删除属性文件值为null的现有文档。
% mergeObjects（可选）：控制如果现有文档和修补程序文档中都存在对象（不是数组），是否将合并对象。如果设置为false，则修补程序文档中的值将覆盖现有文档的值。如果设置为true，则对象将被合并。默认值为 true。
% waitForSync（可选）：等待新文档同步到磁盘。
% ignoreRevs（可选）：默认情况下，或者如果将其设置为true，则忽略给定文档中的_rev属性。如果将其设置为false，则将正文文档中给定的_rev属性作为前提。仅当当前版本是指定的版本时，文档才会更新。
% returnOld（可选）：在结果的old属性下还返回更改后的文档的完整先前修订。
% returnNew（可选）： 在结果的new属性下还返回完整的新文档。
% 请求正文（json）
% 文档更新数组的JSON表示形式是对象。
% 部分更新文档，要更新的文档由主体对象中的_key属性指定。请求的正文必须包含文档更新的JSON数组，其中包含要修补的属性（补丁文档）。修补程序文档中的所有属性（如果尚不存在）将被添加到现有文档中，如果存在的话将被覆盖在现有文档中。
% 该_key属性的值以及用作分片键的属性均不得更改。
% 在补丁文档中将属性值设置为null会导致默认情况下为该属性保存null值。
% 如果ignoreRevs为false，并且正文中的文档中存在_rev属性，并且其值与数据库中相应文档的修订版不匹配，则将违反先决条件。
% 仅限群集：修补程序文档可能包含集合的预定义分片键的值。分片键的值被视为提高性能的提示。如果分片键值不正确，ArangoDB可能会以未发现的错误回答
% 可选地，即使已为整个集合禁用了waitForSync标志，查询参数waitForSync仍可用于强制将文档替换操作同步到磁盘。因此，waitForSync查询参数可用于强制特定操作的同步。要使用此功能，请将waitForSync参数设置为true。如果未指定waitForSync参数或将其设置为 false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync值为true。
% 响应的主体包含一个与输入数组长度相同的JSON数组，其中包含有关标识符和已更新文档的修订的信息。在每个条目中，属性 _id包含每个更新文档的已知文档ID， _key包含用于唯一标识给定集合中的文档的键，而属性_rev包含新文档修订版。如果发生错误或违反先决条件，则会构建一个错误对象，该错误对象的属性error设置为true，属性 errorCode设置为错误代码。
% 如果查询参数returnOld为true，则对于每个生成的文档，将在结果的old属性下返回文档的完整先前修订版。
% 如果查询参数returnNew为true，则对于每个生成的文档，将在结果中的new属性下返回完整的新文档。
% 请注意，如果违反任何前提条件或某些文档发生错误，则返回代码仍为201或202，但是会设置其他HTTP标头X-Arango-Error-Codes，其中包含错误代码的映射，与它们的多重性一起发生，如：1200：17,1205：10，这意味着在17种情况下发生了错误1200“修订冲突”，在10种情况下发生了错误1205“非法文档句柄”。
% 返回码
% 201：如果waitForSync为true，并且已处理操作，则返回。
% 202：如果waitForSync为false并且已处理操作，则返回。
% 400：如果正文不包含文档数组的有效JSON表示形式，则返回。在这种情况下，响应主体包含一个错误文档。
% 404：如果找不到集合，则返回。
updateDocs(PoolNameOrSocket, CollName, MapDataList) ->
   Path = <<"/_api/document/", CollName/binary>>,
   BodyStr = jiffy:encode(MapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

updateDocs(PoolNameOrSocket, CollName, MapDataList, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

% 删除多个文件
% DELETE /_api/document/{collection}
% 路径参数
%     collection（必填）：从中删除文档的集合。
% 查询参数
%     waitForSync（可选）：等待删除操作已同步到磁盘。
%     returnOld（可选）：在结果的old属性下，还返回更改后的文档的完整先前修订。
%     ignoreRevs（可选）：如果设置为true，则忽略选择器中的任何_rev属性。不执行任何修订检查。
% 请求正文（json）
% 字符串或文档的JSON数组。
% 请求的主体是一个由文档选择器组成的数组。选择器可以是带键的字符串，带文档标识符的字符串或带_key属性的对象。此API调用从collection中删除所有指定的文档。如果选择器是一个对象并具有_rev属性，则前提是集合中已删除文档的实际修订版是指定的。
% 响应的主体是一个与输入数组长度相同的数组。对于每个输入选择器，输出都包含一个JSON对象，其中包含有关操作结果的信息。如果未发生错误，则构建一个对象，其中属性_id包含已删除文档的已知文档ID，_key包含用于唯一标识给定集合中的文档的键，而属性_rev包含文档修订版。在发生错误的情况下，与属性的对象错误设置为真和 的errorCode集到的错误代码被构建的。
% 如果未指定waitForSync参数或将其设置为false，则将应用集合的默认waitForSync行为。该waitForSync查询参数不能用于禁用同步用于具有默认集合waitForSync 的价值真。
% 如果查询参数returnOld为true，那么将在结果的old属性下返回文档的完整先前修订版。
% 请注意，如果违反任何先决条件或某些文档发生错误，则返回代码仍为200或202，但是会设置其他HTTP标头X-Arango-Error-Codes，其中包含错误代码的映射，与它们的多重性一起发生，如：1200：17,1205：10，这意味着在17种情况下发生了错误1200“修订冲突”，在10种情况下发生了错误1205“非法文档句柄”。
% 返回码
% 200：如果waitForSync为true，则返回。
% 202：如果waitForSync为false，则返回。
% 404：如果找不到集合，则返回。在这种情况下，响应主体包含一个错误文档。
delDocs(PoolNameOrSocket, CollName, KeyOrMapDataList) ->
   Path = <<"/_api/document/", CollName/binary, "/">>,
   BodyStr = jiffy:encode(KeyOrMapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], BodyStr).

delDocs(PoolNameOrSocket, CollName, KeyOrMapDataList, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/document/", CollName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(KeyOrMapDataList),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], BodyStr).
