-module(agBulkImportExport).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/bulk-imports.html

% 批量导入的HTTP接口
% ArangoDB提供了一个HTTP接口，可以一次将多个文档导入一个集合中。这称为批量导入。
% 上传的数据必须以JSON格式提供。有两种导入数据的机制：
% 自包含的JSON文档：在这种情况下，每个文档都包含所有属性名称和值。在上传的文档中，属性名称可能完全不同
% 属性名称加上文档数据：在这种情况下，第一个数组必须包含后面文档的属性名称。以下数组仅包含属性值。属性值将按位置映射到属性名称。
% 两种输入机制的端点地址均为/ _api / import。必须使用HTTP POST请求将数据发送到此URL。要导入的数据必须包含在POST请求的正文中。
% 该集合的查询参数必须用于指定目标集合导入。将数据导入到不存在的集合中将产生错误。
% 可以将waitForSync查询参数设置为true，以仅在所有文档都已同步到磁盘后才返回导入。
% 如果任何上传的文档无效并且无法导入，可以将complete查询参数设置为true，以使整个导入失败。在这种情况下，即使在导入结束时发生故障，导入操作也不会导入任何文档。
% 如果complete具有除true以外的其他值，则将导入有效文档，而拒绝无效文档，这意味着可能仅导入了一些上载文档。
% 可以将details查询参数设置为true，以使导入API返回有关无法导入的文档的详细信息。如果details为true，则结果还将包含一个details属性，该属性是详细错误消息的数组。如果将详细信息设置为false或省略，则不会返回任何详细信息。

% 从JSON编码的列表中导入文档
% POST /_api/import#document
% 查询参数
%    collection （必填）：集合名称。
%    fromPrefix（可选）：_from属性中值的可选前缀。如果指定，该值将自动添加到每个_from输入值之前。这样就可以仅指定的键_from。
%    toPrefix（可选）：_to属性中值的可选前缀。如果指定，该值将自动添加到每个_to输入值之前。这样就可以仅指定的键_to。
%    overwrite （可选）：如果此参数的值为true或yes，则将在导入之前删除集合中的所有数据。请注意，任何现有的索引定义都将保留。
%    waitForSync（可选）：等待文档同步到磁盘后再返回。
%    onDuplicate（可选）：控制在违反唯一键约束的情况下执行的操作。可能的值为：
%       error：由于违反唯一键约束，因此不会导入当前文档。这是默认设置。
%       update：这将使用请求中指定的数据更新数据库中的现有文档。请求中不存在的现有文档的属性将被保留。
%       replace：这将用请求中指定的数据替换数据库中的现有文档。
%       ignore：这不会更新现有文档，而只是忽略由唯一键约束冲突引起的错误。
% 请注意，仅当请求中的导入文档包含_key属性时，update，replace和ignore才起作用。由于次要唯一键约束冲突，更新和 替换也可能失败。
%
%    complete （可选）：如果设置为true或yes，则如果发生任何错误，将使整个导入失败。否则，即使无法导入某些文档，导入也将继续。
%    details（可选）：如果设置为true或yes，结果将包括一个属性，details 其中包含有关无法导入的文档的详细信息。
%
% 请求正文（字符串）
% 主体必须由JSON编码的属性值数组组成，每个文档一行。请求的第一行必须是JSON编码的属性名称数组。这些属性名称用于后续各行中的数据。
% 在由标识的集合中创建文档collection-name。请求正文的第一行必须包含一个JSON编码的属性名称数组。请求正文中的以下所有行都必须包含JSON编码的属性值数组。每行都被解释为一个单独的文档，并且指定的值将映射到在第一标题行中指定的属性名称的数组。
%
% 响应是具有以下属性的JSON对象：
%    created：导入的文件数。
%    errors：由于错误而未导入的文档数。
%    empty：在输入中找到的空行数（类型documents或只能包含大于零的值auto）。
%    updated：更新/替换的文档数（如果onDuplicate 设置为update或replace）。
%    ignored：失败但被忽略的插入操作数（如果 onDuplicate设置为ignore）。
%    details：如果查询参数details设置为true，则结果将包含一个details属性，该属性是一个数组，其中包含有关无法插入哪些文档的更多详细信息。
%
% 返回码
%    201：如果可以成功导入所有文档，则返回。
%    400：如果type包含无效值，未collection指定no ，文档编码错误或请求格式错误，则返回。
%    404：如果collection或导入边的_from或_to属性引用未知集合，则返回。
%    409：如果导入会触发唯一键冲突，complete则返回，并将 其设置为true。
%    500：如果服务器无法为没有用户定义密钥的文档自动生成文档密钥（密钥错误），则返回500。
docImport(PoolNameOrSocket, ListOfList, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/import", QueryBinary/binary>>,
   BodyStr = <<<<(jiffy:encode(OneList))/binary, "\n">> || OneList <- ListOfList>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 从JSON导入文档
% POST /_api/import#json
% 查询参数
%    type （必填）：确定如何解释请求的正文。type可以具有以下值：
%       documents：使用此类型时，请求正文中的每一行都应为单独的JSON编码的文档。请求主体中的多个JSON对象需要用换行符分隔。
%       list：使用此类型时，请求主体必须包含要导入的单个对象的单个JSON编码数组。
%       auto：如果设置，它将自动确定主体类型（ documents或list）。
%    collection （必填）：集合名称。
%    fromPrefix（可选）：_from属性中值的可选前缀。如果指定，该值将自动添加到每个_from输入值之前。这样就可以仅指定的键_from。
%    toPrefix（可选）：_to属性中值的可选前缀。如果指定，该值将自动添加到每个_to输入值之前。这样就可以仅指定的键_to。
%    overwrite （可选）：如果此参数的值为true或yes，则将在导入之前删除集合中的所有数据。请注意，任何现有的索引定义都将保留。
%    waitForSync（可选）：等待文档同步到磁盘后再返回。
%    onDuplicate（可选）：控制在违反唯一键约束的情况下执行的操作。可能的值为：
%       error：由于违反唯一键约束，因此不会导入当前文档。这是默认设置。
%       update：这将使用请求中指定的数据更新数据库中的现有文档。请求中不存在的现有文档的属性将被保留。
%       replace：这将用请求中指定的数据替换数据库中的现有文档。
%       ignore：这不会更新现有文档，而只是忽略由唯一键约束冲突引起的错误。
%    请注意，仅当请求中的导入文档包含_key属性时，update，replace和ignore才起作用。由于次要唯一键约束冲突，更新和 替换也可能失败。
%    complete （可选）：如果设置为true或yes，则如果发生任何错误，将使整个导入失败。否则，即使无法导入某些文档，导入也将继续。
%    details（可选）：如果设置为true或yes，结果将包括一个属性，details 其中包含有关无法导入的文档的详细信息。
% 请求正文（字符串）
% 主体必须是JSON编码的对象数组，或者是包含多个以换行符分隔的JSON对象的字符串。
% 在由标识的集合中创建文档collection-name。文档的JSON表示形式必须作为POST请求的主体传递。请求主体可以由多行组成，每行都是一个独立的JSON对象，也可以是包含子对象的JSON数组。
%
% 响应是具有以下属性的JSON对象：
%    created：导入的文件数。
%    errors：由于错误而未导入的文档数。
%    empty：在输入中找到的空行数（类型documents或只能包含大于零的值auto）。
%    updated：更新/替换的文档数（如果onDuplicate 设置为update或replace）。
%    ignored：失败但被忽略的插入操作数（如果 onDuplicate设置为ignore）。
%    details：如果查询参数details设置为true，则结果将包含一个details属性，该属性是一个数组，其中包含有关无法插入哪些文档的更多详细信息。
% 返回码
%    201：如果可以成功导入所有文档，则返回。
%    400：如果type包含无效值，未collection指定no ，文档编码错误或请求格式错误，则返回。
%    404：如果collection或导入边的_from或_to属性引用未知集合，则返回。
%    409：如果导入会触发唯一键冲突，complete则返回，并将 其设置为true。
%    500：如果服务器无法为没有用户定义密钥的文档自动生成文档密钥（密钥错误），则返回500。
jsonImport(PoolNameOrSocket, MapDataList, QueryPars) ->
   case lists:keyfind(type, 1, QueryPars) of
      {type, list} ->
         BodyStr = jiffy:encode(MapDataList);
      {type, documents} ->
         BodyStr = <<<<(jiffy:encode(OneList))/binary, "\n">> || OneList <- MapDataList>>;
      _ ->
         BodyStr = MapDataList
   end,
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/import", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 说明------->
% 导入自包含的JSON文档
% 此导入方法允许上传自包含的JSON文档。这些文档必须上传到HTTP POST请求的正文中。正文的每一行都将被解释为一个独立文档。正文中的空行是允许的，但将被跳过。使用此格式，文档将逐行导入。
%
% 输入数据示例：{“ _key”：“ key1”，……} {“ _key”：“ key2”，……}…
%
% 要使用此方法，应将类型查询参数设置为documents。
%
% 还可以上传嵌入到JSON数组中的自包含JSON文档。数组中的每个元素都将被视为文档并导入。
%
% 这种情况下的示例输入数据：
%
% [
% { "_key": "key1", ... },
% { "_key": "key2", ... },
% ...
% ]
% 此格式不需要每个文档都位于单独的行上，并且JSON数据中允许使用任何空格。它可以用于将JSON格式的结果数组（例如，从arangosh中）导入回ArangoDB中。使用此格式需要ArangoDB解析整个数组，并在导入期间将其保留在内存中。这可能比逐行处理要消耗更多的资源。
%
% 要使用此方法，类型查询参数应设置为array。
%
% 将类型查询参数设置为auto将使服务器自动检测数据是按行的JSON文档（类型=文档）还是JSON数组（类型=数组）。
%
% 例子
%
% curl --data-binary @- -X POST --dump - "http://localhost:8529/_api/import?type=documents&collection=test"
% { "name" : "test", "gender" : "male", "age" : 39 }
% { "type" : "bird", "name" : "robin" }
%
% HTTP/1.1 201 Created
% Server: ArangoDB
% Connection: Keep-Alive
% Content-type: application/json; charset=utf-8
%
% {"error":false,"created":2,"empty":0,"errors":0}
% 如果一切顺利，服务器将以HTTP 201响应。导入的文档数将在响应的创建的属性中返回。如果任何文档被跳过或格式错误，将在errors属性中返回该文档。响应中还将有一个空属性，其中包含值为0。
%
% 如果在请求中将details参数设置为true，则响应还将包含一个属性details，该属性是有关导入期间在服务器端发生的错误的详细信息的数组。如果没有发生错误，则此数组可能为空。

% 导入标题和值
% 使用这种类型的导入时，要与实际文档值数据分开指定要导入的文档的属性名称。HTTP POST请求正文的第一行必须是JSON数组，其中包含后面文档的属性名称。以下各行被解释为文档数据。每个文档必须是值的JSON数组。此数据部分中不需要或不允许属性名称。
% 例子
% curl --data-binary @- -X POST --dump - "http://localhost:8529/_api/import?collection=test"
% [ "firstName", "lastName", "age", "gender" ]
% [ "Joe", "Public", 42, "male" ]
% [ "Jane", "Doe", 31, "female" ]
%
% HTTP/1.1 201 Created
% Server: ArangoDB
% Connection: Keep-Alive
% Content-type: application/json; charset=utf-8
%
% {"error":false,"created":2,"empty":0,"errors":0}
% 如果一切顺利，服务器将再次以HTTP 201响应。导入的文档数将在响应的创建的属性中返回。如果任何文档被跳过或格式错误，将在errors属性中返回该文档。输入文件中的空行数将在empty属性中返回。
%
% 如果在请求中将details参数设置为true，则响应还将包含一个属性details，该属性是有关导入期间在服务器端发生的错误的详细信息的数组。如果没有发生错误，则此数组可能为空。
%
% 导入到边缘集合
% 请注意，将文档导入 边缘集合时，必须强制所有导入的文档都包含_from和_to属性，并且这些属性必须包含对现有集合的引用。

% 批处理请求的HTTP接口
% 客户端通常在单独的HTTP请求中向ArangoDB发送单独的操作。这是直接且简单的，但是具有以下缺点：如果连续发出许多小请求，则网络开销可能会很大。
% 为了缓解此问题，ArangoDB提供了一个批处理请求API，客户端可以使用该API批量向ArangoDB发送多个操作。当客户端必须以较小的正文/有效负载发送许多HTTP请求并且各个请求的结果彼此不依赖时，此方法特别有用。
% 客户端可以通过向URL / _api / batch处理程序发出多部分HTTP POST请求来使用ArangoDB的批处理API 。如果Content-type为multipart / form-data，则处理程序将接受请求并指定边界字符串。然后，ArangoDB将使用此边界将批处理请求分解为各个部分。这也意味着边界字符串本身不能包含在任何部分中。当ArangoDB将多部分请求分为其各个部分时，它将按顺序处理所有部分，就好像它是一个独立的请求一样。处理完所有零件后，ArangoDB将生成一个多部分HTTP响应，其中每个零件操作结果都包含一个零件。例如，如果您发送包含5个部分的多部分请求，则ArangoDB还将发送包含5个部分的多部分响应。
% 服务器希望每个零件消息均以以下“头”开头：
% Content-type: application/x-arango-batchpart
% 您可以选择指定Content-Id “标头”以唯一标识每个零件消息。如果已指定，服务器将在其响应中返回Content-Id。否则，服务器将不会发回Content-Id“标头”。服务器将不会验证Content-Id的唯一性。在强制性Content-type和可选Content-Id标头之后，必须紧跟两个Windows换行符（即\ r \ n \ r \ n）。该结构的任何偏差都可能导致零件被拒绝或错误解释。零件请求有效载荷（格式为常规HTTP请求）必须直接遵循两个Windows换行符。
% 请注意，从 技术上来说，文字Content-type：application / x-arango-batchpart是MIME部分的标头，而HTTP请求（包括其标头）是MIME部分的正文部分。
% 实际的零件请求应以通常的HTTP方法，被调用的URL和HTTP协议版本开头，后跟任意的HTTP标头。它的主体应遵循通常的\ r \ n \ r \ n 文字。因此，部分请求是常规的HTTP请求，仅嵌入在多部分消息中。
% 以下示例将发送具有3个单独文档创建操作的批处理。在此示例中使用的边界是 XXXsubpartXXX。
% *******************************************************************
%                批处理请求的HTTP接口 改功能不予实现支持
% *******************************************************************

% 使用光标导出集合中的所有文档
% POST /_api/export
% 查询参数
%    collection （必填）：要导出的集合的名称。
% 具有以下属性的JSON对象是必需的：
%    flush：如果设置为true，则将在导出之前执行WAL刷新操作。刷新操作将开始将文档从WAL复制到集合的数据文件中。刷新后还会有最长为flushWait秒的额外等待时间，以使WAL收集器也可以更改调整后的文档元数据以指向数据文件。默认值为false（即不刷新），因此导出中可能缺少集合中最新插入或更新的文档。
%    flushWait：刷新操作后的最大等待时间（以秒为单位）。默认值为10。仅当flush设置为true时，此选项才有效。
%    count：布尔值标志，指示是否应在结果的“ count”属性中返回结果集中的文档数（可选）。计算“ count”属性将来可能会对性能产生影响，因此默认情况下将关闭此选项，并且仅在请求时才返回“ count”。
%    batchSize：一次往返（从服务器到客户端）要传输的最大结果文档数（可选）。如果未设置此属性，则将使用服务器控制的默认值。
%    limit：可选的限制值，确定要包含在光标中的最大文档数。省略limit属性或将其设置为0将导致不使用任何限制。如果使用限制，则不确定集合中的哪些文档将包含在导出中，哪些文档将排除在外。这是因为集合中没有自然的文档顺序。
%    ttl：光标的可选生存时间（以秒为单位）。在指定的时间后，游标将自动在服务器上删除。这对于确保客户端不完全获取的游标的垃圾回收很有用。如果未设置，将使用服务器定义的值。
%    restrict：包含以下属性的对象，返回结果文档时将包含或排除这些属性名称。默认情况下，不指定 限制将返回每个文档的所有属性。
%       type：必须根据要使用的类型设置为 include or exclude
%       fields：包含要包含或排除的属性名称的数组。包含或排除属性名称的匹配将仅在顶层完成。目前不支持指定嵌套属性的名称。
%    对此方法的调用将创建一个游标，其中包含指定集合中的所有文档。与其他数据生成API相比，导出API生成的内部数据结构更轻便，因此这是从集合中检索所有文档的首选方法。
%    以与/_api/cursorREST API中相似的方式返回文档。如果集合的所有文档都适合第一批，则不会创建任何游标，并且结果对象的hasMore属性将设置为 false。如果不是所有文档都适合第一批文档，则结果对象的hasMore属性将设置为true，并且结果的id属性将包含游标id。
% 未指定文件退回的顺序。
%
% 默认情况下，将仅返回集合中存储在集合数据文件中的那些文档。运行导出时在预写日志（WAL）中存在的文档将不会导出。
% 为了也导出这些文档，调用者可以在调用导出API或设置flush属性之前发出WAL刷新请求。设置冲洗 选项将在导出之前触发WAL冲洗，以便将文档从WAL复制到集合数据文件。
% 如果服务器可以创建结果集，则服务器将使用HTTP 201进行响应 。响应的主体将包含带有结果集的JSON对象。
% 返回的JSON对象具有以下属性：
% error：布尔值标志，指示发生错误（ 在这种情况下为false）
% code：HTTP状态码
% result：结果文档数组（如果集合为空，则可能为空）
% hasMore：一个布尔指示器，指示服务器上的光标是否还有更多结果可用
% count：可用结果文档总数（仅当查询是在设置了count属性的情况下执行的）
% id：在服务器上创建的临时光标的ID（可选，请参见上文）
% 如果JSON格式不正确或请求中缺少查询规范，则服务器将使用HTTP 400进行响应。
% 响应的主体将包含带有其他错误详细信息的JSON对象。该对象具有以下属性：
% error：布尔值标志，指示发生错误（在这种情况下为true）
% code：HTTP状态码
% errorNum：服务器错误号
% errorMessage：描述性错误消息
% 客户端应该始终尽早删除导出游标结果，因为缠结的导出游标会阻止基础集合被压缩或卸载。默认情况下，未使用的游标将在服务器定义的空闲时间后自动删除，并且客户端可以通过设置ttl值来调整此空闲时间。
% 注意：群集协调器当前不支持此API。
% 返回码
%    201：如果服务器可以创建结果集，则返回。
%    400：如果JSON表示格式错误或请求中缺少查询规范，则返回。
%    404：如果查询中访问了不存在的集合，服务器将以HTTP 404进行响应。
%    405：如果使用了不受支持的HTTP方法，则服务器将以HTTP 405进行响应。
%    501：如果在群集协调器上调用此API，则服务器将使用HTTP 501进行响应。
docExport(PoolNameOrSocket, CollName, MapData) ->
   Path = <<"/_api/export?collection=", CollName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).