-module(agAqls).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:
%     AQL Query Cursors:
%           https://www.arangodb.com/docs/stable/http/aql-query-cursor.html
%     AQL Query:
%           https://www.arangodb.com/docs/stable/http/aql-query-cursor.html
%     AQL Query Results Cache:
%           https://www.arangodb.com/docs/stable/http/aql-query-cursor.html
%     AQL User Functions Management:
%           https://www.arangodb.com/docs/stable/http/aql-query-cursor.html
% 该模块汇总封装上面所有AQL操作

% AQL查询游标的HTTP接口
% 这是ArangoDB查询的HTTP接口的简介。AQL的结果和简单查询作为游标返回，以便批量处理服务器与客户端之间的通信。每次调用将返回一批文档，并指示当前批是否为最终批。根据查询的不同，结果集中的文档总数可能会或可能不会事先知道。为了释放服务器资源，客户端应在不再需要游标时将其删除。
% 要执行查询，需要通过HTTP POST请求将查询详细信息从客户端传送到服务器。

% 检索查询结果
% 选择查询在服务器上即时执行，结果集将返回给客户端。
% 客户端可以通过两种方式从服务器获取结果集：
%    单次返回
%    使用游标

% 单次返回
% 服务器将在一次往返中仅将一定数量的结果文档传送回客户端。客户端可以通过在发出查询时设置batchSize属性来控制此数字。
% 如果完整的结果可以一次性传递给客户端，则客户端不需要发出任何其他请求。客户端可以通过检查结果集的hasMore属性来检查是否已检索到完整的结果集。如果将其设置为false，则客户端已从服务器获取了完整的结果集。在这种情况下，将不会创建服务器端游标。

% 使用游标
% 如果结果集中包含的文档数量超出单次往返传输的文档数量（即通过batchSize属性设置的数量），则服务器将返回前几个文档并创建一个临时游标。游标标识符也将返回给客户端。服务器会将光标标识符放在响应对象的id属性中。此外， 响应对象的hasMore属性将设置为true。这表明客户端还有其他结果要从服务器获取。

% 通过HTTP访问游标
% 创建光标
% 创建一个游标并返回第一个结果
% POST /_api/cursor
% 描述查询和查询参数的JSON对象。
% 具有以下属性的JSON对象是必需的：
%    query：包含要执行的查询字符串
%    count：指示是否应在结果的“ count”属性中返回结果集中的文档数。计算“ count”属性将来可能会对某些查询产生性能影响，因此默认情况下此选项处于关闭状态，并且仅在请求时返回“ count”。
%    batchSize：一次往返从服务器传输到客户端的最大结果文档数。如果未设置此属性，则将使用服务器控制的默认值。甲BATCHSIZE的值 0是不允许的。
%    ttl：光标的生存时间（以秒为单位）。在指定的时间后，游标将自动在服务器上删除。这对于确保客户端不完全获取的游标的垃圾回收很有用。如果未设置，则将使用服务器定义的值（默认值：30秒）。
%    cache：用于确定是否应使用AQL查询结果缓存的标志。如果设置为false，那么将跳过查询的任何查询缓存。如果设置为真，这将导致被检查的查询缓存为查询，如果查询缓存模式是上还是需求。
%    memoryLimit：允许查询使用的最大内存数（以字节为单位）。如果设置，则查询将分配过多的内存而失败，并显示错误“超出资源限制”。值为0表示没有内存限制。
%    bindVars：表示绑定参数的键/值对。
%    options：键/值对象，带有用于查询的其他选项。
%       FULLCOUNT：如果设置为真，并在查询中包含LIMIT子句，那么结果将有一个额外的与子属性属性统计 和FULLCOUNT，{ ... , "extra": { "stats": { "fullCount": 123 } } }。该FULLCOUNT属性将包含的文档数量的结果应用于在查询的最后顶层限制之前。它可用于计算符合特定过滤条件的文档数量，但一次只能返回其中的一部分。因此，它类似于MySQL的SQL_CALC_FOUND_ROWS暗示。请注意，设置该选项将禁用一些LIMIT优化，并可能导致处理更多文档，从而使查询运行时间更长。请注意，仅当查询具有顶级LIMIT子句并且在查询中实际使用LIMIT子句时，fullCount属性才可能出现在结果中。
%       maxPlans：限制AQL查询优化器创建的最大计划数。
%       maxWarningCount：限制查询将返回的最大警告数。默认情况下，查询将返回的警告数限制为10，但是可以通过设置此属性来增加或减少该警告数。
%       failOnWarning：设置为true时，查询将引发异常并中止而不产生警告。在开发过程中应使用此选项，以尽早发现潜在问题。当该属性设置为false时，警告将不会传播到异常，并将与查询结果一起返回。还有一个服务器配置选项，--query.fail-on-warning用于设置failOnWarning的默认值，因此不需要在每个查询级别上进行设置。
%       stream：指定true，查询将以流方式执行。查询结果不存储在服务器上，而是动态计算的。注意：只要查询游标存在，长时间运行的查询就需要保持收集锁。设置为false时，查询将立即全部执行。在这种情况下，查询结果要么立即返回（如果结果集足够小），要么存储在arangod实例上，并且可以通过游标API进行访问（相对于ttl）。建议仅在短期运行的查询上使用此选项，或者不使用排他锁（MMFiles上的写锁）。请注意查询选项cache，count并且fullCount不适用于流查询。此外，查询统计信息，警告和概要分析数据仅在查询完成之后才可用。默认值为false
%       Optimizer：与查询优化器有关的选项。
%       rules：可以在此属性中放入要包括的或要排除的优化器规则的列表，告诉优化器包括或排除特定的规则。要禁用规则，请在其名称前面加上-，以启用规则，并在其前面加上+。还有一个伪规则all，它匹配所有优化程序规则。-all禁用所有规则。
%       profile：如果设置为true或1，那么如果未从查询缓存提供查询结果，则将在Extra Return属性的子属性配置文件中返回其他查询概要信息。设置为2时，查询将在Extra Return属性的子属性stats.nodes中包含每个查询计划节点的执行统计信息。此外，查询计划在子属性extra.plan中返回。
%       satelliteSyncWait：此Enterprise Edition参数允许配置DB-Server将有多长时间使查询中涉及的Satellite集合同步。默认值为60.0（秒）。达到最大时间后，查询将停止。
%       maxRuntime：查询必须在给定的运行时内执行，否则将被终止。该值以秒为单位指定。默认值为0.0（无超时）。
%       maxTransactionSize：事务大小限制（以字节为单位）。仅受RocksDB存储引擎的尊敬。
%       middleCommitSize：最大操作总数，之后将自动执行中间提交。仅受RocksDB存储引擎的尊敬。
%       middleCommitCount：操作之后自动执行中间提交的最大操作数。仅受RocksDB存储引擎的尊敬。
%       skipInaccessibleCollections：AQL查询（尤其是图遍历）将用户没有访问权限的集合视为这些集合为空。您的查询将正常执行，而不是返回禁止的访问错误。这旨在帮助某些用例：一个图包含多个集合，并且不同的用户在该图上执行AQL查询。现在，您可以通过更改用户对集合的访问权限来自然地限制可访问的结果。此功能仅在企业版中可用。
%
% 查询详细信息包括查询字符串以及可选的查询选项和绑定参数。这些值需要在POST请求的主体中以JSON表示形式传递。
% 如果结果集可以由服务器创建，则返回HTTP 201。
% error：一个标志，指示发生错误（在这种情况下为false）
% code：HTTP状态码
% result：结果文档数组（如果查询没有结果，则可能为空）
% hasMore：一个布尔值指示符，指示服务器上的游标是否还有更多结果可用
% count：可用结果文档总数（仅当查询是在设置了count属性的情况下执行的）
% id：在服务器上创建的临时光标的ID（可选，请参见上文）
% extra：一个可选的JSON对象，其统计信息子属性中包含有关查询结果的额外信息。对于数据修改查询， extra.stats子属性将包含已修改的文档数和由于错误而无法修改的文档数（如果指定了ignoreErrors查询选项）
% cached：一个布尔型标志，指示是否从查询缓存提供查询结果。如果从查询缓存提供查询结果，则额外的 return属性将不包含任何stats子属性，也不会包含任何配置文件子属性。
%
% 如果JSON格式不正确或请求中缺少查询规范，则返回HTTP 400。
% 如果JSON格式不正确或请求中缺少查询规范，则服务器将使用HTTP 400进行响应。
% 响应的主体将包含带有其他错误详细信息的JSON对象。该对象具有以下属性：
% error：布尔值标志，指示发生错误（在这种情况下为true）
% code：HTTP状态码
% errorNum：服务器错误号
% errorMessage：描述性错误消息
% 如果查询规范已完成，服务器将处理查询。如果在查询处理期间发生错误，则服务器将使用HTTP 400进行响应。同样，响应的正文将包含有关错误的详细信息。
% 一个查询错误的列表可以在这里找到。
% 404：如果查询中访问了不存在的集合，服务器将以HTTP 404进行响应。
% 405：如果使用了不受支持的HTTP方法，则服务器将以HTTP 405进行响应。
newCursor(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/cursor">>, [], BodyStr).


% 从现有游标返回下一个结果
% PUT /_api/cursor/{cursor-identifier}
% 路径参数
% cursor-identifier（必填）：光标的名称
% 如果游标仍然存在，则返回具有以下属性的对象：
% id：光标标识符
% 结果：当前批次的文档列表
% hasMore：如果这是最后一批，则为false
% count：如果存在，元素总数
% 请注意，即使hasMore返回true，下一次调用仍可能不返回任何文档。但是，如果hasMore为false，则光标将被耗尽。一旦hasMore属性的值为 false，客户端就可以停止。
% 返回码
% 200：如果成功，服务器将以HTTP 200响应。
% 400：如果省略了光标标识符，则服务器将使用HTTP 404进行响应。
% 404：如果找不到具有指定标识符的游标，则服务器将使用HTTP 404进行响应。
nextCursor(PoolNameOrSocket, CursorId) ->
   Path = <<"/_api/cursor/", (agMiscUtils:toBinary(CursorId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 删除光标永
% DELETE /_api/cursor/{cursor-identifier}
% 路径参数
%    cursor-identifier（必填）：光标的ID
% 删除游标并释放与其关联的资源。
% 当客户端从服务器上检索所有文档时，游标将在服务器上自动销毁。客户端还可以使用HTTP DELETE请求在任何较早的时间显式销毁游标。游标ID必须作为URL的一部分包含在内。
% 注意：在服务器控制的特定超时后，服务器还将自动销毁废弃的游标，以避免资源泄漏。
% 返回码
%    202：如果服务器知道游标，则返回。
%    404：如果服务器不知道游标，则返回404。如果在销毁游标后使用了游标，也将返回该值。
delCursor(PoolNameOrSocket, CursorId) ->
   Path = <<"/_api/cursor/", (agMiscUtils:toBinary(CursorId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% AQL查询的HTTP接口
%
% 解释和解析查询
% ArangoDB有一个HTTP接口，用于语法验证AQL查询。此外，它提供了一个HTTP接口来检索任何有效AQL查询的执行计划。
% 这两个功能实际上并不执行提供的AQL查询，而只是检查它并返回有关它的元信息。

% 解释一个AQL查询并返回有关它的信息
% POST /_api/explain
% 描述查询和查询参数的JSON对象。
% 具有以下属性的JSON对象是必需的：
%    query：您要解释的查询；如果查询引用了任何绑定变量，则这些变量也必须在属性bindVars中传递。可以在options属性中传递查询的其他选项。
%    bindVars：表示绑定参数的键/值对。
%    options：查询选项
%    allPlans：如果设置为true，将返回所有可能的执行计划。默认值为false，这意味着将仅返回最佳计划。
%    maxNumberOfPlans：允许优化程序生成的可选计划最大数量。将此属性设置为较低的值可以限制优化器的工作量。
%    Optimizer：与查询优化器有关的选项。
%    rules：可以在此属性中放入要包括的或要排除的优化器规则的列表，告诉优化器包括或排除特定的规则。要禁用规则，请在其名称前面加上-，以启用规则，并在其前面加上+。还有一个伪规则all，它匹配所有优化程序规则。-all禁用所有规则。
%
% 为了说明如何在服务器上执行AQL查询，可以通过HTTP POST请求将查询字符串发送到服务器。然后，服务器将验证查询并为其创建执行计划。将返回执行计划，但不会执行查询。
% 服务器返回的执行计划可用于估计查询的可能性能。尽管实际性能将取决于许多不同的因素，但是执行计划通常可以对服务器实际运行查询所需的工作量提供一些粗略的估计。
% 默认情况下，解释操作将返回查询优化器选择的最佳计划。最佳计划是总估计成本最低的计划。该计划将在响应对象的属性计划中返回。如果在请求中指定了allPlans选项，则结果将包含优化器创建的所有计划。然后将在属性计划中返回计划。
% 结果还将包含一个属性warnings，它是在优化或执行计划创建期间发生的一系列警告。此外，结果中还包含stats属性以及一些优化程序统计信息。如果allPlans设置为false，则结果将包含可缓存的属性 ，该属性指示如果使用了查询结果缓存，则是否可以将查询结果缓存在服务器上。该缓存时属性不存在allPlans 设置为真。
% 结果中的每个计划都是一个具有以下属性的JSON对象：
%    nodes：计划执行节点的数组。可在此处找到可用节点类型的数组
%    estimatedCost：计划的总估计费用。如果有多个计划，优化器将选择总成本最低的计划。
%    collections：查询中使用的一组collections
%    rules：优化程序应用的规则数组。可在此处找到可用规则的​​概述
%    variables：查询中使用的变量数组（注意：这可能包含优化器创建的内部变量）
% 返回码
%    200：如果查询有效，则服务器将使用HTTP 200进行响应，并在响应的plan属性中返回最佳执行计划。如果在请求中设置了选项allPlans，则将在allPlans属性中返回一系列计划。
%    400：如果请求格式错误或查询包含解析错误，服务器将以HTTP 400响应。响应的正文将包含嵌入在JSON对象中的错误详细信息。如果查询引用任何变量，则忽略绑定变量也会导致HTTP 400错误。
%    404：如果查询中访问了不存在的集合，服务器将以HTTP 404进行响应。
explainQuery(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/explain">>, [], BodyStr).

% 解析一个AQL查询并返回有关它的信息
% POST /_api/query
% 具有以下属性的JSON对象是必需的：
%    query：要在不执行查询字符串的情况下对其进行验证，可以通过HTTP POST请求将查询字符串传递到服务器。
% 该端点仅用于查询验证。要实际查询数据库，请参阅/api/cursor。
% 返回码
%    200：如果查询有效，服务器将使用HTTP 200进行响应，并在响应的bindVars属性中返回在查询中找到的绑定参数的名称（如果有）。它还将在collections属性中返回查询中使用的collections的数组。如果查询可以成功解析，则返回的JSON 的ast属性将包含查询的抽象语法树表示形式。ast的格式在将来的ArangoDB版本中可能会发生变化，但是可以用来检查ArangoDB如何解释给定查询。请注意，将在不对其应用任何优化的情况下返回抽象语法树。
%    400：如果请求格式错误或查询包含解析错误，服务器将以HTTP 400响应。响应的正文将包含嵌入在JSON对象中的错误详细信息。
parseQuery(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/query">>, [], BodyStr).

% 查询跟踪固定链接
% ArangoDB具有HTTP接口，用于检索当前正在执行的AQL查询列表和慢速AQL查询列表。为了有意义地使用这些API，需要在执行HTTP请求的数据库中启用查询跟踪。

% 返回AQL查询跟踪的配置
% GET /_api/query/properties
% 返回当前查询跟踪配置。该配置是具有以下属性的JSON对象：
%     enabled：如果设置为true，那么将跟踪查询。如果设置为 false，则不会跟踪查询或慢速查询。
%     trackSlowQueries：如果设置为true，则如果慢查询的运行时间超过了slowQueryThreshold中设置的值，则将在慢查询列表中跟踪慢查询 。为了跟踪慢查询， 还必须将enabled属性设置为true。
%     trackBindVars：如果设置为true，那么将跟踪查询中使用的绑定变量。
%     maxSlowQueries：保留在慢速查询列表中的最大慢速查询数。如果慢速查询列表已满，则在发生其他慢速查询时，其中最早的条目将被丢弃。
%     slowQueryThreshold：用于将查询视为慢查询的阈值。当启用慢查询跟踪时，运行时大于或等于此阈值的查询将被放入慢查询列表中。slowQueryThreshold的值以秒为单位指定。
%     maxQueryStringLength：保留在查询列表中的最大查询字符串长度。查询字符串可以有任意长度，如果使用非常长的查询字符串，则可以使用此属性来节省内存。该值以字节为单位指定。
% 返回码
% 200：如果成功检索到属性，则返回。
% 400：如果请求格式错误，服务器将以HTTP 400进行响应，
getQueryProps(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/properties">>, [], undefined).

% 更改AQL查询跟踪的配置
% PUT /_api/query/properties
% 具有以下属性的JSON对象是必需的：
%     enabled：如果设置为true，那么将跟踪查询。如果设置为 false，则不会跟踪查询或慢速查询。
%     trackSlowQueries：如果设置为true，则如果慢查询的运行时间超过了slowQueryThreshold中设置的值，则将在慢查询列表中跟踪慢查询 。为了跟踪慢查询， 还必须将enabled属性设置为true。
%     trackBindVars：如果设置为true，那么将与查询一起跟踪查询中使用的绑定变量。
%     maxSlowQueries：保留在慢速查询列表中的最大慢速查询数。如果慢速查询列表已满，则在发生其他慢速查询时，其中最早的条目将被丢弃。
%     slowQueryThreshold：用于将查询视为慢速的阈值。当启用慢查询跟踪时，运行时大于或等于此阈值的查询将被放入慢查询列表中。slowQueryThreshold的值以秒为单位指定。
%     maxQueryStringLength：要保留在查询列表中的最大查询字符串长度。查询字符串可以有任意长度，如果使用非常长的查询字符串，则可以使用此属性来节省内存。该值以字节为单位指定。
% 这些属性需要在HTTP请求主体的属性属性中传递。属性必须是JSON对象。
% 更改属性后，将在HTTP响应中返回当前属性集。
% 返回码
%     200：如果属性更改成功，则返回。
%     400：如果请求格式错误，服务器将以HTTP 400进行响应，
changeQueryProps(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/query/properties">>, [], BodyStr).

% 返回当前正在运行的AQL查询的列表
% GET /_api/query/current
% 返回一个数组，其中包含所选数据库中当前正在运行的AQL查询。每个查询都是一个具有以下属性的JSON对象：
%    id：查询的ID
%    query：查询字符串（可能被截断）
%    bindVars：查询使用的绑定参数值
%    started：查询开始的日期和时间
%    runTime：查询的运行时间，直到查询到查询列表为止
%    state：查询的当前执行状态（以字符串形式）
%    stream：查询是否使用流游标
% 返回码
%    200：可以成功检索查询列表时返回。
%    400：如果请求格式错误，服务器将以HTTP 400进行响应，
currentQuery(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/current">>, [], undefined).


% 返回运行缓慢的AQL查询的列表
% GET /_api/query/slow
% 返回一个数组，其中包含已完成并超过所选数据库中慢速查询阈值的最后一个AQL查询。可以通过设置查询跟踪属性来控制列表中的最大查询数量maxSlowQueries。可以通过设置查询跟踪属性来调整 将查询视为慢速查询的阈值slowQueryThreshold。
% 每个查询都是一个具有以下属性的JSON对象：
%     id：查询的ID
%     query：查询字符串（可能被截断）
%     bindVars：查询使用的绑定参数值
%      started：查询开始的日期和时间
%     runTime：查询的总运行时间
%     state：查询的当前执行状态（对于慢速查询列表将始终“完成”）
%     stream：查询是否使用流游标
% 返回码
%     200：可以成功检索查询列表时返回。
%     400：如果请求格式错误，服务器将以HTTP 400进行响应，
getSlowQuery(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query/slow">>, [], undefined).

% 清除慢速AQL查询列表
% DELETE /_api/query/slow
% 清除慢速AQL查询列表
% 返回码
% 200：成功清除查询列表后，服务器将以HTTP 200响应。
% 400：如果请求格式错误，服务器将使用HTTP 400进行响应。
delSlowQuery(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, <<"/_api/query/slow">>, [], undefined).


% 杀死查询永久链接
% 运行中的AQL查询也可以在服务器上终止。ArangoDB通过HTTP接口提供了终止功能。要终止正在运行的查询，必须指定其ID（在当前正在运行的查询列表中为该查询返回的ID）。然后将设置查询的kill标志，并且查询到达取消点后将中止查询。

% 杀死一个AQL查询
% DELETE /_api/query/{query-id}
% 路径参数
%     query-id（必填）：查询的ID。
% 终止正在运行的查询。查询将在下一个取消点终止。
% 返回码
%     200：当执行终止请求并设置查询的终止标志时，查询仍在运行时，服务器将以HTTP 200响应。
%     400：如果请求格式错误，服务器将使用HTTP 400进行响应。
%     404：当找不到指定ID的查询时，服务器将以HTTP 404响应。
killQuery(PoolNameOrSocket, QueryId) ->
   Path = <<"/_api/query/", (agMiscUtils:toBinary(QueryId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% AQL查询结果缓存的HTTP接口
% 本节介绍用于控制AQL查询结果缓存的API方法。

% 返回AQL查询结果缓存中存储结果的列表
% GET /_api/query-cache/entries
% 返回一个数组，其中包含当前存储在所选数据库的查询结果缓存中的AQL查询结果。每个结果都是一个具有以下属性的JSON对象：
% hash：查询结果的哈希值
% query：查询字符串
% bindVars：查询的绑定参数。仅当服务器启动时启用了对绑定变量的跟踪时，才显示此属性
% size：查询结果和绑定参数的大小，以字节为单位
% results：查询结果中的文档/行数
% started：查询存储在缓存中的日期和时间
% hits：从缓存中提供结果的次数（对于仅存储在缓存中但以后再也不会访问的查询，可以为 0）
% runTime：查询的运行时间
% dataSources：查询所使用的集合/视图的数组
% 返回码
%    200：可以成功检索结果列表时返回。
%    400：如果请求格式错误，服务器将以HTTP 400进行响应，
getQueryCaches(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query-cache/entries">>, [], undefined).

% 清除AQL查询结果缓存中的所有结果
% DELETE /_api/query-cache
% 清除当前数据库的查询结果缓存
% 返回码
%    200：成功清除缓存后，服务器将以HTTP 200响应。
%    400：如果请求格式错误，服务器将使用HTTP 400进行响应。
clearQueryCaches(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, <<"/_api/query-cache">>, [], undefined).

% 返回AQL查询结果缓存的全局配置
% GET /_api/query-cache/properties
% 返回全局AQL查询结果缓存配置。该配置是具有以下属性的JSON对象：
%    mode：AQL查询结果缓存运行的模式。该模式是下列值之一：off，on或demand。
%    maxResults：每个特定于数据库的缓存将存储的最大查询结果数。
%    maxResultsSize：每个数据库特定的缓存将存储的查询结果的最大累积大小。
%    maxEntrySize：每个特定于数据库的缓存将存储的查询的最大单个结果大小。
%    includeSystem：是否将涉及系统集合的查询结果存储在查询结果缓存中。
% 返回码
%    200：如果可以成功检索属性，则返回。
%    400：如果请求格式错误，服务器将以HTTP 400进行响应，
getQCacheProps(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/query-cache/properties">>, [], undefined).

% 全局调整AQL查询结果缓存属性
% PUT /_api/query-cache/properties
% 具有以下属性的JSON对象是必需的：
%    mode：AQL查询缓存应以哪种模式运行。可能的值是off，on或demand。
%    maxResults：每个特定于数据库的缓存将存储的最大查询结果数。
%    maxResultsSize：每个数据库特定的缓存将存储的查询结果的最大累积大小。
%    maxEntrySize：每个数据库特定的缓存将存储的查询结果的最大单个大小。
%    includeSystem：是否存储涉及系统集合的查询结果。
% 更改属性后，将在HTTP响应中返回当前属性集。
% 注意：更改属性可能会使缓存中的所有结果无效。AQL查询缓存的全局属性。这些属性需要在HTTP请求主体的属性属性中传递。属性必须是具有以下属性的JSON对象：
% 返回码
% 200：如果属性更改成功，则返回。
% 400：如果请求格式错误，服务器将以HTTP 400进行响应，
changeQCacheProps(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_api/query-cache/properties">>, [], BodyStr).

% AQL用户功能管理固定链接
% 这是用于管理AQL用户功能的ArangoDB HTTP接口的简介。AQL用户功能是一种使用用户定义的JavaScript代码扩展ArangoDB查询语言（AQL）功能的方法。
% 有关AQL用户功能及其含义的概述，请参阅“ 扩展AQL”一章。
% HTTP接口提供用于添加，删除和列出以前注册的AQL用户功能的API。
% 通过此接口管理的所有用户功能将存储在系统集合_aqlfunctions中。此集合中的文档不应直接访问，而只能通过专用接口访问。

% 创建一个新的AQL用户功能
% POST /_api/aqlfunction
% 具有以下属性的JSON对象是必需的：
%    name：用户函数的标准名称。
%    code：函数主体的字符串表示形式。
%    isDeterministic：一个可选的布尔值，用于指示函数结果是否完全确定（函数返回值仅取决于输入值，并且对于具有相同输入的重复调用，返回值相同）。该isDeterministic属性是当前未使用但对于优化可以在以后使用。
% 如果成功，则返回HTTP 200。如果该功能无效等，则将返回包含详细错误消息的HTTP 400。
% HTTP 200如果功能已经存在并被调用所取代，则服务器将使用HTTP 200进行响应。
%    error：布尔值标志，指示是否发生错误（在这种情况下为false）
%    code：HTTP状态码
%    isNewlyCreated：布尔值标志，指示是否新创建了函数（在这种情况下为false）
% HTTP 201如果服务器可以注册该功能，则服务器将使用HTTP 201进行响应 。
%    error：布尔值标志，指示是否发生错误（在这种情况下为false）
%    code：HTTP状态码
%    isNewlyCreated：布尔值标志，指示是否新创建了函数（在这种情况下为true）
% HTTP 400如果JSON格式不正确或请求中缺少必需数据，则服务器将使用HTTP 400进行响应。
%    error：布尔值标志，指示是否发生错误（在这种情况下为true）
%    code：HTTP状态码
%    errorNum：服务器错误号
%    errorMessage：描述性错误消息
newUserFun(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/aqlfunction">>, [], BodyStr).

% 删除现有的AQL用户功能
% DELETE /_api/aqlfunction/{name}
% 路径参数
%     name（必填）：AQL用户功能的名称。
% 查询参数
% group（可选）： - 真：中提供的功能的名称的名称被视为一个命名空间前缀，并在指定的命名空间的所有功能。将被删除。如果没有匹配的字符串，返回的删除函数数可能变为0。
% false：中提供的函数名的名称必须是完全限定，包括任何命名空间。如果没有一个与名称匹配，则返回HTTP 404。

% 删除由name标识的现有AQL用户功能或功能组。
% HTTP 200如果服务器可以删除该功能，则服务器将使用HTTP 200进行响应 。
%     error：布尔值标志，指示是否发生错误（在这种情况下为false）
%     code：HTTP状态码
%     DeleteCount：删除的用户功能的数量，总是1在group设置为false时。设置为true>= 0时的任何数字group
% HTTP 400如果用户功能名称格式错误，则服务器将使用HTTP 400进行响应。
%     error：布尔值标志，指示是否发生错误（在这种情况下为true）
%     code：HTTP状态码
%     errorNum：服务器错误号
%     errorMessage：描述性错误消息
% HTTP 404如果指定的用户用户功能不存在，则服务器将使用HTTP 404进行响应。
%     error：布尔值标志，指示是否发生错误（在这种情况下为true）
%     code：HTTP状态码
%     errorNum：服务器错误号
%     errorMessage：描述性错误消息
delUserFun(PoolNameOrSocket, UserFunName) ->
   Path = <<"/_api/aqlfunction/", (agMiscUtils:toBinary(UserFunName))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delUserFun(PoolNameOrSocket, UserFunName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/aqlfunction/", (agMiscUtils:toBinary(UserFunName))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 返回注册的AQL用户功能
% GET /_api/aqlfunction
% 查询参数
%     namespace（可选）：从result下的命名空间namespace返回所有已注册的AQL用户函数。
% 返回所有已注册的AQL用户功能。
% 该调用将返回一个带有状态代码的JSON数组，以及在result下找到的所有用户函数。
% HTTP 200成功HTTP 200返回。
%     error：布尔值标志，指示是否发生错误（在这种情况下为false）
%     code：HTTP状态码
%     result：所有函数，或与命名空间参数匹配的函数
%        name：用户功能的标准名称
%        code：函数体的字符串表示形式
%        isDeterministic：一个可选的布尔值，用于指示函数结果是否完全确定（函数返回值仅取决于输入值，并且对于具有相同输入的重复调用，返回值相同）。该isDeterministic属性是当前未使用但对于优化可以在以后使用。
% HTTP 400如果用户功能名称格式错误，则服务器将使用HTTP 400进行响应。
%     error：布尔值标志，指示是否发生错误（在这种情况下为true）
%     code：HTTP状态码
%     errorNum：服务器错误号
%     errorMessage：描述性错误消息
getUserFuns(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/aqlfunction">>, [], undefined).

getUserFuns(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/aqlfunction", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).











