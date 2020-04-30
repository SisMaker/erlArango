-module(agAdminMonitor).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/administration-and-monitoring.html

% 从服务器Permalink读取全局日志
% 返回服务器日志
% GET /_admin/log
% 查询参数
%    upto （可选）：返回所有日志条目多达日志级别高达。请注意，upto必须为：
%       fatal 或0
%       error或1
%       warning或2
%       info或3
%       debug 或4 默认值为info。
%    level （可选）：返回日志级别的所有日志条目级别。请注意，查询参数 upto和level是互斥的。
%    start（可选）：返回所有日志条目，以使其日志条目标识符（lid值）大于或等于start。
%    size（可选）：将结果限制为最大大小的日志条目。
%    offset（可选）：开始返回日志条目，跳过第一个偏移日志条目。偏移量 和大小可用于分页。
%    search （可选）：仅返回包含search中指定的文本的日志条目。
%    sort（可选）：根据日志条目的盖值对日志条目进行升序（如果sort为asc）或降序（如果sort为desc）。请注意，盖子会 按时间顺序排列。默认值为asc。
% 从服务器的全局日志中返回致命，错误，警告或信息日志消息。结果是具有以下属性的JSON对象：
% HTTP 200
%    lid：日志条目标识符的列表。每个日志消息都由其@LIT {lid}唯一标识，并且标识符按升序排列。
%    level：所有日志条目的日志级别列表。
%    timestamp：所有日志条目的时间戳列表，自1970-01-01开始以秒为单位。
%    text：所有日志条目的文本列表
%    topic：所有日志条目的主题列表
%    totalAmount：分页前的日志条目总数。
% 400：如果为up或level指定了无效值，则返回。
% 500：如果服务器由于内存不足错误而无法生成结果，则返回。
getAdminLog(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/log">>, [], undefined).

getAdminLog(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_admin/log", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 返回当前的日志级别设置
% GET /_admin/log/level
% 返回服务器的当前日志级别设置。结果是一个JSON对象，其日志主题为对象键，日志级别为对象值。
% 返回码
% 200：如果请求有效，则返回
% 500：如果服务器由于内存不足错误而无法生成结果，则返回。
getAdminLogLevel(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/log/level">>, [], undefined).

% 修改当前的日志级别设置
% PUT /_admin/log/level
% 具有以下属性的JSON对象是必需的：
% agency：可能的日志级别之一。
% agencycomm：可能的日志级别之一。
% authentication：可能的日志级别之一。
% 授权：可能的日志级别之一。
% cache：可能的日志级别之一。
% cluster：可能的日志级别之一。
% collector：可能的日志级别之一。
% 通信：可能的日志级别之一。
% 压缩器：可能的日志级别之一。
% config：可能的日志级别之一。
% datafiles：可能的日志级别之一。
% 开发：可能的日志级别之一。
% 引擎：可能的日志级别之一。
% general：可能的日志级别之一。
% graphs：可能的日志级别之一。
% 心跳：可能的日志级别之一。
% 内存：可能的日志级别之一。
% mmap：可能的日志级别之一。
% 性能：可能的日志级别之一。
% pregel：可能的对数水平之一。
% 查询：可能的日志级别之一。
% 复制：可能的日志级别之一。
% 请求：可能的日志级别之一。
% rocksdb：可能的日志级别之一。
% ssl：可能的日志级别之一。
% startup：可能的日志级别之一。
% 监视：可能的日志级别之一。
% syscall：可能的日志级别之一。
% 线程：可能的日志级别之一。
% trx：可能的日志级别之一。
% v8：可能的日志级别之一。
% views：可能的日志级别之一。
% ldap：可能的日志级别之一。
% audit-authentication：可能的日志级别之一。
% audit-authorization：可能的日志级别之一。
% audit-database：可能的日志级别之一。
% audit-collection：可能的日志级别之一。
% audit-view：可能的日志级别之一。
% audit-document：可能的日志级别之一。
% audit-service：可能的日志级别之一。
% 修改并返回服务器的当前日志级别设置。请求主体必须是JSON对象，日志主题为对象键，日志级别为对象值。
% 结果是一个JSON对象，其中调整后的日志主题为对象键，调整后的日志级别为对象值。
% 它可以通过仅将日志级别指定为字符串而不设置json来设置所有功能的日志级别。
% 可能的日志级别为：
% 致命-这是没有办法的。此消息后，ArangoDB将关闭。
% 错误-这是一个错误。您应该调查并修复它。它可能会损害您的生产。
% 警告-这可能是严重的应用程序明智的选择，但我们不知道。
% 信息-发生了什么事，请注意，但没有发生任何戏剧事件。
% 调试-输出调试消息
% 跟踪-跟踪-准备要淹没的日志-请勿在生产中使用。
% 返回码
% 200：如果请求有效，则返回
% 400：当请求正文包含无效JSON时返回。
% 405：使用无效的HTTP方法时返回。
% 500：如果服务器由于内存不足错误而无法生成结果，则返回。
modifyAdminLogLevel(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, <<"/_admin/log/level">>, [], BodyStr).

% 返回统计信息
% GET /_admin/statistics
% 返回统计信息。返回的对象包含根据_admin / statistics-description返回的描述分组在一起的 统计数字。例如， 要从组系统访问图userTime，您首先选择描述存储在系统中的组的子对象，然后在该子对象中，userTime的值存储在同名属性中。
% 如果是分发，则返回的对象包含以count为单位的总计 数和以counts为单位的分发列表。各个值的总和（或全部）中被返回总和。
% 事务统计信息显示本地已启动，已提交和已中止的事务，以及为查询的服务器完成的中间提交。对于RocksDB存储引擎，中间提交计数将仅采用非零值。协调器几乎不会在其本地数据库中进行任何本地事务，因此，群集事务（在协调器上启动的事务需要DBServer在事务提交到群集范围之前完成）才被添加到其本地统计信息中。这意味着您将在单个服务器上看到的统计信息大致是在使用单个协调器查询该协调器的群集设置中所期望的统计信息。区别在于群集事务没有中间提交的概念，也不会增加价值。
% HTTP 200统计信息已成功返回。
% error：布尔值标志，指示是否发生错误（在这种情况下为false）
% code：HTTP状态码-在这种情况下为200
% time：当前服务器时间戳
% errorMessage：描述性错误消息
% enabled：如果服务器启用了统计模块，则为true。如果没有，不要期望任何值。
% 系统：从系统收集的有关此过程的指标；可能取决于主机操作系统
% minorPageFaults：pagefaults
% majorPageFaults：pagefaults
% userTime：服务器进程使用的用户CPU时间
% systemTime：服务器进程使用的系统CPU时间
% numberOfThreads：服务器中的线程数
% residentSize：流程的RSS
% residentSizePercent：进程的RSS，以％为单位
% virtualSize：进程的VSS
% client：有关连接的客户端及其资源使用情况的信息
% sum：所有计数的汇总值
% count：汇总的值数
% counts：包含值的数组
% connectionTime：总连接时间
% totalTime：系统时间
% requestTime：请求时间
% queueTime：请求排队等待处理的时间
% ioTime：IO时间
% bytesSent：发送给客户端的字节数
% bytesReceived：从客户端收到的字节数
% httpConnections：打开的http连接数
% http：动词的请求数
% requestsTotal：http请求总数
% requestsAsync：异步http请求的总数
% RequestsGet：使用GET动词的请求数
% requestHead：使用HEAD动词的请求数
% requestPost：使用POST动词的请求数
% requestsPut：使用PUT动词的请求数
% requestsPatch：使用PATCH动词的请求数
% requestsDelete：使用DELETE动词的请求数
% requestsOptions：使用OPTIONS动词的请求数
% requestOther：没有使用以上识别的动词的任何请求
% 服务器：服务器的统计信息
% 正常运行时间：服务器启动和运行的时间
% physicalMemory：服务器上的可用物理内存
% Transactions：交易统计
% 已开始：已开始交易的数量
% 已提交：已提交交易的数量
% 已中止：已中止交易的数量
% middleCommits：完成的中间提交数
% v8Context：有关V8 JavaScript上下文的统计信息
% 可用：当前生成的V8上下文的数量
% busy：当前活动的V8上下文的数量
% dirty：先前使用的上下文数量，现在应该在重新使用之前对其进行垃圾回收
% free：可以免费使用的V8上下文的数量
% max：我们可以通过--javascript.v8-contexts配置生成的V8上下文总数
% 内存：V8内存/垃圾回收水印列表；每次运行垃圾回收时都要刷新；将当时使用的最小/最大内存保留10秒钟
% contextId：这组内存统计信息来自的上下文的ID
% tMax：10秒间隔开始的时间戳
% countOfTimes：这10秒内垃圾回收运行了多少次
% heapMax：所有垃圾收集的高水位标记在10秒内运行
% heapMin：这10秒内运行的所有垃圾回收的低水印
% 线程：有关服务器工作线程的统计信息（不包括特定于V8或jemalloc的线程和系统线程）
% scheduler-threads：产生的工作线程数
% 进行中：当前繁忙的工作线程数
% 排队：排队等待工作线程可用的作业数
getAdminProps(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/statistics">>, [], undefined).

% 统计数据说明
% 获取统计信息的描述性信息
% GET /_admin/statistics-description
% 返回/ _admin / statistics返回的统计信息的描述。返回的对象在属性组中包含一组统计信息组，在属性图中包含一组统计信息 图。
% 统计组由
% group：组的标识符。
% name：组的名称。
% description：组的描述。
% 统计数字由
% group：此图所属的组的标识符。
% 标识符：图形的标识符。它在组中是唯一的。
% name：图形名称。
% description：对图形的描述。
% 类型：当前，累计或分配。
% cuts：分布向量。
% 单位：测量数字的单位。
% HTTP 200描述已成功返回。
% 组：统计组
% group：组的标识符。
% name：组的名称。
% description：组的描述。
% 数字：统计数字
% group：此图所属的组的标识符。
% 标识符：图形的标识符。它在组中是唯一的。
% name：图形名称。
% description：对图形的描述。
% 类型：当前，累计或分配。
% cuts：分布向量。
% 单位：测量数字的单位。
% code：HTTP状态码
% 错误：错误，在这种情况下为false
getAdminStatisticsDesc(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/statistics-description">>, [], undefined).

% TLS 永久链接
% 返回TLS数据的摘要
% 返回此服务器的TLS数据（服务器密钥，客户端身份验证CA）
% GET /_admin/server/tls
% 返回TLS数据的摘要。JSON响应将包含result具有以下组件的字段 ：
% keyfile：有关密钥文件的信息。
% clientCA：有关用于客户端证书验证的CA的信息。
% 如果使用服务器名称指示（SNI），并且为不同的服务器名称配置了多个密钥文件，那么将存在一个附加属性SNI，该属性为每个配置的服务器名称包含有关该服务器名称的密钥文件的相应信息。
%
% 在所有情况下，该属性的值都将是一个JSON对象，该对象具有以下属性的子集（无论适当）：
% SHA256：该值是一个带有整个输入文件的SHA256的字符串。
% certificates：值是一个JSON数组，文件链中包含公共证书。
% privateKeySHA256：如果存在私钥（keyfile 但没有私钥clientCA），则此字段存在并且包含带有私钥SHA256的JSON字符串。
% 这是一个公共API，因此它不要求身份验证。
% 返回码
% 200：如果一切正常，此API将返回HTTP 200
getAdminTLS(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/tls">>, [], undefined).

% 触发TLS数据的重新加载并返回摘要永久链接
% 触发此服务器的TLS数据（服务器密钥，客户端身份验证CA）的重新加载，并以摘要形式返回新数据。
% POST /_admin/server/tls
% 此API调用触发所有TLS数据的重新加载，然后返回摘要。JSON响应与相应的GET请求完全相同（请参见此处）。
% 这是受保护的API，只能以超级用户权限执行。
% 返回码
% 200：如果一切正常，此API将返回HTTP 200
% 403：如果未使用超级用户权限调用此API，它将返回HTTP 403 FORBIDDEN。
triggerAdminTLS(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/server/tls">>, [], undefined).

% 返回当前实例指标
% GET /_admin/metrics
% 以Prometheus格式返回实例的当前指标。返回的文档收集所有实例指标，这些指标在任何给定时间进行测量，并将其公开以供Prometheus收集。
% 该文档包含不同的度量标准和度量标准组，具体取决于查询实例的角色。所有导出的指标都使用arangodb_或rocksdb_字符串发布，以将其与其他收集的数据区分开。
% 然后需要将API添加到Prometheus配置文件中进行收集。
% 返回码
% 200：指标已成功返回。
% 404：可以使用--server.export-metrics-api false 服务器中的设置禁用指标API 。在这种情况下，调用结果表明找不到该API。
getAdminMetrics(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/metrics">>, [], undefined).


% 集群
% 服务器返回是否是在只读模式
% 返回此服务器的模式（只读或默认）
% GET /_admin/server/mode
% 关于服务器的返回模式信息。json响应将包含一个mode值为readonly或的字段default。在只读服务器中，所有写入操作将失败，错误代码为1004（ERROR_READ_ONLY）。创建或删除数据库和集合也将失败，并显示错误代码11（ERROR_FORBIDDEN）。
% 这是一个公共API，因此它不要求身份验证。
% 返回码
% 200：如果一切正常，此API将返回HTTP 200
getAdminServerMode(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/mode">>, [], undefined).

% 返回集群永久链接中服务器的ID
% 了解服务器的内部ID
% GET /_admin/server/id
% 返回集群中服务器的ID。如果服务器未在集群模式下运行，则请求将失败。
% 返回码
% 200：当服务器以群集模式运行时返回。
% 500：当服务器未在群集模式下运行时返回。
getAdminServerId(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/id">>, [], undefined).

% 返回集群中服务器的角色
% GET /_admin/server/role
% 返回集群中服务器的角色。该角色在结果的role属性中返回。角色的可能返回值是：
% SINGLE：服务器是没有集群的独立服务器
% 协调器：服务器是集群中的协调器
% PRIMARY：服务器是集群中的DBServer
% 次要的：不再使用此角色
% 代理：服务器是集群中的代理节点
% UNDEFINED：在集群中，如果无法确定服务器角色，则返回UNDEFINED。
% 在所有情况下均返回HTTP 200。
% 错误：始终为假
% code：HTTP状态码，始终为200
% errorNum：服务器错误号
% 作用：之一[ SINGLE，协调员，PRIMARY，SECONDARY，AGENT，UNDEFINED ]
getAdminServerRole(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/role">>, [], undefined).


% 返回服务器是否可用
% GET /_admin/server/availability
% 返回有关服务器的可用性信息。
% 这是一个公共API，因此它不要求身份验证。它仅在服务器监视的上下文中使用。
% 返回码
% 200：如果服务器已启动并且正在运行并且可用于任意操作，并且未设置为只读模式，并且在活动故障转移设置的情况下当前不是关注者，则此API将返回HTTP 200。
% 503：如果服务器在启动或关闭过程中，设置为只读模式或当前在活动故障转移设置中为关注者，则将返回HTTP 503。
getAdminServerAvailability(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/server/availability">>, [], undefined).

% DBserver 永久链接的查询统计信息
% 允许查询集群中数据库服务器的统计信息
% GET /_admin/clusterStatistics
% 查询参数
% DBserver（必填）：查询给定DBserver的统计信息
% 返回码
% 200：
% 400：数据库服务器的ID
% 403：
getAdminClusterProps(PoolNameOrSocket, DBserver) ->
   Path = <<"/_admin/clusterStatistics?DBserver=", (agMiscUtils:toBinary(DBserver))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 查询集群的运行状况以监视Permalink
% 返回由监督（机构）评估的集群的运行状况
% GET /_admin/cluster/health
% 查询群集的运行状况以进行监视。该响应是一个JSON对象，包含标准code，error，errorNum，和errorMessage字段适当。特定于端点的字段如下：
% ClusterId：标识集群的UUID字符串
% Health：一个对象，该对象包含群集中每个节点的描述性子对象。
% <nodeID>：中的每个条目Health将由节点ID键入，并包含以下属性：
% Endpoint：代表服务器网络端点的字符串。
% Role：服务器扮演的角色。可能的值是"AGENT"，"COORDINATOR"和"DBSERVER"。
% CanBeDeleted：布尔值，表示是否可以安全地从群集中删除节点。
% Version：该节点使用的ArangoDB的版本字符串。
% Engine：该节点使用的存储引擎。
% Status：一个字符串，指示由监督（机构）评估的节点的运行状况。对于协调器和dbservers节点运行状况，应将其视为真实的主要来源。如果节点正常响应请求，则为"GOOD"。如果错过了一个心跳，那就是"BAD"。如果在缺少心跳约15秒钟后通过监督宣布它失败，则会对其进行标记"FAILED"。
% 此外，它还将具有以下属性：
% 协调器和数据库服务器
% SyncStatus：节点上次报告的同步状态。该值主要用于确定的值Status。可能的值包括"UNKNOWN"，"UNDEFINED"，"STARTUP"，"STOPPING"，"STOPPED"，"SERVING"，"SHUTDOWN"。
% LastAckedTime：ISO 8601时间戳记，指定接收到的最后一个心跳。
% ShortName：代表服务器的简称的字符串，例如"Coordinator0001"。
% Timestamp：ISO 8601时间戳记，指定接收到的最后一个心跳。（已弃用）
% Host：可选字符串，指定主机（如果已知）。
% 仅协调员
% AdvertisedEndpoint：表示已播报端点的字符串（如果已设置）。（例如，外部IP地址或负载平衡器，可选）
% 代理商
% Leader：此节点视为领导者的代理的ID。
% Leading：此代理程序是否是领导者（true）或不是（false）。
% LastAckedTime：自上次以来的时间（acked以秒为单位）。
% 返回码
% 200：
getAdminClusterHealth(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/cluster/health">>, [], undefined).

% 重新加载路由表。
% POST /_admin/routing/reload
% 从集合路由中重新加载路由信息。
% 返回码
% 200：路由信息重新加载成功
reloadAdminRouting(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/routing/reload">>, [], undefined).
