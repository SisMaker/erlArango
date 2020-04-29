-module(agTransactions).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/transaction.html

% 交易的HTTP接口
% 交易次数
% ArangoDB的事务在服务器上执行。交易可以由客户以两种不同的方式执行：
%
% 通过流事务 API
% 通过JavaScript交易 API
% 两者之间的区别不难理解，下面列出了一个简短的入门。有关ArangoDB中事务如何工作以及ArangoDB可以提供哪些保证的详细说明，请参阅Transactions。
%
% 流交易
% 流事务使您可以使用单独的开始和提交/中止命令执行多文档事务。这类似于传统RDBMS使用BEGIN，COMMIT和ROLLBACK操作的方式。
%
% 这是大型交易的推荐API。但是，客户端负责确保不再需要事务时提交或中止事务，以避免占用资源。
%
% JavaScript交易
% JS-Transactions允许您向服务器发送一段专用的JavaScript代码（即一个函数），该代码将以事务方式执行。
%
% 在该功能结束时，将自动提交事务，并且该事务完成的所有更改都将保留。除了初始启动请求之外，客户端不需要任何交互。

% 局限性固定链接
% 在协调器上强制执行流事务的最大生命周期和事务大小，以确保事务不会阻止群集正常运行：
%
% 两次操作之间的最大闲置超时时间为10秒
% 每个数据库服务器的最大事务大小为128 MB
% 这些限制也适用于单个服务器上的流事务。
%
% 强制执行限制对于释放被放弃的事务使用的资源很有用，例如，从由于编程错误而被客户端应用程序放弃的事务中，或者从由于客户端连接中断而留下的事务中释放资源。

% 流事务的HTTP接口
% 在v3.5.0中引入
% 流事务使您可以使用单独的开始和提交/中止命令执行多文档事务。这类似于传统RDBMS使用BEGIN，COMMIT和ROLLBACK操作的方式。
% 要使用流事务，客户端首先将 事务的配置发送到ArangoDB服务器。
% 与JS-Transaction相反，此事务的定义必须仅包含将要使用的集合以及（可选）ArangoDB支持的各种事务选项。不支持任何动作属性。
% 流事务API 与ArangoDB中的其他API 结合使用。要将事务用于支持的操作，客户端需要在每个请求的x-arango-trx-id标头中指定事务标识符。这将自动使这些操作使用指定的事务。
% 支持的事务性API操作包括：
% Document API中的所有操作
% 通过Collection API的文档数
% 通过Collection API截断集合
% 通过Cursor API创建AQL游标
% 处理 托管图的顶点和边（通用图 / Gharial API，从v3.5.1开始）
% 请注意，客户端始终需要首先启动事务，并且需要显式指定用于写访问的集合。客户端负责确保不再需要事务时提交或中止事务。这样可以避免占用ArangoDB服务器上的资源。
% 事务将获取集合锁，用于MMFiles存储引擎中的读写操作以及RocksDB中的写操作。因此，建议尽量缩短交易时间。
% 有关ArangoDB中事务如何工作的更详细描述，请参阅Transactions。
% 另请参阅：
% 局限性
% 已知的问题

% 开始服务器端事务
% POST /_api/transaction/begin
% 具有以下属性的JSON对象是必需的：
%    collections：collections必须是一个JSON对象，可以具有一个或所有子属性read，write或Exclusive，每个子属性 都是collection名称的数组或单个collection name作为字符串。必须使用write或Exclusive属性声明将在事务中写入的集合，否则它将失败，而仅从中读取的未声明集合将被延迟添加。有关 更多信息，请参见锁定和隔离。
%    waitForSync：一个可选的布尔标志，如果设置了该标志，将强制事务在返回之前将所有数据写入磁盘。
%    allowImplicit：允许从未声明的集合中读取。
%    lockTimeout：一个可选的数值，可用于设置等待收集锁的超时时间。如果未指定，将使用默认值。将lockTimeout设置为0将使ArangoDB不会在等待锁定时超时。
%    maxTransactionSize：事务大小限制（以字节为单位）。仅受RocksDB存储引擎的尊敬。
% 事务描述必须在POST请求的正文中传递。如果可以在服务器上启动事务，则将返回HTTP 201。
% 对于成功启动的事务，返回的JSON对象具有以下属性：
%    error：布尔值标志，指示是否发生错误（ 在这种情况下为false）
% code：HTTP状态码
% 结果：结果包含
%    id：交易的标识符
%    status：包含字符串“ running”
% 如果缺少交易规范或格式错误，则服务器将使用HTTP 400或HTTP 404进行响应。
% 然后，响应的主体将包含带有其他错误详细信息的JSON对象。该对象具有以下属性：
%    error：布尔值标志，指示发生错误（在这种情况下为true）
%    code：HTTP状态码
%    errorNum：服务器错误号
%    errorMessage：描述性错误消息
% 返回码
%    201：如果事务正在服务器上运行， 则将返回HTTP 201。
%    400：如果事务规范丢失或格式不正确，则服务器将使用HTTP 400进行响应。
%    404：如果事务规范包含未知集合，则服务器将使用HTTP 404进行响应。
beginTransaction(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/transaction/begin">>, [], BodyStr).

% 提取服务器端事务的状态
% GET /_api/transaction/{transaction-id}
% 路径参数
%    transaction-id（必填）：交易标识符。
% 结果是一个描述事务状态的对象。它至少具有以下属性：
%    id：交易的标识符
%    status：交易的状态。“运行”，“承诺”或“中止”之一。
% 返回码
%    200：如果事务已在服务器上完全执行并提交， 则将返回HTTP 200。
%    400：如果指定的事务标识符丢失或格式错误，则服务器将使用HTTP 400进行响应。
%    404：如果找不到具有指定标识符的交易，则服务器将使用HTTP 404进行响应。
getTransactionStatus(PoolNameOrSocket, TransactionId) ->
   Path = <<"/_api/transaction/", (agMiscUtils:toBinary(TransactionId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 提交或中止正在运行的事务必须由客户端完成。在完成使用事务后，不要提交或中止事务是一个坏习惯。这将迫使服务器保留资源和收集锁，直到整个事务超时为止。
% 提交交易
% 提交服务器端事务
% PUT /_api/transaction/{transaction-id}
% 路径参数
%    transaction-id（必填）：交易标识符，
% 提交正在运行的服务器端事务。提交是幂等操作。多次提交事务不是错误。
% 如果可以提交事务，则将返回HTTP 200。返回的JSON对象具有以下属性：
%    error：布尔值标志，指示是否发生错误（ 在这种情况下为false）
%    code：HTTP状态码
% 结果：结果包含
%    id：交易的标识符
%    status：包含字符串“ committed”
% 如果找不到事务，不允许提交或事务被中止，则服务器将使用HTTP 400，HTTP 404或HTTP 409进行响应。
% 然后，响应的主体将包含带有其他错误详细信息的JSON对象。该对象具有以下属性：
%    error：布尔值标志，指示发生错误（在这种情况下为true）
%    code：HTTP状态码
%    errorNum：服务器错误号
%    errorMessage：描述性错误消息
% 返回码
%    200：如果已提交事务， 则将返回HTTP 200。
%    400：如果无法提交事务，则服务器将使用HTTP 400进行响应。
%    404：如果未找到事务，则服务器将使用HTTP 404进行响应。
%    409：如果事务已经中止，则服务器将使用HTTP 409进行响应。
commitTransaction(PoolNameOrSocket, TransactionId) ->
   Path = <<"/_api/transaction/", (agMiscUtils:toBinary(TransactionId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 中止服务器端事务
% DELETE /_api/transaction/{transaction-id}
% 路径参数
%    transaction-id（必填）：交易标识符，
% 中止正在运行的服务器端事务。中止是一个幂等的操作。多次中止事务不是错误。
% 如果可以中止该事务，则将返回HTTP 200。返回的JSON对象具有以下属性：
%    error：布尔值标志，指示是否发生错误（ 在这种情况下为false）
%    code：HTTP状态码
% 结果：结果包含
%    id：交易的标识符
%    status：包含字符串“ aborted”
% 如果找不到事务，不允许中止或已提交事务，则服务器将使用HTTP 400，HTTP 404或HTTP 409进行响应。
% 然后，响应的主体将包含带有其他错误详细信息的JSON对象。该对象具有以下属性：
%    error：布尔值标志，指示发生错误（在这种情况下为true）
%    code：HTTP状态码
%    errorNum：服务器错误号
%    errorMessage：描述性错误消息
% 返回码
%    200：如果事务中止， 将返回HTTP 200。
%    400：如果无法中止事务，则服务器将使用HTTP 400进行响应。
%    404：如果未找到事务，则服务器将使用HTTP 404进行响应。
%    409：如果事务已经提交，则服务器将以HTTP 409响应。
abortTransaction(PoolNameOrSocket, TransactionId) ->
   Path = <<"/_api/transaction/", (agMiscUtils:toBinary(TransactionId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 返回当前运行的服务器端事务
% GET /_api/transaction
% 结果是一个对象，该对象使用transactions属性进行描述，其中包含一个事务数组。在群集中，阵列将包含来自所有协调器的事务。
% 每个数组条目都包含一个具有以下属性的对象：
%    id：交易的ID
%    status：交易的状态
% 返回码
%    200：如果可以成功检索事务列表，则将返回HTTP 200。
getTransactionList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/transaction">>, [], undefined).

% JavaScript交易的HTTP接口
% ArangoDB的JS事务在服务器上执行。客户端可以通过将事务描述发送给服务器来启动事务。
% ArangoDB中的JS事务不提供单独的BEGIN，COMMIT和ROLLBACK 操作。取而代之的是，ArangoDB JS事务由JavaScript函数描述，然后JavaScript函数中的代码将以事务方式执行。
% 在该功能结束时，将自动提交事务，并且该事务完成的所有更改都将保留。如果在事务执行过程中引发异常，则将回滚事务中执行的所有操作。
% 有关ArangoDB中事务如何工作的更详细描述，请参阅Transactions。

% 执行服务器端事务
% POST /_api/transaction
% 具有以下属性的JSON对象是必需的：
%    collections：collections必须是一个JSON对象，可以具有一个或所有子属性read，write或Exclusive，每个子属性 都是collection名称的数组或单个collection name作为字符串。必须使用write或Exclusive属性声明将在事务中写入的集合，否则它将失败，而仅从中读取的未声明集合将被延迟添加。可以将可选的子属性allowImplicit设置为false， 以使事务在未声明声明的读取集合的情况下失败。如果可能，应完全声明要读取的集合，以避免死锁。看到锁定和隔离 以获取更多信息。
%    action：要执行的实际交易操作，采用字符串化JavaScript代码的形式。该代码将在服务器端执行，并具有后期绑定。因此，至关重要的是，在操作中指定的代码 正确设置其所需的所有变量。如果操作中指定的代码以return语句结尾， 则如果事务成功提交，则REST API还将在result属性中返回返回的值。
%    waitForSync：一个可选的布尔标志，如果设置了该标志，将强制事务在返回之前将所有数据写入磁盘。
%    allowImplicit：允许从未声明的集合中读取。
%    lockTimeout：一个可选的数值，可用于设置等待收集锁的超时时间。如果未指定，将使用默认值。将lockTimeout设置为0将使ArangoDB不会在等待锁定时超时。
%    params：传递给操作的可选参数。
%    maxTransactionSize：事务大小限制（以字节为单位）。仅受RocksDB存储引擎的尊敬。
% 事务描述必须在POST请求的正文中传递。
% 如果事务已在服务器上完全执行并提交， 则将返回HTTP 200。另外，在操作中定义的代码的返回值将在result属性中返回。
% 对于成功提交的事务，返回的JSON对象具有以下属性：
%    error：布尔值标志，指示是否发生错误（ 在这种情况下为false）
%    code：HTTP状态码
%    result：交易的返回值
% 如果缺少交易规范或格式错误，则服务器将使用HTTP 400进行响应。
% 然后，响应的主体将包含带有其他错误详细信息的JSON对象。该对象具有以下属性：
%    error：布尔值标志，指示发生错误（在这种情况下为true）
%    code：HTTP状态码
%    errorNum：服务器错误号
%    errorMessage：描述性错误消息
% 如果事务提交失败（无论是由于操作代码中引发的异常 还是内部错误），服务器都会以错误进行响应。其他任何错误都将使用返回码 HTTP 400，HTTP 409或HTTP 500返回。
% 返回码
%    200：如果事务已在服务器上完全执行并提交， 则将返回HTTP 200。
%    400：如果事务规范丢失或格式不正确，则服务器将使用HTTP 400进行响应。
%    404：如果事务规范包含未知集合，则服务器将使用HTTP 404进行响应。
%    500：用户抛出的异常将使服务器以HTTP 500的返回码进行响应
executeTransaction(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/transaction">>, [], BodyStr).



