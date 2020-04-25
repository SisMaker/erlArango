-module(agAsyncResultHandling).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/async-results-management.html

% 用于异步结果管理的HTTP接口
% 请求执行
% ArangoDB提供了执行客户端请求的各种方法。客户可以根据其吞吐量，控制流和耐用性要求，在每个请求级别上选择适当的方法。
%
% 阻止执行
% ArangoDB是多线程服务器，允许同时处理多个客户端请求。通信处理和实际工作可以由多个工作线程并行执行。
%
% 尽管多个客户端可以连接并并行将其请求发送到ArangoDB，但是客户端可能需要等待其请求被处理。
%
% 默认情况下，服务器将完全处理传入的请求，然后将结果返回给客户端。客户端必须等待服务器的响应，然后才能通过连接发送其他请求。对于单线程或非事件驱动的客户端，等待完整的服务器响应可能不是最佳的。
%
% 此外，请注意，即使客户端关闭了HTTP连接，在服务器上运行的请求仍将继续，直到完成为止，然后注意客户端不再侦听。因此，关闭连接无助于中止长时间运行的查询！有关详细信息，请参见下面的“ 异步执行”和“结果检索” 以及HttpJobPutCancel下的内容。
%
% 忘记并
% 为了缓解客户端阻止问题，自版本1.4开始使用ArangoDB。提供了一种非阻塞请求的通用机制：如果客户端在请求中添加HTTP标头x-arango-async：true，则ArangoDB会将请求放入内存任务队列中，并向HTTP请求返回HTTP 202（接受）响应客户立即。服务器将异步执行队列中的任务，从而将客户端请求与实际工作分离。
%
% 与客户端等待服务器响应相比，这可以提供更高的吞吐量。缺点是发送到客户端的响应始终是相同的（通用HTTP 202），并且客户端此时无法基于实际操作的结果做出决定。实际上，在通用响应到达客户端时，该操作甚至可能尚未执行。因此，客户不能依赖其请求已被成功处理。
%
% 服务器上的异步任务队列不会保留，这意味着如果发生崩溃，队列中尚未处理的任务将丢失。但是，客户端将不知道它们是否已被处理。
%
% 因此，当客户具有严格的持久性要求或依靠发送操作的结果采取进一步措施时，客户不应发送额外的标头。
%
% 排队任务的最大数量由启动选项 --server.maximal-queue-size确定。如果已排队的任务数量超过此数量，则服务器将拒绝该请求，并显示HTTP 500错误。
%
% 最后，请注意，无法取消这种火灾并忘记工作，因为稍后您将无法识别它。如果您需要取消请求，请使用“ 异步执行”以及更高版本的“结果检索” 和下面的HttpJobPutCancel。
%
% 异步执行和以后的结果检索
% 通过将HTTP标头x-arango-async：存储添加到请求中，客户端可以指示ArangoDB服务器如上所述异步执行操作，还可以将操作结果存储在内存中以供以后检索。服务器将在HTTP响应标头x-arango-async-id中返回作业ID。客户端可以将此ID与/ _api / job上的HTTP API结合使用，这在本手册中有详细说明。
%
% 客户可以通过异步作业API询问ArangoDB服务器，哪些结果准备好检索，哪些没有准备好。客户端还可以通过将原始返回的作业ID传递给异步作业API，以获取已执行的异步作业的原始结果。然后，服务器将返回作业结果，就好像作业已正常执行一样。此外，客户端可以通过其作业ID取消运行异步作业，请参见HttpJobPutCancel。
%
% ArangoDB将保留通过x-arango-async：存储 头启动的所有作业结果。仅当客户端明确要求服务器提供特定结果时，才会从服务器中删除结果。
%
% 异步作业API还提供了用于垃圾回收的方法，客户端可以使用这些方法来摆脱“旧的”未获取的结果。客户应定期调用此方法，因为ArangoDB不会人为地限制尚未获取的结果的数量。
%
% 因此，客户有责任仅存储所需的尽可能多的结果，并尽快获取可用的结果，或至少不时清理未获取的结果。
%
% 作业队列和结果仅保存在服务器上的内存中，因此在崩溃时它们将丢失。
%
% 取消异步作业
% 如上所述，可以使用其作业ID取消异步运行的作业。如HttpJobPutCancel中所述，这是通过PUT请求 完成的。
%
% 但是，要对幕后发生的事情进行一些解释。首先，一个正在运行的异步查询可以在内部由C ++代码或JavaScript代码执行。例如，CRUD操作直接在C ++中执行，而AQL查询和事务由JavaScript代码执行。作业取消仅适用于JavaScript代码，因为所使用的机制只是在JavaScript线程中触发不可捕获的异常，而该异常将在C ++级别上捕获，从而导致作业的取消。以后将无法检索到任何结果，因为有关该请求的所有数据都将被丢弃。
%
% 如果取消在集群的协调器上运行的作业（共享），则仅停止在协调器上运行的代码，集群中可能残留了已经分配给DB-Server的任务，目前无法执行也要取消它们
%
% 异步执行和身份验证
% 如果请求需要身份验证，则在排队之前运行身份验证过程。仅当请求有效凭据且身份验证成功时，该请求才会排队。如果请求不包含有效的凭据，则不会将其排队，但会以与“常规”非排队请求相同的方式立即被拒绝。


% 获取作业结果并将其从队列中删除
% PUT /_api/job/{job-id}
% 路径参数
%    job-id（必填）：异步作业ID。
% 返回由job-id标识的异步作业的结果。如果服务器上存在异步作业结果，则该结果将从结果列表中删除。这意味着可以为每个job-id调用一次此方法。该方法将返回原始作业结果的标头和正文，以及附加的HTTP标头x-arango-async-job-id。如果存在此标头，则找到作业，并且响应中包含原始作业的结果。如果标题不存在，则找不到作业，并且响应中包含来自作业管理器的状态信息。
% 返回码
%    204：如果通过job-id请求的作业仍在待处理（或尚未完成）的作业队列中，则返回。在这种情况下，不会返回x-arango-async-id HTTP标头。
%    400：如果在请求中未指定作业ID，则返回。在这种情况下，不会返回x-arango-async-id HTTP标头。
%    404：如果找不到或已经从作业结果列表中删除或提取了作业，则返回404。在这种情况下，不会返回x-arango-async-id HTTP标头。
getAsyncJobRet(PoolNameOrSocket, JodId) ->
   Path = <<"/_api/job/", (agMiscUtils:toBinary(JodId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).


% 取消异步作业
% PUT /_api/job/{job-id}/cancel
% 路径参数
% job-id（必填）：异步作业ID。
% 取消由作业ID标识的当前正在运行的作业。请注意，实际取消正在运行的异步作业仍可能需要一些时间。
% 返回码
% 200：取消已启动。
% 400：如果在请求中未指定作业ID，则返回。在这种情况下，不会返回x-arango-async-id HTTP标头。
% 404：如果找不到或已经从作业结果列表中删除或提取了作业，则返回404。在这种情况下，不会返回x-arango-async-id HTTP标头。
cancelAsyncJob(PoolNameOrSocket, JodId) ->
   Path = <<"/_api/job/", (agMiscUtils:toBinary(JodId))/binary, "/cancel">>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], undefined).

% 删除异步作业结果
% DELETE /_api/job/{type}#by-type
% 路径参数
%     type（必填）：要删除的作业类型。类型可以是：
%     all：删除所有作业结果。当前正在执行或排队的异步作业不会被此调用停止。
%     expired：删除过期的结果。要确定结果的到期状态，请传递戳记查询参数。stamp必须是UNIX时间戳，所有在较低时间戳创建的异步作业结果都将被删除。
%     an actual job-id：在这种情况下，调用将删除指定的异步工作的结果。如果作业当前正在执行或排队，则不会中止。
% 查询参数
%     stamp（可选）：UNIX时间戳记，用于指定类型过期时的过期阈值。
% 删除所有作业结果，过期的作业结果或特定作业的结果。客户可以使用此方法对工作结果进行最终的垃圾收集。
% 返回码
%     200：如果删除操作成功执行，则返回。如果未删除任何结果，还将返回此代码。
%     400：如果未指定type或值无效，则返回。
%     404：如果type为job-id，但未找到具有指定id的异步作业，则返回404。
delAsyncJobRet(PoolNameOrSocket, TypeOrJodId) ->
   Path = <<"/_api/job/", (agMiscUtils:toBinary(TypeOrJodId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delAsyncJobRet(PoolNameOrSocket, TypeOrJodId, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/job/", (agMiscUtils:toBinary(TypeOrJodId))/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 返回特定作业的状态
% GET /_api/job/{job-id}
% 路径参数
%     job-id（必填）：异步作业ID。
% 返回指定作业的处理状态。可以通过查看响应的HTTP响应代码来确定处理状态。
% 返回码
%     200：如果已经执行了通过job-id请求的作业，并且已准备好获取其结果，则返回200。
%     204：如果通过job-id请求的作业仍在待处理（或尚未完成）的作业队列中，则返回。
%     404：如果找不到或已经从作业结果列表中删除或提取了作业，则返回404。
getAsyncJobStatus(PoolNameOrSocket, JodId) ->
   Path = <<"/_api/job/", (agMiscUtils:toBinary(JodId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 返回具有特定状态的工作结果ID
% GET /_api/job/{type}#by-type
% 路径参数
%     type（必填）：要返回的作业类型。类型可以是 done 的或 pending 的。将类型设置为done将使该方法返回可以获取其结果的异步作业的ID。将类型设置为pending将返回尚未完成的异步作业的ID。
% 查询参数
%     count（可选）：每次调用返回的最大ID数。如果未指定，将使用服务器定义的最大值。
% 返回具有特定状态（已完成或未决）的异步作业的ID列表。客户端可以使用该列表获取作业系统状态的概述，并在以后检索完成的作业结果。
% 返回码
%     200：如果列表可以成功编译，则返回。注意：该列表可能为空。
%     400：如果未指定type或值无效，则返回。
getAsyncJobList(PoolNameOrSocket, Type) ->
   Path = <<"/_api/job/", (agMiscUtils:toBinary(Type))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getAsyncJobList(PoolNameOrSocket, Type, Count) ->
   Path = <<"/_api/job/", (agMiscUtils:toBinary(Type))/binary, "?count=", (agMiscUtils:toBinary(Count))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

