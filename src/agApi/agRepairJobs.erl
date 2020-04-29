-module(agRepairJobs).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/repairs.html

% 维修工作
% DistributionShardsLike
% 在版本3.2.12和3.3.4之前，集合创建中存在一个错误，这可能导致违反该属性，即其碎片与distributeShardsLike设置中的原型集合完全一样地分布在DB-Server上 。
%
% 使用此API之前，请仔细阅读所有内容！
%
% 有一项可以安全还原此属性的作业。但是，在作业运行期间，
%
% 在replicationFactor 不能改变任何受影响的集合或原型集合（即集distributeShardsLike，包括 SmartGraphs）
% 这些原型之一的碎片也不应移动
% 修复过程中应避免关闭数据库服务器和关闭数据库服务器。在任何给定时间也只能执行一项维修工作。如果不满足这些要求，通常会导致作业中止，但仍然可以安全地重新启动它。但是，replicationFactor在修理过程中进行更改可能会使其处于无法人工干预而无法修理的状态！
% 关闭执行作业的协调器将中止它，但是可以安全地在另一个协调器上重新启动它。但是，即使作业停止后，仍可能会有碎片移动。如果在移动完成之前再次启动该作业，则修复受影响的集合将失败，但是可以安全地重新启动修复。
%
% 如果有任何受影响的集合replicationFactor等于数据库服务器的总数，则修复可能会中止。在这种情况下，有必要将其减少replicationFactor一个（或添加一个DB-Server）。这项工作不会自动执行。
%
% 通常，如果作业的任何假设失败，在开始或维修期间，该作业都会中止。它可以再次启动，并从当前状态恢复。
%
% 使用测试GET /_admin/repairs/distributeShardsLike
% 使用GET将不会触发任何修复，而只会计算并返回修复群集所需的操作。这样，您还可以检查是否有需要维修的东西。

%% 检查修复
%% GET /_admin/repairs/distributeShardsLike
checkRepair(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_admin/repairs/distributeShardsLike">>, [], undefined).

% 如果要修复某些内容，则响应将具有该属性 collections，<db>/<collection>其中包含每个必须修复的集合的条目。每个集合还作为单独的error属性，true如果该集合（以及false 其他情况）发生错误，则将作为错误。如果error为true，则还将设置属性errorNum和 errorMessage，并且在某些情况下还将errorDetails 提供有关如何处理特定错误的附加信息。
% 用修复POST /_admin/repairs/distributeShardsLike
% 由于此工作可能需要移动大量数据，因此可能需要一段时间，具体取决于受影响集合的大小。因此，这不应该被同步调用，而只能通过 Async Results 来调用：即，设置标头x-arango-async: store将作业放入后台，并在稍后获取其结果。否则，该请求很可能会导致超时，并且响应将丢失！除非协调器已停止，否则该作业仍将继续，但是无法找到它是否仍在运行，或者之后无法获得成功或错误信息。
% 可以像这样在后台启动作业：
%
% $ wget --method=POST --header='x-arango-async: store' -qSO - http://localhost:8529/_admin/repair/distributeShardsLike
% HTTP/1.1 202 Accepted
% X-Content-Type-Options: nosniff
% X-Arango-Async-Id: 152223973119118
% Server: ArangoDB
% Connection: Keep-Alive
% Content-Type: text/plain; charset=utf-8
% Content-Length: 0
%
% 这条线非常重要：
%
% X-Arango-Async-Id: 152223973119118
% 因为它包含作业ID，可用于以后获取作业的状态和结果。GET婷/_api/job/pending和/_api/job/done将列出未完成或者完成，分别作业的作业ID。
%
% 这也可以通过GET测试方法来完成。
%
% 必须使用job api来获取状态和结果。它会204在作业运行时返回。实际的响应将仅返回一次，然后删除作业，并且api将返回404。因此，建议将响应直接写到文件中以供以后检查。抓取的结果是通过调用/_api/job通过 PUT：
%
% $ wget --method=PUT -qSO - http://localhost:8529/_api/job/152223973119118 | jq .
% HTTP/1.1 200 OK
% X-Content-Type-Options: nosniff
% X-Arango-Async-Id: 152223973119118
% Server: ArangoDB
% Connection: Keep-Alive
% Content-Type: application/json; charset=utf-8
% Content-Length: 53
% {
% "error": false,
% "code": 200,
% "message": "Nothing to do."
% }

%% 修复
%% POST /_admin/repairs/distributeShardsLike
doRepair(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/repairs/distributeShardsLike">>, [], undefined).
