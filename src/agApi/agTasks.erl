-module(agTasks).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/traversal.html

% HTTP任务接口
% 接下来，您将看到ArangoDB的任务HTTP接口。
% 还为每个API操作提供了一些示例。
%
% 提取所有任务或一项任务
% GET /_api/tasks/
% 获取服务器上的所有现有任务
% HTTP 200任务列表
% **：所有任务的列表
getTaskList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/tasks/">>, [], undefined).

% 检索一个当前活动的服务器任务
% GET /_api/tasks/{id}
% 路径参数
%    id（必填）：要提取的任务的ID。
% 在ID指定的服务器上获取一个现有任务
% HTTP 200请求的任务
% **：相关功能
getTask(PoolNameOrSocket, TaskId) ->
   Path = <<"/_api/tasks/", (agMiscUtils:toBinary(TaskId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 创建一个新任务
% POST /_api/tasks
% 具有以下属性的JSON对象是必需的：
%    name：任务名称
%    command：要执行的JavaScript代码
%    params：要传递给命令的参数
%    period：执行之间的秒数
%    offset：初始延迟的秒数
%
% 用生成的ID创建一个新任务
%    HTTP 200任务已注册
%    id：标识任务的字符串
%    created：创建此任务的时间戳
%    type：这是什么类型的任务[ periodic，timed]
%       定期重复执行的任务
%       定时是在特定时间执行一次的任务
%    period：这个任务应该运行的每个period秒
%    offset：距创建的时间戳的时间偏移（以秒为单位）
%    command：此任务的javascript函数
%    database：此任务所属的数据库
%    code：状态码，在这种情况下为200。
%    error：在这种情况下为false
%    400：如果帖子正文不正确，则返回HTTP 400。
newTask(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/tasks">>, [], BodyStr).

% 注册具有预定义ID的新任务；与负载均衡器不兼容
% PUT /_api/tasks/{id}
% 路径参数
%    id（必填）：要创建的任务的id
% 具有以下属性的JSON对象是必需的：
%    name：任务名称
%    command：要执行的JavaScript代码
%    params：要传递给命令的参数
%    period：执行之间的秒数
%    offset：初始延迟的秒数
% 注册具有指定ID的新任务
% 返回码
%    400：如果任务ID已经存在或其余主体不正确，则返回HTTP 400。
newTask(PoolNameOrSocket, TaskId, MapData) ->
   Path = <<"/_api/tasks/", (agMiscUtils:toBinary(TaskId))/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 删除一个当前活动的服务器任务
% DELETE /_api/tasks/{id}
% 路径参数
%    id（必填）：要删除的任务的id。
% 删除服务器上ID标识的任务。
%    HTTP 200如果删除了任务，则返回HTTP 200。
%    code：状态码，在这种情况下为200。
%    error：在这种情况下为false
%    HTTP 404如果任务ID未知，则返回HTTP 404。
%    code：状态码，在这种情况下为404。
%    error：在这种情况下为true
%    errorMessage：一条纯文本消息，指出出了什么问题。
delTask(PoolNameOrSocket, TaskId) ->
   Path = <<"/_api/tasks/", (agMiscUtils:toBinary(TaskId))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).
