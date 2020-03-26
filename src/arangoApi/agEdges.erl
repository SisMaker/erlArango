-module(agEdges).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/edge.html

% 边是具有两个附加属性的文档：_from和_to。这些属性是强制性的，并且必须包含边的from和to顶点的文档句柄。
% 使用常规文档 REST api 进行创建/读取/更新/删除。
% 读取入站或出站边缘
% 获得优势
% GET /_api/edges/{collection-id}
% 路径参数
% collection-id（必填）：集合的ID。
% 查询参数
% 顶点（必填）：起始顶点的ID。
% 方向（可选）：选择在或出为边缘方向。如果未设置，则返回任何边。
% 返回以vertex标识的顶点开始或结束的边数组 。
% 返回码
% 200：如果找到边缘集合并检索到边缘，则返回。
% 400：如果请求包含无效参数，则返回。
% 404：如果未找到边缘集合，则返回。
getEdges(PoolNameOrSocket, CollName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/edges/", CollName/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).
