-module(agEdges).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/edge.html

% 边的地址和标签
% ArangoDB中的所有文档都有一个文档句柄。该句柄唯一地标识一个文档。可以使用其唯一的URI检索任何文档：
%
% http://server:port/_api/document/<document-handle>
% 边缘是文档的特殊变体。要访问边缘，请使用与文档相同的URL格式：
%
% http://server:port/_api/document/<document-handle>
% 例如，假定存储在 边缘的_id属性中的文档句柄是demo / 362549736，则该边缘的URL为：
%
% http://localhost:8529/_api/document/demo/362549736
% 上面的URL方案没有明确指定数据库名称，因此将使用默认数据库。要明确指定数据库上下文，请使用以下URL模式：
%
% http://server:port/_db/<database-name>/_api/document/<document-handle>
% 范例：
%
% http://localhost:8529/_db/mydb/_api/document/demo/362549736
% 注意：为了简洁起见，以下示例使用简短的URL格式。

% 获取边
% GET /_api/edges/{collection-id}
% 路径参数
% collection-id（必填）：边集合的ID或者边集合名。
% 查询参数
%    vertex（必填）：起始顶点的ID。
%    direction（可选）：选择 in or out 为边缘方向。如果未设置，则返回任何边。
% 返回以vertex标识的顶点开始或结束的边数组 。
% 返回码
% 200：如果找到边缘集合并检索到边缘，则返回。
% 400：如果请求包含无效参数，则返回。
% 404：如果未找到边缘集合，则返回。
getEdges(PoolNameOrSocket, CollName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/edges/", CollName/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).