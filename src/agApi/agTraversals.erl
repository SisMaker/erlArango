-module(agTraversals).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/traversal.html

% 执行遍历永久链接
% 执行服务器端遍历
% POST /_api/traversal
% 此路由不应再使用。从3.4.0版开始，它被视为已弃用。它被AQL图形遍历取代。
