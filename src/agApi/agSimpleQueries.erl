-module(agSimpleQueries).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/simple-query.html

% 从版本3.4.0开始不推荐使用简单查询API。这些端点不应再使用。它们被AQL查询取代。