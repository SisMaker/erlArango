-module(agAqlMod).
-include("erlArango.hrl").

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