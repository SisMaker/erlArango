-module(agRepairJobs).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/repairs.html

%% 检查修复
%% GET /_admin/repairs/distributeShardsLike
checkRepir(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/repairs/distributeShardsLike">>, [], undefined).

%% 修复
%% POST /_admin/repairs/distributeShardsLike
repir(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, <<"/_admin/repairs/distributeShardsLike">>, [], undefined).
