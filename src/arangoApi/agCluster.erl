-module(agCluster).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/cluster.html

%% GET /_admin/server/id
serverId(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/server/id">>, [], undefined).

%% GET /_admin/server/role
serverRole(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/server/role">>, [], undefined).

%% GET /_admin/clusterStatistics
clusterStatistics(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/clusterStatistics">>, [], undefined).

%% GET /_admin/cluster/health
clusterHealth(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/cluster/health">>, [], undefined).

%% PUT /_admin/cluster/maintenance
clusterMaintenance(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Put, <<"/_admin/cluster/maintenance">>, [], undefined).




