-module(agCluster).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/cluster.html

% 集群的HTTP接口
% 本章介绍了ArangoDB群集的REST API。
%
% 服务器ID
% 服务器角色
% 集群统计
% 集群健康
% 集群维护
% 机构

% 返回集群中服务器的ID
% 了解服务器的内部ID
% GET /_admin/server/id
% 返回集群中服务器的ID。如果服务器未在集群模式下运行，则请求将失败。
% 返回码
% 200：当服务器以群集模式运行时返回。
% 500：当服务器未在群集模式下运行时返回。
getServerId(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/server/id">>, [], undefined).

%返回集群中服务器的角色
%GET /_admin/server/role
%返回集群中服务器的角色。该角色在结果的role属性中返回。角色的可能返回值是：
%SINGLE：服务器是没有集群的独立服务器
%协调器：服务器是集群中的协调器
%PRIMARY：服务器是集群中的DBServer
%次要的：不再使用此角色
%代理：服务器是集群中的代理节点
%UNDEFINED：在集群中，如果无法确定服务器角色，则返回UNDEFINED。
%在所有情况下均返回HTTP 200。
%错误：始终为假
%code：HTTP状态码，始终为200
%errorNum：服务器错误号
%作用：之一[ SINGLE，协调员，PRIMARY，SECONDARY，AGENT，UNDEFINED ]
getServerRole(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/server/role">>, [], undefined).

% DBserver查询统计
% 允许查询集群中数据库服务器的统计信息
% GET /_admin/clusterStatistics
% 查询参数
% DBserver（必填）：查询给定DBserver的统计信息
% 返回码
% 200：
% 400：数据库服务器的ID
% 403：
getClusterStatistics(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/clusterStatistics">>, [], undefined).

% 查询集群的运行状况以进行监视
% 返回由监督（机构）评估的集群的运行状况
% GET /_admin/cluster/health
% 查询群集的运行状况以进行监视。该响应是一个JSON对象，包含标准code，error，errorNum，和errorMessage字段适当。特定于端点的字段如下：
% ClusterId：标识集群的UUID字符串
% Health：一个对象，该对象包含群集中每个节点的描述性子对象。
% <nodeID>：中的每个条目Health将由节点ID键入，并包含以下属性：
% Endpoint：代表服务器网络端点的字符串。
% Role：服务器扮演的角色。可能的值是"AGENT"，"COORDINATOR"和"DBSERVER"。
% CanBeDeleted：布尔值，表示是否可以安全地从群集中删除节点。
% Version：该节点使用的ArangoDB的版本字符串。
% Engine：该节点使用的存储引擎。
% Status：一个字符串，指示由监督（机构）评估的节点的运行状况。对于协调器和dbservers节点运行状况，应将其视为真实的主要来源。如果节点正常响应请求，则为"GOOD"。如果错过了一个心跳，那就是"BAD"。如果在缺少心跳约15秒钟后通过监督宣布它失败，则会对其进行标记"FAILED"。
% 此外，它还将具有以下属性：
% 协调器和数据库服务器
% SyncStatus：节点上次报告的同步状态。该值主要用于确定的值Status。可能的值包括"UNKNOWN"，"UNDEFINED"，"STARTUP"，"STOPPING"，"STOPPED"，"SERVING"，"SHUTDOWN"。
% LastAckedTime：ISO 8601时间戳记，指定接收到的最后一个心跳。
% ShortName：代表服务器的简称的字符串，例如"Coordinator0001"。
% Timestamp：ISO 8601时间戳记，指定接收到的最后一个心跳。（已弃用）
% Host：可选字符串，指定主机（如果已知）。
% 仅协调员
% AdvertisedEndpoint：表示已播报端点的字符串（如果已设置）。（例如，外部IP地址或负载平衡器，可选）
% 代理商
% Leader：此节点视为领导者的代理的ID。
% Leading：此代理程序是否是领导者（true）或不是（false）。
% LastAckedTime：自上次以来的时间（acked以秒为单位）。
% 返回码 200：
getClusterHealth(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/cluster/health">>, [], undefined).

% 启用或禁用监督维护模式
% 启用或禁用集群监督（机构）维护模式
% PUT /_admin/cluster/maintenance
% 通过此API，您可以临时启用监督维护模式。请注意，启用维护模式后，不会进行任何类型的自动故障转移。该集群监控会自动重新激活自身60分钟禁用后。
% 要启用维护模式，请求正文必须包含字符串"on"。要禁用它，请发送字符串"off"（请注意，该字符串 必须为小写并包括引号）。
% 返回码
% 200：
% 400：
% 501：
% 504：
setClusterMaintenance(PoolNameOrSocket, OnOrOff) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Put, <<"/_admin/cluster/maintenance">>, [], OnOrOff).




