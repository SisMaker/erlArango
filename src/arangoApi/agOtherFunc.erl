-module(agOtherFunc).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/miscellaneous-functions.html

%% 返回server版本
%% GET /_api/version
serverInfo(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_api/version">>, [], undefined).

%% 返回server版本详细信息
%% GET /_api/version?details=true
serverDetails(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, <<"/_api/version?details=true">>, [], undefined).

%% 返回服务器配置使用的存储引擎
%% GET /_api/engine
engine(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_api/engine">>, [], undefined).

%% 日志刷新进磁盘
%% PUT /_admin/wal/flush
logFlush(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Put, <<"/_admin/wal/flush">>, [], undefined).

%% 获取当前配置
%% GET /_admin/wal/properties
walConfig(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/wal/properties">>, [], undefined).

%% 设置配置
%% PUT /_admin/wal/properties
setWalConfig(PoolNameOrSocket, LogFileSize, AllowOversizeEntries) ->
   BodyStr = jiffy:encode(#{<<"logfileSize">> => LogFileSize, <<"allowOversizeEntries">> => AllowOversizeEntries}),
   agHttpCli:callAgency(PoolNameOrSocket, ?Put, <<"/_admin/wal/properties">>, BodyStr, undefined).


%% 返回有关当前正在运行的事务的信息
%% GET /_admin/wal/transactions
transactions(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/wal/transactions">>, [], undefined).

%% 获取系统的当前时间
%% GET /_admin/time
ostime(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/time">>, [], undefined).

%% 将发送的东西发回
%% POST /_admin/echo
echo(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, <<"/_admin/echo">>, [], undefined).

%% 返回数据库的版本
%% GET /_admin/database/target-version
dbVersion(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/database/target-version">>, [], undefined).

%% DELETE /_admin/shutdown
shutDown(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Delete, <<"/_admin/shutdown">>, [], undefined).

%% 执行脚本
%% POST /_admin/execute
execute(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Post, <<"/_admin/execute">>, [], undefined).

%% 返回服务器的状态信息
%% GET /_admin/status
status(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, <<"/_admin/status">>, [], undefined).

