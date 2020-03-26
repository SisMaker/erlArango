-module(agDbMgr).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/database-database-management.html

%% 检索有关当前数据库的信息（别名 /_api/database/properties）
%% GET /_api/database/current
curDbInfo(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/database/current">>, [], undefined).

%% 检索当前用户可以访问的所有数据库的列表
%% GET /_api/database/user
curVisitDbs(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/database/user">>, [], undefined).

%% 检索所有现有数据库的列表
%% GET /_api/database
curDbList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/database">>, [], undefined).


%% 创建一个新的数据库
%% POST /_api/database
% 具有以下属性的JSON对象是必需的：
% name：必须包含一个有效的数据库名称。
% options：可选对象，可以包含以下属性：
%     sharding：用于此数据库中新集合的分片方法。有效值为：“”，“ flexible”或“ single”。前两个是等效的。（仅集群）
%     ReplicationFactor：在此数据库中创建的新集合的默认复制因子。
%        特殊值包括“ satellite”和将其禁用复制的“ satellite”（将集合复制到每个数据库服务器）和“ 1”。（仅集群）
%     writeConcern：在此数据库中创建的新集合的默认写关注。
%        它确定在不同的DBServer上同步每个分片需要多少个副本。如果集群中的副本数量很少，那么分片将拒绝写入。
%        但是，具有足够最新副本的分片写入将同时成功。writeConcern的值 不能大于ReplicationFactor。（仅集群）
% users：必须是最初为新数据库创建的用户对象数组。对于已经存在的用户，不会更改用户信息。
%        如果未指定users或不包含任何用户，则将使用空字符串密码创建默认用户 root。这确保了新数据库在创建后将可访问。
%        每个用户对象可以包含以下属性：
%     username：要创建的用户的登录名
%     passwd：用户密码（字符串）。如果未指定，则默认为空字符串。
%     active：一个标志，指示是否应该激活用户帐户。默认值为true。如果设置为false，则用户将无法登录数据库。
%     extra：带有额外用户信息的JSON对象。Extra中包含的数据 将为用户存储，但ArangoDB不会进一步解释。

newDb(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/database">>, [], BodyStr, true).

%% 删除现有数据库
%% DELETE /_api/database/{database-name}

delDb(PoolNameOrSocket, Name) ->
   Path = <<"/_api/database/", Name/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined, true).
