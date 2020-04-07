-module(agDbMgr).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/database-database-management.html

% 数据库信息永久链接
% 检索有关当前数据库的信息（别名/_ap/database/properties）
% GET /_api/database/current
% 检索有关当前数据库的信息
% 响应是具有以下属性的JSON对象：
% name：当前数据库的名称
% id：当前数据库的ID
% path：当前数据库的文件系统路径
% isSystem：当前数据库是否为_system数据库
% sharding：此数据库中创建的集合的默认分片方法
% ReplicationFactor：此数据库中集合的默认复制因子
% writeConcern：此数据库中集合的默认写关注点
% 返回码
% 200：如果成功检索到信息，则返回。
% 400：如果请求无效，则返回。
% 404：如果找不到数据库，则返回。
curDbInfo(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/database/current">>, [], undefined).

% 检索当前用户可以访问的所有数据库的列表
% GET /_api/database/user
% 检索当前用户可以访问的所有数据库的列表，而无需指定其他用户名或密码。
% 返回码
% 200：如果数据库列表编译成功，则返回。
% 400：如果请求无效，则返回。
visitDbs(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/database/user">>, [], undefined).

% 检索所有现有数据库的列表
% GET /_api/database
% 检索所有现有数据库的列表
% 注意：只能从_system数据库中检索数据库列表。
% 注意：您现在应该使用GET用户API来获取可用数据库的列表。
% 返回码
% 200：如果数据库列表编译成功，则返回。
% 400：如果请求无效，则返回。
% 403：如果请求未在_system数据库中执行，则返回。
allDbs(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/database">>, [], undefined).

% 创建一个新的数据库
% POST /_api/database
% 具有以下属性的JSON对象是必需的：
% name：必须包含一个有效的数据库名称。
% options：可选对象，可以包含以下属性：
% sharding：用于此数据库中新集合的分片方法。有效值为：“”，“ flexible”或“ single”。前两个是等效的。（仅集群）
% ReplicationFactor：在此数据库中创建的新集合的默认复制因子。特殊值包括“ satellite”和将其禁用复制的“ satellite”（将集合复制到每个数据库服务器）和“ 1”。（仅集群）
% writeConcern：在此数据库中创建的新集合的默认写关注。它确定在不同的DBServer上同步每个分片需要多少个副本。如果集群中的副本数量很少，那么分片将拒绝写入。但是，具有足够最新副本的分片写入将同时成功。writeConcern的值 不能大于ReplicationFactor。（仅集群）
% users：必须是最初为新数据库创建的用户对象数组。对于已经存在的用户，不会更改用户信息。如果未指定users或不包含任何用户，则将使用空字符串密码创建默认用户 root。这确保了新数据库在创建后将可访问。每个用户对象可以包含以下属性：
% username：要创建的用户的登录名
% passwd：用户密码（字符串）。如果未指定，则默认为空字符串。
% active：一个标志，指示是否应该激活用户帐户。默认值为true。如果设置为false，则用户将无法登录数据库。
% extra：带有额外用户信息的JSON对象。Extra中包含的数据 将为用户存储，但ArangoDB不会进一步解释。
% 创建一个新的数据库
% 响应是一个JSON对象，其属性结果设置为true。
% 注意：仅可以在_system数据库中创建新数据库。
% 返回码
% 201：如果数据库创建成功，则返回。
% 400：如果请求参数无效或具有指定名称的数据库已存在，则返回。
% 403：如果请求未在_system数据库中执行，则返回。
% 409：如果具有指定名称的数据库已经存在，则返回。
newDb(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/database">>, [], BodyStr, true).

% 删除现有数据库
% DELETE /_api/database/{database-name}
% 路径参数
% database-name（必填）：数据库的名称
% 删除数据库以及其中存储的所有数据。
% 注意：只能从_system数据库中删除数据库。该_SYSTEM数据库本身不能被丢弃。
% 返回码
% 200：如果成功删除数据库，则返回。
% 400：如果请求格式错误，则返回。
% 403：如果请求未在_system数据库中执行，则返回。
% 404：如果找不到数据库，则返回。
delDb(PoolNameOrSocket, Name) ->
   Path = <<"/_api/database/", Name/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined, true).
