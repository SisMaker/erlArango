-module(agDbMgr).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% 请注意，所有数据库管理操作只能通过默认数据库（_system）访问，而不能通过其他数据库访问。

%% 检索有关当前数据库的信息
%% GET /_api/database/current
curDbInfo(PoolName) ->
   agHttpCli:callAgency(PoolName, ?Get, <<"/_api/database/current">>, [], undefined).

%% 检索当前用户可以访问的所有数据库的列表
%% GET /_api/database/user
userVisitDbs(PoolName) ->
   agHttpCli:callAgency(PoolName, ?Get, <<"/_api/database/user">>, [], undefined, infinity, true).

%% 创建一个新的数据库 注意：仅可以在_system数据库中创建新数据库。
%% POST /_api/database
% 具有以下属性的JSON对象是必需的：
% name：必须包含一个有效的数据库名称。
% users：必须是最初为新数据库创建的用户对象数组。对于已经存在的用户，不会更改用户信息。如果未指定用户或不包含任何用户，则将使用空字符串密码创建默认用户 root。这确保了新数据库在创建后将可访问。每个用户对象可以包含以下属性：
%    username：要创建的用户的登录名
%    passwd：用户密码（字符串）。如果未指定，则默认为空字符串。
%    active：一个标志，指示是否应该激活用户帐户。默认值为true。如果设置为false，则用户将无法登录数据库。
%    extra：带有额外用户信息的JSON对象。Extra中包含的数据 将为用户存储，但ArangoDB不会进一步解释

newDb(PoolName, Name) ->
   NameStr = jiffy:encode(Name),
   agHttpCli:callAgency(PoolName, ?Post, <<"/_api/database">>, [], [<<"{\"name\":">>, NameStr, <<"}">>], infinity, true).

newDb(PoolName, Name, Users) ->

   BodyStr = jiffy:encode(#{<<"name">> => Name, <<"users">> => Users}),
   agHttpCli:callAgency(PoolName, ?Post, <<"/_api/database">>, [], BodyStr, infinity, true).

%% 删除现有数据库
%% DELETE /_api/database/{database-name}

delDb(PoolName, Name) ->
   Path = <<"/_api/database/", Name/binary>>,
   agHttpCli:callAgency(PoolName, ?Delete, Path, [], undefined, infinity, true).
