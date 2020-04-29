-module(agUserMgr).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/user-management.html

% 用户管理的HTTP接口
% 这是用于管理用户的ArangoDB HTTP接口的简介。
% 该界面提供了添加，更新和删除用户的简单方法。通过此接口管理的所有用户都将存储在系统集合 _users中。您永远不要直接操作_users集合。
% 该专用接口有意不提供常规文档REST API中可用的所有功能。
% 请注意，ArangoDB的复制中不包括用户操作。

% 创建一个新用户。
% POST /_api/user
% 具有以下属性的JSON对象是必需的：
%    user：用户名（字符串）。这是强制性的。
%    passwd：用户密码（字符串）。如果未指定密码，将使用空字符串。如果传递特殊值ARANGODB_DEFAULT_ROOT_PASSWORD，则密码将设置为存储在环境变量中的值 ARANGODB_DEFAULT_ROOT_PASSWORD。这可用于将实例变量传递给ArangoDB。例如，来自Amazon的实例标识符。
%    active：一个可选标志，用于指定用户是否处于活动状态。如果未指定，则默认为true
%    extra：一个可选的JSON对象，其中包含有关用户的任意额外数据。
% 创建一个新用户。您需要服务器访问级别“ 管理 ”才能执行此REST调用。
% 返回码
%    201：如果服务器可以添加用户，则返回
%    400：如果JSON格式不正确或请求中缺少必需数据。
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
%    409：如果已经存在同名用户，则返回。
newUser(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/user">>, [], BodyStr).

% 设置数据库访问级别。
% PUT /_api/user/{user}/database/{dbname}
% 路径参数
%    user （必填）：用户名。
%    dbname（必填）：数据库的名称。
% 具有以下属性的JSON对象是必需的：
%    grant：使用“ rw”将数据库访问级别设置为Administrate。
%    使用“ ro”将数据库访问级别设置为Access。
%    使用“none”将数据库访问级别设置为“ 无访问权”。
% 为用户user的数据库dbname设置数据库访问级别。您需要管理服务器访问级别才能执行此REST调用。
% 返回码
%    200：如果访问级别更改成功，则返回。
%    400：如果JSON格式不正确或请求中缺少必需数据。
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
setUserDbAccessLevel(PoolNameOrSocket, UserName, DbName, MapData) ->
   Path = <<"/_api/user/", UserName/binary, "/database/", DbName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 设置收集访问级别。
% PUT /_api/user/{user}/database/{dbname}/{collection}
% 路径参数
%    user（必填）：用户名。
%    dbname（必填）：数据库的名称。
%    collection （必填）：集合的名称。
% 具有以下属性的JSON对象是必需的：
%    grant：使用“ rw”将收集级别访问权限设置为“ 读/写”。
%    使用“ ro”将收集级别访问权限设置为“ 只读”。
%    使用“none”将收集级别访问权限设置为“ 无访问权限”。
%
% 设置用于收集访问级别收集在数据库DBNAME 用户的用户。您需要管理服务器访问级别才能执行此REST调用。
% 返回码
%    200：成功修改访问权限后返回。
%    400：如果JSON格式不正确或请求中缺少必需数据。
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
setUserCollAccessLevel(PoolNameOrSocket, UserName, DbName, CollName, MapData) ->
   Path = <<"/_api/user/", UserName/binary, "/database/", DbName/binary, "/", CollName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 清除数据库访问级别，恢复为默认访问级别
% DELETE /_api/user/{user}/database/{dbname}
% 路径参数
%    user（必填）：用户名。
%    dbname（必填）：数据库的名称。
%
% 清除用户user的数据库dbname的数据库访问级别。因此，将使用默认的数据库访问级别。如果没有定义的默认数据库访问级别，则默认为No access。您需要获得_system数据库的权限才能执行此REST调用。
%
% 返回码
%    202：成功更改访问权限后返回。
%    400：如果JSON格式不正确或请求中缺少必需数据。。
clearUserDbAccessLevel(PoolNameOrSocket, UserName, DbName) ->
   Path = <<"/_api/user/", UserName/binary, "/database/", DbName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 清除集合访问级别，恢复为默认访问级别
% DELETE /_api/user/{user}/database/{dbname}/{collection}
% 路径参数
%    user（必填）：用户名。
%    dbname（必填）：数据库的名称。
%    collection（必填）：集合的名称。
%
% 清除用户user的数据库dbname中集合集合的集合访问级别。因此，将使用默认的集合访问级别。如果没有定义的默认集合访问级别，则默认为No access。您需要具有_system数据库的权限才能执行此REST调用。
% 返回码
%    202：成功更改访问权限后返回。
%    400：如果有错误
clearUserCollAccessLevel(PoolNameOrSocket, UserName, DbName, CollName) ->
   Path = <<"/_api/user/", UserName/binary, "/database/", DbName/binary, "/", CollName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 列出用户的可访问数据库
% GET /_api/user/{user}/database/
% 路径参数
%    用户（必填）：您要查询数据库的用户名。
% 查询参数
%    full（可选）：返回所有数据库和所有集合的完整访问级别集。
% 获取指定用户可用的数据库列表。您需要 管理的服务器访问级别，以执行该REST调用。
% 该调用将返回一个具有指定用户按数据库访问权限的JSON对象。该结果对象将包含数据库名称作为对象键，以及相关的权限的数据库值。
% 如果您指定full，则结果将包含数据库的权限以及集合的权限。
% 返回码
%    200：如果可以返回可用数据库列表，则返回。
%    400：如果访问权限不正确等
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
getUserDbList(PoolNameOrSocket, UserName) ->
   Path = <<"/_api/user/", UserName/binary, "/database/">>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

getUserDbList(PoolNameOrSocket, UserName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/user/", UserName/binary, "/database/", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 获取特定的数据库访问级别
% GET /_api/user/{user}/database/{dbname}
% 路径参数
%    user（必填）：您要查询数据库的用户名。
%    dbname（必填）：要查询的数据库的名称
% 获取特定数据库的数据库访问级别
% 返回码
%    200：如果可以返回访问级别，则返回
%    400：如果访问权限不正确等
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
getUserDbAccessLevel(PoolNameOrSocket, UserName, DbName) ->
   Path = <<"/_api/user/", UserName/binary, "/database/", DbName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 获取特定集合访问级别
% GET /_api/user/{user}/database/{dbname}/{collection}
% 路径参数
%    user（必填）：您要查询数据库的用户名。
%    dbname（必填）：要查询的数据库的名称
%    collection（必填）：集合的名称
% 返回特定集合的集合访问级别
% 返回码
%    200：如果可以返回访问级别，则返回
%    400：如果访问权限不正确等
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
getUserCollAccessLevel(PoolNameOrSocket, UserName, DbName, CollName) ->
   Path = <<"/_api/user/", UserName/binary, "/database/", DbName/binary, "/", CollName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 替换现有用户属性。
% PUT /_api/user/{user}
% 路径参数
%    user（必填）：用户名
% 具有以下属性的JSON对象是必需的：
%    passwd：用户密码（字符串）。必须指定密码，但是密码允许使用空字符串
%    active：一个可选标志，用于指定用户是否处于活动状态。如果未指定，则默认为true
%    extra：一个可选的JSON对象，其中包含有关用户的任意额外数据。
% 替换现有用户的数据。现有用户的名称必须在user中指定。您需要服务器访问级别“ 管理 ”才能执行此REST调用。另外，用户可以更改他/她自己的数据。
% 返回码
%    200：如果用户数据可以被服务器替换，则返回。
%    400：JSON格式不正确或请求中缺少必需数据
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
%    404：指定的用户不存在
replaceUser(PoolNameOrSocket, UserName, MapData) ->
   Path = <<"/_api/user/", UserName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 修改现有用户的属性
% PATCH /_api/user/{user}
% 路径参数
%    user（必填）：用户名
% 具有以下属性的JSON对象是必需的：
%    passwd：用户密码（字符串）。必须指定密码，但是密码允许使用空字符串
%    active：一个可选标志，用于指定用户是否处于活动状态。如果未指定，则默认为true
%    extra：一个可选的JSON对象，其中包含有关用户的任意额外数据。
% 部分更新现有用户的数据。现有用户的名称必须在user中指定。您需要服务器访问级别“ 管理 ”才能执行此REST调用。另外，用户可以更改他/她自己的数据。
% 返回码
%    200：如果用户数据可以被服务器替换，则返回。
%    400：JSON格式不正确或请求中缺少必需数据。
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
%    404：指定的用户不存在
updateUser(PoolNameOrSocket, UserName, MapData) ->
   Path = <<"/_api/user/", UserName/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

% 永久删除用户。
% DELETE /_api/user/{user}
%路径参数
%   user（必填）：用户名
%删除由user标识的现有用户。您需要管理的服务器访问级别，以执行该REST调用。
%返回码
%   202：如果服务器删除了用户，则返回
%   401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%   403：如果您没有访问服务器访问级别，则返回。
%   404：指定的用户不存在
delUser(PoolNameOrSocket, UserName) ->
   Path = <<"/_api/user/", UserName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 获取用户的属性。
% GET /_api/user/{user}
% 路径参数
%    user（必填）：用户名
% 获取有关指定用户的数据。您可以获取有关您自己的信息，或者您需要管理服务器访问级别才能执行此REST调用。
% 返回码
%    200：找到用户
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
%    404：指定用户名不存在。
getUser(PoolNameOrSocket, UserName) ->
   Path = <<"/_api/user/", UserName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 获取用户的属性。
% GET /_api/user/
% 获取有关所有用户的数据。您需要管理服务器访问级别才能执行此REST调用。否则，您只会获得有关您自己的信息。
% 调用成功后将返回至少具有以下属性的JSON对象：
%    user：用户名（字符串）。
%    active：一个可选标志，用于指定用户是否处于活动状态。
%    extra：一个可选的JSON对象，其中包含有关用户的任意额外数据。
% 返回码
%    200：找到的用户
%    401：如果您没有对_system 数据库的访问数据库访问级别，则返回。
%    403：如果您没有访问服务器访问级别，则返回。
getUserList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/user/">>, [], undefined).
