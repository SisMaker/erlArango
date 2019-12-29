-module(agDbMgr).

-export([]).


%% 请注意，所有数据库管理操作只能通过默认数据库（_system）访问，而不能通过其他数据库访问。

%% 检索有关当前数据库的信息
% GET /_api/database/current
dbCurrent(PoolName) ->
   ok.