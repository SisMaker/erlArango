-module(agHotBackup).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/hot-backup.html

% HTTP接口进行热备份和还原
% 在v3.5.1中引入
% 这是用于热备份和还原的ArangoDB HTTP接口的简介。
% 热备份仅在企业版中可用 。
% 热备份
% 热备份接近整个 ArangoDB服务的即时一致快照 。这包括在任何给定时间的所有数据库，集合，索引，视图定义和用户。
% 对于创建，可以指定标签，如果省略，则将其替换为生成的UUID。然后，此标签与时间戳组合以为创建的热备份生成标识符。随后，所有其他API在这些标识符上运行。
% 以下API专用于处理POST操作。
% 使用API​​之前，请确保了解热备份的所有方面，以及所有要求和限制。

% 创建本地备份
% POST /_admin/backup/create
% 具有以下属性的JSON对象是必需的：
%    label：此备份的标签。该标签与时间戳字符串一起使用，创建唯一的备份标识符<timestamp>_<label>。如果未指定标签，则假定为空字符串，并为此ID的此部分创建默认的UUID。
%    timeout：操作尝试获取一致快照的时间（以秒为单位）。默认值为120秒。
%    allowInconsistent：如果将此标志设置为，true并且在给定的超时时间内无法获取全局事务锁定，则会进行可能不一致的备份。此标志的默认值为false，在这种情况下，超时将导致HTTP 408错误。
%    force：如果将此标志设置为，true并且在给定的超时时间内无法获取全局事务锁定，则将强制中止所有正在运行的事务，以确保可以创建一致的备份。这不包括JavaScript事务。它等待事务最多中止 timeout几秒钟。因此，使用force请求超时将增加一倍。中止交易几乎肯定不是您想要的应用程序。在存在中间提交的情况下，它甚至可能破坏交易的原子性。仅在需要不惜一切代价进行一致备份的情况下，使用风险自负。推荐的默认值是false。如果同时 allowInconsistent并force设置为true，则后者优先，事务中止。仅在群集中可用。
% 使用给定标签“尽快”创建一致的备份，非常类似于及时快照。短语“尽快”中的歧义是指下一个窗口，在该窗口中可以获得跨所有数据库的全局写锁定以保证一致性。请注意，备份首先与原始数据位于同一台计算机和硬盘驱动器上。确保将其上传到远程站点以进行实际备份。
% 返回码
%    201：如果一切正常，则返回代码201。
%    400：如果使用错误的参数或除之外的任何HTTP方法调用了create命令POST，则返回HTTP 400。具体细节在返回的错误文档中有详细说明。
%    408：如果操作无法在超时时间内获得全局事务锁定，则返回HTTP 408。
newBackup(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/backup/create">>, [], BodyStr).

% 从本地备份还原
% POST /_admin/backup/restore
% 具有以下属性的JSON对象是必需的：
%    id：要还原的备份的ID。
% 使用给定的ID及时从快照恢复一致的备份。备份快照必须位于本地ArangoDB服务上。
% 返回码
%    200：如果可以恢复备份，则返回。请注意，单个服务器和群集之间不可避免存在差异。在单个服务器中，请求成功返回，但是还原仅在之后执行。在集群中，仅在成功完成还原操作后才返回请求。群集行为显然是所需的行为，但是在单个实例中，无法在重新启动期间保持连接打开。
%    400：如果使用错误的参数或除以外的任何HTTP方法调用了restore命令POST，则返回HTTP 400。具体细节在返回的错误文档中有详细说明。
restoreBackup(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/backup/restore">>, [], BodyStr).

% 删除特定的本地备份
% POST /_admin/backup/delete
% 具有以下属性的JSON对象是必需的：
% id：此备份的标识符。
% 删除给定标识的特定本地备份id。
% 返回码
%    200：如果一切正常，则返回此代码200。
%    400：如果使用错误的参数或除以外的任何HTTP方法调用delete命令POST，则返回HTTP 400。
%    404：如果id找不到与该标识符相对应的备份。
delBackup(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/backup/delete">>, [], BodyStr).

% 列出所有本地备份
% POST /_admin/backup/list
% 具有以下属性的JSON对象是必需的：
%    id：正文可以为空（在这种情况下，将列出所有可用的备份），也可以是具有attribute的对象，该属性id为字符串。在后一种情况下，返回的列表仅限于具有给定ID的备份。
% 列出所有本地找到的备份。
% 返回码
%    200：如果一切正常，则返回代码200。
%    400：如果使用错误的参数调用list命令，则 返回HTTP 400。
%    404：如果id给出了ID或ID列表，但未找到给定ID作为备份的标识符，则返回HTTP 404 NOT FOUND。
%    405：如果使用以外的任何HTTP方法调用了list命令POST，则返回HTTP 405 METHOD NOT ALLOWED。
getBackupList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/backup/list">>, [], undefined).

% 结果由list热备份的一个对象组成id，其中id唯一地标识了特定的热备份，version描绘了用于创建任何单个热备份的ArangoDB的版本，并datetime显示了创建热备份的时间。
% 其他参数包括：备份大小（以字节为单位）sizeInBytes，单个数据文件nrFiles的数量（以），创建时的数据库服务器nrDBServers的数量（以），备份部分的数量（在当前可访问的db服务器上找到的）为nrPiecesPresent。
% 如果创建的备份允许不一致，则将其表示为potentiallyInconsistent。的available布尔参数被紧密地连接到备份到存在并且准备好被所有分贝服务器上恢复。它是true 除非当前可访问的数据库服务器数量与备份中列出的
% 数据库服务器数量不匹配。

% 将备份上传到远程存储库
% 上传特定的本地备份
% POST /_admin/backup/upload
% 具有以下属性的JSON对象是必需的：
%    id：此备份的标识符。计划上载操作时，这是必需的。在这种情况下，请忽略该uploadId 属性。
%    remoteRepository：远程存储库的URL。计划上载操作时，这是必需的。在这种情况下，请忽略该uploadId属性。所提供的存储库URL进行了规范化和验证，如下所示：必须出现一个冒号，以分隔配置节名称和路径。冒号之前的URL前缀必须作为键存在于以下配置对象中。冒号前不能出现斜线。多个反斜杠折叠为1， ..并相应.地应用。本地存储库必须是绝对路径，并且必须以开头/。尾随/被删除。
%    config：远程存储库的配置。计划上载操作时，这是必需的。在这种情况下，请忽略该uploadId 属性。有关对象的说明，请参见手册中的arangobackup程序说明config。
%    uploadId：上传ID，用于指定要查询的上传操作进度或要中止的上传操作。如果指定此参数，则忽略上述所有主体参数。
%    abort：true如果正在运行的上载操作应中止，则将其设置为。在这种情况下，唯一需要的其他body参数是uploadId。
% 将特定的本地备份上载到远程存储库，或查询先前计划的上载操作的进度，或中止正在运行的上载操作。
% 返回码
%    200：如果一切正常，如果查询进度或操作中止，则返回代码200。
%    202：如果一切正常，如果计划了新操作，则返回代码202。
%    400：如果使用错误的参数或除以外的任何HTTP方法调用了上载命令POST，则返回HTTP 400。
%    401：如果对转储存储库的身份验证失败，则返回HTTP 400。
%    404：如果id 找不到对应于标识符的备份，或者没有已知的上载操作uploadId。
uploadBackup(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/backup/upload">>, [], BodyStr).

% 下载特定的本地备份
% POST /_admin/backup/download
% 具有以下属性的JSON对象是必需的：
%    id：此备份的标识符。计划下载操作时，这是必需的。在这种情况下，请忽略该downloadId 属性。
%    remoteRepository：远程存储库的URL。计划下载操作时，这是必需的。在这种情况下，请忽略该downloadId属性。所提供的存储库URL进行了规范化和验证，如下所示：必须出现一个冒号，以分隔配置节名称和路径。冒号之前的URL前缀必须作为键存在于以下配置对象中。冒号前不能出现斜线。多个反斜杠折叠为1， ..并相应.地应用。本地存储库必须是绝对路径，并且必须以开头/。尾随/被删除。
%    config：远程存储库的配置。计划下载操作时，这是必需的。在这种情况下，请忽略该downloadId 属性。有关对象的说明，请参见手册中的arangobackup程序说明config。
%    downloadId：下载ID，用于指定要查询的下载操作进度或要中止的下载操作。如果指定此参数，则忽略上述所有主体参数。
%    abort：true如果正在运行的下载操作应中止，则将其设置为。在这种情况下，唯一需要的其他body参数是downloadId。
% 从远程存储库下载特定的本地备份，或查询先前计划的下载操作的进度，或中止正在运行的下载操作。
% 返回码
%    200：如果一切正常，如果查询进度或操作中止，则返回代码200。
%    202：如果一切正常，如果计划了新操作，则返回代码202。
%    400：如果使用错误的参数或除以外的任何HTTP方法调用了download命令POST，则返回HTTP 400。
%    401：如果对转储存储库的身份验证失败，则返回HTTP 401。
%    404：如果id 找不到与该标识符相对应的备份，或者如果没有已知的与的下载操作downloadId。
downloadBackup(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_admin/backup/download">>, [], BodyStr).
