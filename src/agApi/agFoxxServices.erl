-module(agFoxxServices).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/foxx.html

% Foxx HTTP API
% 这些路由允许操作数据库中安装的Foxx服务。
%
% 有关Foxx及其JavaScript API的更多信息，请参见主要文档的Foxx章节。

% Foxx服务管理
% 这是用于管理Foxx服务的ArangoDB HTTP接口的简介。

% 列出已安装的服务
% GET /_api/foxx
% 查询参数
%    excludeSystem（可选）：是否应从结果中排除系统服务。
% 获取当前数据库中安装的服务列表。
% 返回具有以下属性的对象列表：
%    mount：服务的安装路径
%    development：如果服务以开发模式运行，则为true
%    legacy：如果服务以2.8旧版兼容模式运行，则为true
% 提供：服务清单的提供值或空对象
% 此外，如果在清单上设置了以下对象，则该对象可能包含以下属性：
%    name：标识服务类型的字符串
%    version：与semver兼容的版本字符串
% 返回码
%    200：如果请求成功，则返回。
getFoxxList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/foxx">>, [], undefined).

getFoxxList(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/foxx", QueryBinary/binary>>, [], undefined).

% 服务说明
% 服务元数据
% GET /_api/foxx/service
% 查询参数
%    mount（必需）：已安装服务的安装路径。
% 在给定的安装路径中获取服务的详细信息。
% 返回具有以下属性的对象：
%    mount：服务的安装路径
%    path：服务的本地文件系统路径
%    development：如果服务以开发模式运行，则为true
%    legacy：如果服务以2.8旧版兼容模式运行，则为true
%    manifest：服务的规范化JSON清单
% 此外，如果在清单上设置了以下对象，则该对象可能包含以下属性：
%    name：标识服务类型的字符串
%    version：与semver兼容的版本字符串
% 返回码
% 200：如果请求成功，则返回。
% 400：如果安装路径未知，则返回。
getFoxxService(PoolNameOrSocket, Mount) ->
   Path = <<"/_api/foxx/service?mount=", (agMiscUtils:toBinary(Mount))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 安装新服务
% POST /_api/foxx
% 查询参数
%    mount（必需）：应在其中安装服务的安装路径。
%    开发（可选）：设置true为启用开发模式。
%    安装程序（可选）：设置为false不运行服务的安装脚本。
%    legacy（可选）：设置为true以2.8旧版兼容性模式安装服务。
% 在给定的安装路径上安装给定的新服务。
% 请求主体可以是以下任何格式：
%    application/zip：包含服务的原始zip捆绑包
%    application/javascript：独立的JavaScript文件
%    application/json：服务定义为JSON
%    multipart/form-data：服务定义为多部分形式
% 服务定义是具有以下属性或字段的对象或表单：
%    configuration：描述配置值的JSON对象
%    依赖项：一个描述依赖项设置的JSON对象
%    source：服务器文件系统上的标准URL或绝对路径
% 使用多部分数据时，源字段也可以是包含zip捆绑包或独立JavaScript文件的文件字段。
% 使用独立的JavaScript文件时，将执行给定的文件来定义我们服务的HTTP端点。main与在服务清单字段中定义的相同。
% 如果源是URL，则必须可以从服务器访问该URL。如果source是文件系统路径，则该路径将在服务器上解析。在任何情况下，路径或URL都应解析为zip包，JavaScript文件或（如果是文件系统路径）目录。
% 请注意，在具有多个协调器的群集中使用文件系统路径时，文件系统路径必须解析为每个协调器上的等效文件。
% 返回码
%    201：如果请求成功，则返回。
installFoxx(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 卸载服务
% DELETE /_api/foxx/service
% 查询参数
%    mount（必需）：已安装服务的安装路径。
%    teardown （可选）：设置为false不运行服务的拆卸脚本。
% 从数据库和文件系统中删除给定安装路径中的服务。
% 成功返回空响应。
% 返回码
%     204：如果请求成功，则返回。
uninstallFoxx(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/service", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 更换服务
% PUT /_api/foxx/service
% 查询参数
%    mount（必需）：已安装服务的安装路径。
%    teardown （可选）：设置为false不运行旧服务的拆卸脚本。
%    setup （可选）：设置为false不运行新服务的安装脚本。
%    legacy（可选）：设置为true以2.8 Legacy兼容模式安装新服务。
%    force （可选）：设置为true强制安装服务，即使在给定的安装下未安装任何服务也是如此。
% 从数据库和文件系统中删除给定安装路径中的服务。然后在相同的安装路径上安装给定的新服务。
% 这与执行旧服务的卸载，然后安装新服务的安全性等效。在删除旧服务之前，将检查新服务的主文件和脚本文件（如果有）是否存在基本语法错误。
% 请求主体可以是以下任何格式：
%    application/zip：包含服务的原始zip捆绑包
%    application/javascript：独立的JavaScript文件
%    application/json：服务定义为JSON
%    multipart/form-data：服务定义为多部分形式
% 服务定义是具有以下属性或字段的对象或表单：
% configuration：描述配置值的JSON对象
% 依赖项：一个描述依赖项设置的JSON对象
% source：服务器文件系统上的标准URL或绝对路径
% 使用多部分数据时，源字段也可以是包含zip捆绑包或独立JavaScript文件的文件字段。
% 使用独立的JavaScript文件时，将执行给定的文件来定义我们服务的HTTP端点。main与在服务清单字段中定义的相同。
% 如果源是URL，则必须可以从服务器访问该URL。如果source是文件系统路径，则该路径将在服务器上解析。在任何情况下，路径或URL都应解析为zip包，JavaScript文件或（如果是文件系统路径）目录。
% 请注意，在具有多个协调器的群集中使用文件系统路径时，文件系统路径必须解析为每个协调器上的等效文件。
% 返回码
%    200：如果请求成功，则返回。
replaceFoxx(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/service", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 升级服务
% PATCH /_api/foxx/service
% 查询参数
%     mount（必需）：已安装服务的安装路径。
%     teardown（可选）：设置为true运行旧服务的拆卸脚本。
%     setup（可选）：设置为false不运行新服务的安装脚本。
%     legacy（可选）：设置为true以2.8 Legacy兼容模式安装新服务。
%     force（可选）：设置为true强制安装服务，即使在给定的安装下未安装任何服务也是如此。
% 将给定的新服务安装在给定的安装路径上当前安装的服务之上。仅建议在同一服务的不同版本之间进行切换时使用。
% 与替换服务不同，升级服务会保留旧服务的配置和依赖关系（如果有），因此仅应用于将现有服务迁移到新服务或等效服务。
% 请求主体可以是以下任何格式：
%     application/zip：包含服务的原始zip捆绑包
%     application/javascript：独立的JavaScript文件
%     application/json：服务定义为JSON
%     multipart/form-data：服务定义为多部分形式
% 服务定义是具有以下属性或字段的对象或表单：
% configuration：描述配置值的JSON对象
% 依赖项：一个描述依赖项设置的JSON对象
% source：服务器文件系统上的标准URL或绝对路径
% 使用多部分数据时，源字段也可以是包含zip捆绑包或独立JavaScript文件的文件字段。
% 使用独立的JavaScript文件时，将执行给定的文件来定义我们服务的HTTP端点。main与在服务清单字段中定义的相同。
% 如果源是URL，则必须可以从服务器访问该URL。如果source是文件系统路径，则该路径将在服务器上解析。在任何情况下，路径或URL都应解析为zip包，JavaScript文件或（如果是文件系统路径）目录。
% 请注意，在具有多个协调器的群集中使用文件系统路径时，文件系统路径必须解析为每个协调器上的等效文件。
% 返回码
% 200：如果请求成功，则返回。
upgradeFoxx(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/service", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

% Foxx服务配置/依赖关系
% 这是用于管理Foxx服务配置和依赖关系的ArangoDB HTTP接口的简介。
%
% 获取配置选项
% GET /_api/foxx/configuration
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 在给定的安装路径中获取服务的当前配置。
% 返回一个对象，该对象将配置选项名称映射到其定义，包括易于理解的标题和当前值（如果有）。
% 返回码
% 200：如果请求成功，则返回。
getFoxxConfig(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/configuration", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 更新配置选项
% PATCH /_api/foxx/configuration
% 查询参数
%    mount（必需）：已安装服务的安装路径。
% 请求正文（json）
% JSON对象映射配置选项名称为其新值。任何省略的选项将被忽略。
% 替换给定服务的配置。
% 返回一个将所有配置选项名称映射到其新值的对象。
% 返回码
% 200：如果请求成功，则返回。
updateFoxxConfig(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/configuration", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

% 替换配置选项
% PUT /_api/foxx/configuration
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 请求正文（json）
% JSON对象映射配置选项名称为其新值。任何省略的选项将重置为默认值或标记为未配置。
% 完全替换给定服务的配置。
% 返回一个将所有配置选项名称映射到其新值的对象。
% 返回码
%     200：如果请求成功，则返回。
replaceFoxxConfig(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/configuration", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% 获取依赖项选项
% GET /_api/foxx/dependencies
% 查询参数
%    mount（必需）：已安装服务的安装路径。
% 在给定的安装路径中获取服务的当前依赖关系。
% 返回一个对象，该对象将依赖项名称映射到它们的定义，包括易于理解的标题和当前的安装路径（如果有）。
% 返回码
% 200：如果请求成功，则返回。
getFoxxDependencies(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/dependencies", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).


% 更新依赖项选项
% PATCH /_api/foxx/dependencies
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 请求正文（json）
% 映射依赖项名称到其新安装路径的JSON对象。任何省略的依赖项将被忽略。
% 替换给定服务的依赖项。
% 返回一个将所有依赖项名称映射到其新安装路径的对象。
% 返回码
%     200：如果请求成功，则返回。
updateFoxxDependencies(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/dependencies", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPatch, Path, [], BodyStr).

% 替换依赖项选项
% PUT /_api/foxx/dependencies
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 请求正文（json）
% 映射依赖项名称到其新安装路径的JSON对象。任何省略的依赖项将被禁用。
% 完全替换给定服务的依赖项。
% 返回一个将所有依赖项名称映射到其新安装路径的对象。
% 返回码
% 200：如果请求成功，则返回。
replaceFoxxDependencies(PoolNameOrSocket, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/dependencies", QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPut, Path, [], BodyStr).

% Foxx服务杂项
%
% 列出服务脚本
% GET /_api/foxx/scripts
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 获取服务定义的脚本列表。
% 返回一个将原始脚本名称映射到人类友好名称的对象。
% 返回码
%     200：如果请求成功，则返回。
getFoxxScripts(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/scripts", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 运行服务脚本
% POST /_api/foxx/scripts/{name}
% 路径参数
%     名称（必填）：要运行的脚本的名称。
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 请求正文（json）
% 一个任意的JSON值，将被解析并作为第一个参数传递给脚本。
% 在给定的安装路径上为服务运行给定的脚本。
% 返回脚本的导出（如果有）。
% 返回码
%     200：如果请求成功，则返回。
runFoxxScripts(PoolNameOrSocket, ScriptName, MapData, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/scripts/", ScriptName/binary, QueryBinary/binary>>,
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], BodyStr).

% 运行服务测试
% POST /_api/foxx/tests
% 查询参数
%     mount （必需）：已安装服务的安装路径。
%     reporter （可选）：测试记者使用。
%     idiomatic （可选）：与报告器使用匹配的格式，而与Accept标头无关。
%     filter（可选）：仅运行全名（包括完整的测试套件和测试用例）与该字符串匹配的测试。
% 在给定的安装路径上运行该服务的测试并返回结果。
% 支持的测试报告者为：
%     default：测试用例的简单列表
%     suite：嵌套在套件中的测试用例的对象
%     stream：测试结果的原始流
%     xunit：与XUnit / JUnit兼容的结构
%     tap：原始TAP兼容流
% 该接受请求报头可以被用于进一步控制的响应格式：
% 使用流报告器时，application/x-ldjson将导致响应主体被格式化为以换行符分隔的JSON流。
% 使用点击报告程序text/plain或时，text/*将导致响应正文被格式化为纯文本TAP报告。
% 使用xunit报告程序时，application/xml或text/xml将导致响应正文被格式化为XML而不是JSONML。
% 否则，响应主体将被格式化为非prettyprinted JSON。
% 返回码
%     200：如果请求成功，则返回。
runFoxxTest(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/tests", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], undefined).

% 启用开发模式
% POST /_api/foxx/development
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 将服务置于开发模式。
% 当服务在开发模式下运行时，每次服务处理请求时，将从文件系统重新加载服务，并且将重新执行其设置脚本（如果有）。
% 在具有多个协调器的集群中运行ArangoDB时，请注意，一个协调器上文件系统的更改不会在其他协调器上反映出来。这意味着只要任何服务都在开发模式下运行，就应该将协调器视为不一致。
% 返回码
%  200：如果请求成功，则返回。
enableFoxxDevelopment(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/development", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], undefined).

% 禁用开发模式
% DELETE /_api/foxx/development
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 将给定安装路径上的服务置于生产模式。
% 在具有多个协调器的集群中运行ArangoDB时，这会将所有其他协调器上的服务替换为该协调器上的版本。
% 返回码
% 200：如果请求成功，则返回。
disableFoxxDevelopment(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/development", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

% 服务自述文件
% GET /_api/foxx/readme
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 获取服务的README或README.md文件的内容（如果有）。
% 返回码
%     200：如果请求成功，则返回。
%     204：如果未找到自述文件，则返回。
getFoxxReadme(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/readme", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 招摇说明
% GET /_api/foxx/swagger
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 在给定的安装路径中获取服务的Swagger API描述。
% 响应主体将是服务API的与OpenAPI 2.0兼容的JSON描述。
% 返回码
% 200：如果请求成功，则返回。
getFoxxSwagger(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/swagger", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 下载服务包
% POST /_api/foxx/download
% 查询参数
%     mount（必需）：已安装服务的安装路径。
% 下载服务目录的zip捆绑包。
% 启用开发模式后，将始终创建一个新捆绑包。
% 否则，捆绑软件将代表该ArangoDB实例上安装的服务的版本。
% 返回码
%     200：如果请求成功，则返回。
%     400：如果安装路径未知，则返回。
downloadFoxxBundle(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/download", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], undefined).

% 提交本地服务状态
% POST /_api/foxx/commit
% 查询参数
%     replace（可选）：覆盖数据库中的现有服务文件，即使它们已经存在也是如此。
% 将协调器的本地服务状态提交到数据库。
% 这可用于解决由于数据丢失而无法自动修复的协调器之间的服务冲突。
% 返回码
%     204：如果请求成功，则返回。
commitFoxxState(PoolNameOrSocket, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/foxx/commit", QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, Path, [], undefined).