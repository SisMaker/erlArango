-module(agAnalyzers).
-include("erlArango.hrl").

-compile(inline).
-compile({inline_size, 128}).
-compile([export_all, nowarn_export_all]).

% doc_address:https://www.arangodb.com/docs/stable/http/analyzers.html

% 分析仪的HTTP接口
% 可通过/_api/analyzer端点访问用于管理ArangoSearch Analyzer的RESTful API 。
% 有关简介以及可用的类型，属性和功能，请参见分析器的描述。

% 根据提供的定义创建一个新的分析器
% POST /_api/analyzer
% 具有以下属性的JSON对象是必需的：
%    name：分析器名称。
%    type：分析器类型。
%    properties：用于配置指定分析器类型的属性。
%    features：在分析器生成的字段上设置的一组功能。默认值为空数组。
% 根据提供的配置创建一个新的分析器。
% 返回码
%    200：名称和定义匹配的分析器已存在。
%    201：成功创建了新的分析器定义。
%    400：缺少一个或多个必需参数，或者一个或多个参数无效。
%    403：用户无权使用此配置创建和分析器。
newAnalyzer(PoolNameOrSocket, MapData) ->
   BodyStr = jiffy:encode(MapData),
   agHttpCli:callAgency(PoolNameOrSocket, ?AgPost, <<"/_api/analyzer">>, [], BodyStr).

% 返回分析器定义
% GET /_api/analyzer/{analyzer-name}
% 路径参数
%    Analyzer-name（必填）：要检索的分析器的名称。
% 检索指定分析器名称的完整定义。结果对象包含以下属性：
%    name：分析器名称
%    type：分析仪类型
%    properties：用于配置指定类型的属性
%    features：在分析器生成的字段上设置的功能集
% 返回码
%    200：分析器定义已成功检索。
%   404：不存在这种分析器配置。
getAnalyzer(PoolNameOrSocket, AnalyzerName) ->
   Path = <<"/_api/analyzer/", AnalyzerName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, Path, [], undefined).

% 返回可用的分析器定义列表
% GET /_api/analyzer
% 检索所有分析器定义的数组。结果数组包含具有以下属性的对象：
%    name：分析器名称
%    type：分析仪类型
%    properties：用于配置指定类型的属性
%    features：在分析器生成的字段上设置的功能集
% 返回码
% 200：分析器定义已成功检索。
getAnalyzerList(PoolNameOrSocket) ->
   agHttpCli:callAgency(PoolNameOrSocket, ?AgGet, <<"/_api/analyzer">>, [], undefined).

% 删除分析仪配置
% DELETE /_api/analyzer/{analyzer-name}
% 路径参数
%    Analyzer-name（必填）：要删除的分析器的名称。
% 查询参数
%    force （可选）：即使正在使用分析仪配置，也应将其删除。默认值为false。
% 删除由analyzer-name标识的Analyzer配置。
% 如果成功删除了分析器定义，将返回具有以下属性的对象：
% 错误：假
% name：删除的分析器的名称
% 返回码
%    200：分析仪配置已成功删除。
%    400：未提供分析器名称，或其他请求参数无效。
%    403：用户无权删除此分析器配置。
%    404：不存在这种分析器配置。
%    409：指定的分析器配置仍在使用中，并且省略了强制或 指定了错误。。
delAnalyzer(PoolNameOrSocket, AnalyzerName) ->
   Path = <<"/_api/analyzer/", AnalyzerName/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).

delAnalyzer(PoolNameOrSocket, AnalyzerName, QueryPars) ->
   QueryBinary = agMiscUtils:spellQueryPars(QueryPars),
   Path = <<"/_api/analyzer/", AnalyzerName/binary, QueryBinary/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?AgDelete, Path, [], undefined).
