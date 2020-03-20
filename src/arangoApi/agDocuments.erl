-module(agDocuments).
-include("erlArango.hrl").

-compile([export_all, nowarn_export_all]).

%% doc_address:https://www.arangodb.com/docs/stable/http/document.html
% 读取单个文档
% GET /_api/document/{collection}/{key}
% 路径参数
% 集合（必填）：要从中读取文档的集合的名称。
% 密钥（必填）：文档密钥。
% 标头参数
% If-None-Match（可选）：如果给出了“ If-None-Match”标头，则它必须恰好包含一个Etag。如果文档版本与给定的Etag不同，则返回文档。否则，返回HTTP 304。
% If-Match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则返回文档。否则，返回HTTP 412。
% 返回由document-id标识的文档。返回的文档包含三个特殊属性：_id包含文档标识符，_key包含唯一标识给定集合中的文档的键，_rev包含修订版。
% 返回码
% 200：如果找到文档，则返回
% 304：如果给出“ If-None-Match”标题并且文档具有相同版本，则返回
% 404：如果找不到文档或集合，则返回
% 412：如果给出“ If-Match”标头并且找到的文档具有不同版本，则返回412。响应还将在_rev属性中包含找到的文档的当前修订。此外，将返回属性_id和_key。
getDocument(PoolNameOrSocket, CollName, Key) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, Path, [], undefined).

getDocument(PoolNameOrSocket, CollName, Key, Headers) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Get, Path, Headers, undefined).


% 读取单个文档头
% HEAD /_api/document/{collection}/{key}
% 路径参数
% 集合（必填）：要从中读取文档的集合的名称。
% 密钥（必填）：文档密钥。
% 标头参数
% If-None-Match（可选）：如果给出了“ If-None-Match”标头，则它必须恰好包含一个Etag。如果当前文档修订版不等于指定的Etag，则返回HTTP 200响应。如果当前文档修订版与指定的Etag相同，则返回HTTP 304。
% If-Match（可选）：如果给出了“ If-Match”标头，则它必须恰好包含一个Etag。如果文档的版本与给定的Etag相同，则返回文档。否则，返回HTTP 412。
% 类似于GET，但仅返回标头字段，而不返回正文。您可以使用此调用来获取文档的当前版本，或检查文档是否已删除。
% 返回码
% 200：如果找到文档，则返回
% 304：如果给出“ If-None-Match”标题并且文档具有相同版本，则返回
% 404：如果找不到文档或集合，则返回
% 412：如果给出“ If-Match”标头并且找到的文档具有不同版本，则返回412。响应还将在Etag标头中包含找到的文档的当前版本。

getDocHead(PoolNameOrSocket, CollName, Key) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Head, Path, [], undefined).

getDocHead(PoolNameOrSocket, CollName, Key, Headers) ->
   Path = <<"/_api/document/", CollName/binary, "/", (agMiscUtils:toBinary(Key))/binary>>,
   agHttpCli:callAgency(PoolNameOrSocket, ?Head, Path, Headers, undefined).


