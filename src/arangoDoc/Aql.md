插入文档
	语法是INSERT document INTO collectionName。该文档是一个对象，您可以从JavaScript或JSON中了解它，它由属性键和值对组成。
	属性键周围的引号在AQL中是可选的。键总是字符序列（字符串），而属性值可以有不同的类型：
	空值
	布尔值（true，false）
	数字（整数和浮点数）
	串
	排列
	宾语

	该LET关键字定义一个带有名称数据的变量和一个对象数组作为值，因此LET variableName = valueExpression表达式是一个文字数组定义[ {...}, {...}, ... ]。

	FOR variableName IN expression用于迭代数据数组的每个元素 。在每个循环中，将一个元素分配给变量d。然后在INSERT语句中使用此变量而不是文字对象定义。基本上是做什么的：

	INSERT {
		"name": "Robert",
		"surname": "Baratheon",
		"alive": false,
		"traits": ["A","H","C"]
	} INTO Characters

	INSERT {
		"name": "Jaime",
		"surname": "Lannister",
		"alive": true,
		"age": 36,
		"traits": ["A","F","B"]
	} INTO Characters

	...
	注意：AQL不允许INSERT在单个查询中针对同一集合的多个操作。然而， 允许它作为FOR循环体，插入多个文档，就像我们对上面的查询所做的那样。

	
	FOR c IN Characters_1 RETURN c
	循环的语法是FOR variableName IN collectionName。对于集合中的每个文档，将为c分配一个文档，然后根据循环体返回该文档。查询返回我们先前存储的所有字符。
	该文档包含我们存储的四个属性，以及数据库系统添加的另外三个属性。每个文档都需要一个唯一的文档_key，用于在集合中标识它。它_id是计算属性，集合名称，
	正斜杠/和文档键的串联。它唯一标识数据库中的文档。_rev是系统管理的修订版ID。

	用户可以在创建文档时提供文档键，也可以自动分配唯一值。它以后不能改变。以下划线开头的所有三个系统属性_都是只读的。

	我们可以使用文档密钥或文档ID在AQL函数的帮助下检索特定文档DOCUMENT()

更新文档
	UPDATE documentKey WITH object IN collectionName  l列出的属性更新指定的文档（存在则添加它们）
	要更新整个文档 整个用replace

	可以用循环 更新或者替换属性
	FOR c IN Character 
		UPDATE c with {swason: 1} IN Character
	
删除文件
	要从集合中删除文档 执行 REMOVE
	REMOVE "201213" IN Character

	FOR C IN Characters
		REMOVE c IN Characters
		
匹配条件
	为了查找满足条件的文档 FILTER AQL
	FOR c IN Characters 
		FILTER c.age >= 12
		RETURN c.name
	
	多种条件
	FOR c IN Characters
		FILTER c.age 《 13
		FILTER c.age != null
		RETURN {name: c.name, age: c.age} 可以用AND运算符
		同时也有OR运算符  
	
限制结果计数
	FOR c IN Characters_1
	LIMIT	5
	RETURN c.name
	FOR c IN Characters 
	LIMIT 2,5
	RETURN c.name
		
	
	
	




	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	