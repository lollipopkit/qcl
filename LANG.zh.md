# QCL Language

QCL 是一个小型表达式语言，用于在结构化上下文上执行 ACL 风格的检查。

中文版。英文版见 [LANG.md](LANG.md)。

## 词法规则

- 空白字符会被忽略。
- `//` 表示单行注释的开始。
- 字符串可以使用 `"` 或 `'` 包裹。
- 支持的转义包括 `\\`, `\"`, `\'`, `\n`, `\r`, `\t`, `\0`。
- 数字字面量可以带前导 `+` 或 `-`。
- 整数类型为 `i64`；浮点类型为 `f64`。
- 不存在独立的一元负号运算符。
- 在 `@` 之外，裸标识符会被当作字符串值，而不是变量。
- 在 `@` 路径内部，相同的标识符 token 会被当作字段名。
- 精确关键字为 `true`、`false`、`nil` 和 `in`。

示例：

```text
foo == "foo"
123
-1
"hello\nworld"
// comment
```

## 值类型

QCL 中的值类型包括：

- `String`
- `Int`
- `Float`
- `Bool`
- `Nil`
- `List`
- `Map`

`Nil` 是显式的、类似 null 的值。

## 字面量

### 基本

```text
"Hello"
'Hello'
17
-1
3.14
true
false
nil
```

### 列表

```text
[1, 2, "three", true]
[]
[1, 2, 3,]
```

### 映射

`Map` 的 key 会像普通表达式一样先求值；如果结果是原始值（primitive value），则会再转换成字符串。
允许作为 key 结果类型的是 `String`、`Int`、`Float` 和 `Bool`。

```text
{
  "name": "Alice",
  42: "answer",
  true: "yes",
  name: "literal key"
}
```

允许嵌套的 `List` 和 `Map`，并且 list/map 的元素中也可以包含完整表达式。

```text
[1 + 2, @user.age, {"name": @user.name}]
{"sum": 2 + 3, "user": {"id": @req.user.id}}
```

## 访问

### 上下文访问

`@` 用于开始一次上下文查找。

```text
@req.user.role
@record.granted
@users.0.name
```

路径片段可以是：

- 标识符
- 带引号的字符串
- 整数索引
- 带括号的表达式
- 另一个 `@` 访问

第一个路径片段用于选择顶层上下文 key。List 索引从 0 开始。
越界访问和缺失的 map key 会返回 `nil`。

示例：

```text
@req."user-data"."is-active"
@data.'special-field'
@users.(@index)
@req.user."name"
```

### 后缀访问

主表达式后面可以继续跟 `.field` 或 `.index`。如果你想访问复合表达式的结果，请使用括号。

```text
[1, 2, 3].1
{"name": "Alice"}.name
({"users": [1, 2]}).users.1
```

## 运算符

运算符优先级从高到低如下：

1. 后缀访问: `.`
1. 一元非: `!`
1. 乘除取模: `* / %`
1. 加减: `+ -`
1. 比较: `== != < > <= >= in`
1. 逻辑与: `&&`
1. 逻辑或: `||`

说明：

- 二元运算符都是左结合。
- `&&` 和 `||` 采用短路求值。
- `!` 只接受 `Bool`。
- `in` 的含义是：
  - 当两边都是字符串时，表示子串判断
  - 当右侧是 list、左侧是单个值时，表示成员判断
  - 当两边都是 list 时，表示子集判断
  - 不支持 map 成员判断

示例：

```text
@req.user.role == "admin"
@req.user.id in @record.granted
@user.age > 18 && @user.active
!@user.disabled
```

## 特殊语义

- 访问失败会返回 `nil`。
- `@nonexistent == nil` 的结果是 `true`。
- `@nonexistent != nil` 的结果是 `false`。
- `@nonexistent != 1` 的结果是 `true`。

## 特性开关

默认启用的 feature：

- `json`
- `sem_arith`

可选的 feature：

- `yaml`
- `toml`
- `adv_arith`

`sem_arith` 会改变整数除法的行为：结果能整除时保持整数，不能整除时返回浮点数。

`adv_arith` 会启用额外的算术能力：

- `String + Int/Float`
- `List + List`
- `List + Value` 追加一个元素
- `List - Value` 删除第一个匹配元素
- `List - List` 删除右侧 list 中出现过的值
- `Map + Map`（右侧同名 key 会覆盖左侧）
- `Map - Map` 删除右侧 map 中出现的 key
- `Map - String` 删除一个 key

## 输入格式

context 会根据已启用的特性，从 JSON、YAML 或 TOML 中解析。

在默认构建中，JSON 是默认解析器。

CLI 会从 `stdin` 读取 context，并从 argv 读取表达式。

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'
echo 'name: test' | cargo run --features yaml -- --yaml '@name == "test"'
echo 'name = "test"' | cargo run --features toml -- --toml '@name == "test"'
```

## 示例

```text
@record.published || @record.owner == @req.user.id
@req.user.role == "admin" || @req.user.id in @record.granted
@config."debug-mode" && {"name": @user.name}.name == "lk"
```

## 语法

非正式语法如下：

```ebnf
exp      ::= or
or       ::= and { "||" and }
and      ::= cmp { "&&" cmp }
cmp      ::= add { ("==" | "!=" | "<" | ">" | "<=" | ">=" | "in") add }
add      ::= mul { ("+" | "-") mul }
mul      ::= unary { ("*" | "/" | "%") unary }
unary    ::= "!" unary | postfix
postfix  ::= primary { "." segment }
primary  ::= nil | bool | number | string | id | list | map | at | "(" exp ")"
list     ::= "[" [ exp { "," exp } [ "," ] ] "]"
map      ::= "{" [ pair { "," pair } [ "," ] ] "}"
pair     ::= exp ":" exp
at       ::= "@" segment { "." segment }
segment  ::= id | string | int | "(" exp ")" | at
```

说明：

- 在 `@` 之外，裸 `id` token 会按字符串求值。
- `string` 表示带引号的字符串 token。
- `Map` key 最终必须解析为原始值（primitive value）。
