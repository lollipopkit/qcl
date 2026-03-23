# QCL Language

QCL is a small expression language for ACL-style checks over structured contexts.

English version. Chinese version: [LANG.zh.md](LANG.zh.md).

## Lexical Rules

- Whitespace is ignored.
- `//` starts a single-line comment.
- Strings can be wrapped in either `"` or `'`.
- Supported escapes are `\\`, `\"`, `\'`, `\n`, `\r`, `\t`, and `\0`.
- Numeric literals may have a leading `+` or `-`.
- Integers are `i64`; floats are `f64`.
- There is no standalone unary minus operator.
- Outside `@`, bare identifiers are treated as string values, not variables.
- Inside `@` paths, the same identifier tokens are treated as field names.
- Exact keywords are `true`, `false`, `nil`, and `in`.

Examples:

```text
foo == "foo"
123
-1
"hello\nworld"
// comment
```

## Values

QCL values include:

- `String`
- `Int`
- `Float`
- `Bool`
- `Nil`
- `List`
- `Map`

`Nil` is an explicit null-like value.

## Literals

### Basic

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

### Lists

```text
[1, 2, "three", true]
[]
[1, 2, 3,]
```

### Maps

`Map` keys are evaluated like normal expressions first; if the result is a primitive value, it is converted to a string.
Allowed key result types are `String`, `Int`, `Float`, and `Bool`.

```text
{
  "name": "Alice",
  42: "answer",
  true: "yes",
  name: "literal key"
}
```

Nested `List` and `Map` values are allowed, and list/map elements can contain full expressions.

```text
[1 + 2, @user.age, {"name": @user.name}]
{"sum": 2 + 3, "user": {"id": @req.user.id}}
```

## Access

### Context access

`@` starts a context lookup.

```text
@req.user.role
@record.granted
@users.0.name
```

Path segments may be:

- an identifier
- a quoted string
- an integer index
- a parenthesized expression
- another `@` access

The first path segment selects a top-level context key. List indices are zero-based.
Out-of-bounds access and missing map keys return `nil`.

Examples:

```text
@req."user-data"."is-active"
@data.'special-field'
@users.(@index)
@req.user."name"
```

### Postfix access

Primary expressions can be followed by `.field` or `.index`. Use parentheses when you want to access the result of a compound expression.

```text
[1, 2, 3].1
{"name": "Alice"}.name
({"users": [1, 2]}).users.1
```

## Operators

Operator precedence, from high to low:

1. Postfix access: `.`
1. Unary not: `!`
1. Multiplicative: `* / %`
1. Additive: `+ -`
1. Comparison: `== != < > <= >= in`
1. Logical AND: `&&`
1. Logical OR: `||`

Notes:

- Binary operators are left-associative.
- `&&` and `||` short-circuit.
- `!` only accepts `Bool`.
- `in` means:
  - substring when both sides are strings
  - membership when the right side is a list and the left side is a single value
  - subset when both sides are lists
  - map membership is not supported

Examples:

```text
@req.user.role == "admin"
@req.user.id in @record.granted
@user.age > 18 && @user.active
!@user.disabled
```

## Special Semantics

- Failed access returns `nil`.
- `@nonexistent == nil` is `true`.
- `@nonexistent != nil` is `false`.
- `@nonexistent != 1` is `true`.

## Feature Flags

Default features:

- `json`
- `sem_arith`

Optional features:

- `yaml`
- `toml`
- `adv_arith`

`sem_arith` changes integer division so exact results stay integers and inexact results become floats.

`adv_arith` enables extra arithmetic:

- `String + Int/Float`
- `List + List`
- `List + Value` appends one element
- `List - Value` removes the first matching element
- `List - List` removes values present in the right list
- `Map + Map` (right-hand keys win)
- `Map - Map` removes keys present in the right map
- `Map - String` removes one key

## Input Formats

Contexts are parsed from JSON, YAML, or TOML depending on enabled features.

In the default build, JSON is the default parser.

The CLI reads the context from `stdin` and the expression from argv.

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'
echo 'name: test' | cargo run --features yaml -- --yaml '@name == "test"'
echo 'name = "test"' | cargo run --features toml -- --toml '@name == "test"'
```

## Examples

```text
@record.published || @record.owner == @req.user.id
@req.user.role == "admin" || @req.user.id in @record.granted
@config."debug-mode" && {"name": @user.name}.name == "lk"
```

## Grammar

Informal grammar:

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

Notes:

- Outside `@`, bare `id` tokens evaluate as strings.
- `string` means a quoted string token.
- `Map` keys must finally resolve to a primitive value.
