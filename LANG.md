# QCL Language

QCL is a small expression language for ACL-style checks over structured contexts.

English version. Chinese version: [LANG.zh.md](LANG.zh.md).

## Lexical Rules

- Whitespace is ignored.
- `//` starts a single-line comment.
- `/* */` starts a block comment (can be nested).
- Strings can be wrapped in either `"` or `'`.
- Supported escapes are `\\`, `\"`, `\'`, `\n`, `\r`, `\t`, `\0`, and `\uXXXX` (Unicode code point).
- Numeric literals may have a leading `+` or `-`.
- Integer literals support decimal, hexadecimal (`0x` / `0X`), and octal (`0o` / `0O`) bases.
- Integers are `i64`; floats are `f64`.
- Outside `@`, bare identifiers are treated as string values, not variables.
- Inside `@` paths, the same identifier tokens are treated as field names.
- Exact keywords are `true`, `false`, `nil`, and `in`.

Examples:

```text
foo == "foo"
123
0xFF
0o77
-1
"hello\nworld"
"\u0041lice"  // "Alice"
// single-line comment
/* block comment */
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
Negative integer indices count from the end: `-1` is the last element, `-2` the second-to-last, etc.
Out-of-bounds access and missing map keys return `nil`.

Examples:

```text
@req."user-data"."is-active"
@data.'special-field'
@users.(@index)
@req.user."name"
@list.-1          // last element
@list.-2          // second-to-last
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
1. Unary: `! -`
1. Multiplicative: `* / %`
1. Additive: `+ -`
1. Comparison: `== != < > <= >= in`
1. Logical AND: `&&`
1. Logical OR: `||`
1. Nullish coalesce: `??`
1. Ternary: `? :`

Notes:

- Binary operators are left-associative.
- `&&` and `||` short-circuit.
- `??` returns the left operand if it is not `nil`, otherwise the right operand.
- `? :` requires a `Bool` condition; `cond ? true_expr : false_expr`.
- `!` only accepts `Bool`.
- `-` (unary) negates `Int` and `Float`.
- `in` means:
  - substring when both sides are strings
  - membership when the right side is a list and the left side is a single value
  - subset when both sides are lists
  - key membership when the right side is a map and the left side is a string

Examples:

```text
@req.user.role == "admin"
@req.user.id in @record.granted
@user.age > 18 && @user.active
!@user.disabled
-@price
"name" in {"name": "Alice"}
@nickname ?? "anonymous"
@active ? "yes" : "no"
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
- `std` (enabled by default; provides expression cache and deserialization)
- `wasm` (WebAssembly bindings via `wasm-bindgen`)
- `ffi` (C-compatible FFI)
- `python` (Python bindings via PyO3)

The library supports `no_std` (with `alloc`) when the `std` feature is disabled.

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

CLI flags:

- `--check` / `-c`: exit with code 0 for `true`, 1 for `false`
- `--ast`: print the parsed AST instead of evaluating
- `--version` / `-V`: print version
- `--help` / `-h`: print usage
- `--json` / `--yaml` / `--toml`: force input format

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'
echo 'name: test' | cargo run --features yaml -- --yaml '@name == "test"'
echo 'name = "test"' | cargo run --features toml -- --toml '@name == "test"'
echo '{"x": 1}' | cargo run -- --check '@x == 1' && echo ok
echo '{"x": 1}' | cargo run -- --ast '@x + 1'
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
exp      ::= ternary
ternary  ::= coalesce [ "?" ternary ":" ternary ]
coalesce ::= or { "??" or }
or       ::= and { "||" and }
and      ::= cmp { "&&" cmp }
cmp      ::= add { ("==" | "!=" | "<" | ">" | "<=" | ">=" | "in") add }
add      ::= mul { ("+" | "-") mul }
mul      ::= unary { ("*" | "/" | "%") unary }
unary    ::= "!" unary | "-" unary | postfix
postfix  ::= primary { "." segment }
primary  ::= nil | bool | number | string | id | list | map | at | "(" exp ")"
list     ::= "[" [ exp { "," exp } [ "," ] ] "]"
map      ::= "{" [ pair { "," pair } [ "," ] ] "}"
pair     ::= exp ":" exp
at       ::= "@" segment { "." segment }
segment  ::= id | string | int | "(" exp ")" | at
number   ::= [ "+" | "-" ] ( digit+ [ "." digit+ ] | "0x" hex+ | "0o" oct+ )
```

Notes:

- Outside `@`, bare `id` tokens evaluate as strings.
- `string` means a quoted string token.
- `Map` keys must finally resolve to a primitive value.
- Ternary is right-associative; coalesce (`??`) is left-associative.
