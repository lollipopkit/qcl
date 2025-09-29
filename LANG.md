## Language Overview

This document describes the QCL language as implemented in this repository (parser, evaluator, statements, types, and standard library wiring).

Comments
- Line comments: `// ...`
- Block comments: `/* ... */`

Identifiers
- Consist of letters, digits, `_`, and `-`. Keywords are reserved. (Be mindful that `-` within identifiers is allowed by the lexer.)

Literals
- String: `"..."` or `'...'` UTF‑8 strings. Supports escapes `\n \r \t \\ \" \' \$ \0`.
- Raw string (Rust‑style, no escapes/interpolation): `r"..."`, `r#"..."#`, `r##"..."##` (multi‑line allowed).
- Int: 64‑bit signed, supports leading sign and scientific notation for floats.
- Float: 64‑bit floating point, supports scientific notation.
- Bool: `true`, `false`
- Nil: `nil`

Collections
- List: `[a, b, c]` (heterogeneous allowed). Indexing: `list[0]`. Safe access helpers via stdlib/meta‑methods.
- Map: `{ key: value, ... }`. Keys are evaluated expressions and coerced to strings at runtime (string/int/float/bool); access with `map.key` or `map["key"]`.

Template Strings
- Interpolation only with `${expr}` inside normal quotes (both `"..."` and `'...'`).
- Raw strings do not support interpolation.
- Examples: `"Hello, ${@user.name}!"`, `"Sum: ${1 + 2}"`.

Context Access `@`
- `@` reads from the input context (e.g., JSON piped to CLI). Paths are dot‑separated.
- First segment may be identifier, string, or parenthesized expression; subsequent segments may be id/str/int.
- Examples: `@req.user.id`, `@users.0.name`, `@user.(@record.index - 1)`.

Function Calls and Methods
- Call any expression: `f(x, y)`, `(g)(z)`.
- Property access: `expr.field` or `expr[expr]`. Optional chaining: `expr?.field` and `expr?[index]`.
- Method sugar: `value.method(args...)` dispatches as:
  1) If `value.method` yields a callable (closure/native), call it.
  2) Else dispatch a registered meta‑method for the value’s runtime type, passing the receiver as the first argument (e.g., `"abc".len()`; see stdlib).

Closures
- Expression form only: `|a, b| a + b`.

Ranges
- `a..b` and `a..=b` produce integer lists when evaluated (inclusive/exclusive end). Used in patterns as well.

Nullish Coalescing and Ternary
- `lhs ?? rhs` yields `lhs` unless it is `nil`, then `rhs`.
- `cond ? then : else` (right‑associative). In expressions, `cond` must be Bool. In `if`/`while`, truthiness is used (see below).

## Operators (by precedence)
- Postfix: call `()`, dot `.field`, index `[expr]`, optional `?.field`, optional `?[expr]`
- Unary: `!` (logical not)
- Multiplicative: `* / %`
- Additive: `+ -`
- Range: `.. ..=`
- Comparison/membership: `== != < > <= >= in`
- Logical: `&& ||`
- Nullish coalescing: `??`
- Ternary: `? :` (lowest among expression operators)

Notes
- `+` supports String + String concatenation. Other string/number mixes are feature‑gated and not enabled by default.
- `in` supports: substring `str in str`, element membership in lists, and key existence in maps. For `list in list`, it checks all elements of the left are contained in the right.

## Expressions
- Literals, lists, maps, variables, context `@...`, calls, property/index access, closures, ranges, logical/comparison, `??`, and `?:`.
- Concurrency expressions (feature‑gated `concurrency`):
  - `spawn(expr)` → Task
  - `chan(capacity?, type?)` → Channel (type is a string like `"Int"`)
  - `send(channel, value)` → Bool
  - `recv(channel)` → `[ok, value]`
  - `select { case recv(c) => expr; case send(c, v) => expr; default => expr }`

Match Expression
- `match value { pattern => expr, ... }` (`,` or `;` separators allowed). Returns the chosen arm’s value. Patterns below.

## Patterns
Used in `match`, `if let`, `while let`, and `let` destructuring.
- Literal: `1`, `3.14`, `"x"`, `true`, `nil`
- Variable binding: `name`
- Wildcard: `_`
- List destructuring: `[p1, p2, ..rest]`
- Map destructuring: `{ "key": pat, other: pat, ..rest }` (keys may be string literals or identifiers; rest binds remaining fields)
- Or‑pattern: `p1 | p2 | p3`
- Guarded pattern: `pat if expr`
- Range pattern: `1..10`, `0..=n`

For‑loop Patterns
- Support an extended pattern set:
  - Variable: `x`
  - Ignore: `_`
  - Tuple: `(a, b, c)`
  - Array: `[a, b, ..rest]`
  - Object: `{ "k": v, ... }` (string keys)

## Statements
- Program is a sequence of statements. Semicolons `;` terminate simple statements and expression statements.

Control Flow
- `if (cond) stmt` or `if cond stmt` (parentheses optional). Truthiness: `false` and `nil` are false; everything else is true.
- `if let pattern = expr stmt [else stmt]`
- `while (cond) stmt` or `while cond stmt`
- `while let pattern = expr stmt`
- `for pattern in expr stmt` where `expr` is iterable: List, String (chars), or Map (iterates `[key, value]`).
- `break;`, `continue;`
- `return;` or `return expr;`

Variables
- Declaration/destructuring: `let pattern [: Type] = expr;`
- Assignment: `name = expr;`
- Compound assignment: `name += expr;`, `-=`, `*=`, `/=`, `%=`
- Short definition: `name := expr;` (define and initialize)
- Lexical scoping: blocks `{ ... }` introduce a new scope.

Functions
- Definition: `fn name(param1[: Type], param2[: Type]) [-> Type] { statements }`
- Parameters and return type are optional; functions return `nil` by default unless `return` is used.
- First‑class: closures and function values can be passed, returned, and called.

Imports
- Forms:
  - `import math;` — stdlib module as a namespace
  - `import "path/to/file.qcl";` — file module as a namespace (name is the file stem)
  - `import { abs, sqrt } from math;` — selected items
  - `import { f as g } from "m.qcl";` — with alias
  - `import * as m from math;` — namespace alias
  - `import math as m;` — module alias

- File import resolution and safety:
  - Paths are relative-only and sanitized: absolute paths and any `..` components are rejected.
  - Resolution attempts, in order: `${MOD_NAME}.qcl`, then `${MOD_NAME}/mod.qcl` (relative to the current directory).
  - If you pass a quoted path with `.qcl` already (e.g., `"lib/foo.qcl"`), it must be relative and will be used directly if it exists.

Builtins and Stdlib
- Builtin globals: `print(fmt, ...args)`, `println(fmt, ...args)`, `panic([msg])`.
- Stdlib modules (import as needed): `math`, `string`, `list`, `map`, `iter`, `datetime`, `os`, `tcp`. With `concurrency` feature: `task`, `chan`, `time`.
- `iter` module highlights: `enumerate(list)`, `range([start,] end [, step])`, `zip(list1, list2)`,
  `take(list, n)`, `skip(list, n)`, `chain(list1, list2)`, `flatten(list)`, `unique(list)`, `chunk(list, size)`.
- Meta‑methods (usable as `value.method()` without importing): String: `len, lower, upper, trim, starts_with, ends_with, contains, replace, substring, split, join`; List: `len, push, concat, join, get, first, last, map, filter, reduce`; Map: `len, keys, values, has, get`.

## Types and Annotations
Primitive and composite types
- `Int`, `Float`, `String`, `Bool`, `Nil`, `Any`
- `List<T>`, `Map<K, V>`
- `Task<T>`, `Channel<T>` (concurrency)
- Function types: `(T1, T2) -> R`
- Union: `A | B | Nil`; Optional: `?T` (sugar for `T | Nil`)
- Named and generic types are parsed (e.g., `List<Int>`, `Map<String, Int>`)

Annotations
- `let x: Int = 1;`
- `fn f(a: Int, b: String) -> Bool { ... }`
- Type checking/inference is best‑effort and conservative; runtime remains dynamic.

## Grammar (EBNF‑style)

Expressions (precedence from low to high)
```
expr        ::= conditional
conditional ::= nullish [ '?' expr ':' expr ]
nullish    ::= or { '??' or }
or          ::= and { '||' and }
and         ::= cmp { '&&' cmp }
cmp         ::= range { ('==' | '!=' | '<' | '>' | '<=' | '>=' | 'in') range }
range       ::= addsub [ ('..' | '..=') addsub? ]
addsub      ::= muldiv { ('+' | '-') muldiv }
muldiv      ::= unary { ('*' | '/' | '%') unary }
unary       ::= { '!' } postfix
postfix     ::= primary { call | dot | opt_dot | opt_index | index }
call        ::= '(' args ')'
dot         ::= '.' field
opt_dot     ::= '?.' field
index       ::= '[' expr ']'
opt_index   ::= '?[' expr ']'
primary     ::= nil | false | true | int | float | string | template | at | list | map | var | paren
             | closure | spawn | chan | send | recv | select | match
closure     ::= '|' [id {',' id}] '|' expr
template    ::= string_with_${...}
at          ::= '@' at_field { '.' at_field }
at_field    ::= id | int | string | '(' expr ')'
field       ::= id | int | string
list        ::= '[' [ expr { ',' expr } [ ',' ] ] ']'
map         ::= '{' [ expr ':' expr { ',' expr ':' expr } [ ',' ] ] '}'
var         ::= identifier
paren       ::= '(' expr ')'
args        ::= [ expr { ',' expr } ]
```

Statements
```
program      ::= statement*
statement    ::= import_stmt | if_stmt | if_let_stmt | while_stmt | while_let_stmt
               | for_stmt | let_stmt | define_stmt | assign_stmt | compound_assign_stmt
               | return_stmt | break_stmt | continue_stmt | fn_stmt | expr_stmt | block_stmt

import_stmt  ::= 'import' ( module | string | items_from_source | namespace_import | module_alias ) ';'
module       ::= identifier
string       ::= string_literal
items_from_source ::= '{' import_item { ',' import_item } '}' 'from' ( module | string )
import_item  ::= id [ 'as' id ]
namespace_import ::= '*' 'as' id 'from' ( module | string )
module_alias ::= module 'as' id

if_stmt      ::= 'if' ( '(' expr ')' | expr ) statement [ 'else' statement ]
if_let_stmt  ::= 'if' 'let' pattern '=' expr statement [ 'else' statement ]
while_stmt   ::= 'while' ( '(' expr ')' | expr ) statement
while_let_stmt ::= 'while' 'let' pattern '=' expr statement
for_stmt     ::= 'for' for_pattern 'in' expr statement

let_stmt     ::= 'let' pattern [ ':' type ] '=' expr ';'
define_stmt  ::= id ':' '=' expr ';'
assign_stmt  ::= id '=' expr ';'
compound_assign_stmt ::= id ( '+=' | '-=' | '*=' | '/=' | '%=' ) expr ';'
return_stmt  ::= 'return' [ expr ] ';'
break_stmt   ::= 'break' ';'
continue_stmt ::= 'continue' ';'
fn_stmt      ::= 'fn' id '(' [ param { ',' param } ] ')' [ '->' type ] block_stmt
param        ::= id [ ':' type ]
expr_stmt    ::= expr ';'
block_stmt   ::= '{' statement* '}'
```

Patterns
```
pattern      ::= literal | '_' | id | list_pat | map_pat | or_pat | guard_pat | range_pat
list_pat     ::= '[' pattern { ',' pattern } [ ',' '..' id ] ']'
map_pat      ::= '{' ( (string|id) ':' pattern ) { ',' (string|id) ':' pattern } [ ',' '..' id ] '}'
or_pat       ::= pattern '|' pattern { '|' pattern }
guard_pat    ::= pattern 'if' expr
range_pat    ::= literal ('..' | '..=') expr

for_pattern  ::= '_' | id | '(' for_pattern { ',' for_pattern } ')' | '[' for_pattern { ',' for_pattern } [ ',' '..' id ] ']'
               | '{' string ':' for_pattern { ',' string ':' for_pattern } '}'
```

## Notes for CLI usage
- Expression mode (`--expr`) evaluates a single expression without setting up the statement environment or stdlib modules. Program mode (default, or by passing a file) parses statements, initializes stdlib, and supports imports/functions.
- Context is read from stdin (JSON/YAML/TOML). Access via `@` as shown above.

- `@user.role == 'admin'` - Example ACL evaluation
- Dynamic field access with expressions: `@(expr).field`

### Types
- `String` - UTF-8 strings
- `Int` - 64-bit signed integers
- `Float` - 64-bit floating point
- `Bool` - Boolean values
- `Nil` - Null/undefined value
- `List` - Ordered collections
- `Map` - Key-value maps
- `Function` - First-class functions
