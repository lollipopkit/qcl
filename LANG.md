## Types

### Basic
- `String`: UTF-8 encoded.
```
"Hello, World!"
'Hello, World!'
// Raw strings (Rust-like), support multi-line and no escapes/interpolation
r"Hello"
r#"He said "hi""#
r##"a "# quote"##
```

- `Int`: 64-bit signed
```
17
-1
```

- `Float`: 64-bit floating point
```
3.14
-1.0
```

- `Bool`: `true` or `false`
- `Nil`: `nil`

### Composite

- `List`: A list of values.
```json
[false, 1, '2']
```

- `Map`: A map of key-value pairs. Key must be a `String`.
```json
{
    "key": "value",
    "key2": 2
}
```

- `Function`: First-class functions with closures.
```
fn add(a, b) { return a + b; }
```

## Operators

### Arithmetic
```
+ - * / %
```

### Comparison
```
== != < > <= >= in
```

### Logical
```
&& || !
```

### Conditional (Ternary)
```
cond ? then : else
```
- Right-associative.
- Lowest precedence among expression operators (evaluated after `||`, `&&`, comparisons, and arithmetic).
- Only the selected branch is evaluated.

### Access
```
@ . ()
```

## Expressions

### Primary
```
1 + 1 % 1 != 2
```

### Parentheses
```
(1 + 1) % 1 != 2
```

### Context Access
```
@req.user.role == 'admin'
```

### Function Calls
```
add(1, 2)
sqrt(@req.user.level)
```

### Complex
```
@req.user.role == 'admin' || @req.user.id in @record.granted
```

## Grammar

### Expression Grammar (precedence hierarchy):
```ebnf
exp     ::= conditional
conditional ::= or [ '?' expr ':' expr ]
or      ::= and { '||' and }
and     ::= cmp { '&&' cmp }
cmp     ::= addsub { ('==' | '!=' | '<' | '>' | '<=' | '>=' | 'in') addsub }
addsub  ::= muldiv { ('+' | '-') muldiv }
muldiv  ::= unary { ('*' | '/' | '%') unary }
unary   ::= { '!' } postfix
postfix ::= primary { '.' field | '(' args ')' }
primary ::= nil | false | true | int | float | string | at | list | map | var | paren
at      ::= '@' field { '.' field }
field   ::= id | int | string | '(' expr ')'
list    ::= '[' [expr { ',' expr}] ']'
map     ::= '{' [expr ':' expr { ',' expr ':' expr}] '}'
var     ::= identifier
paren   ::= '(' expr ')'
args    ::= [expr { ',' expr}]
```

### Statement Grammar:
```ebnf
program  ::= statement*
statement ::= import_stmt | if_stmt | while_stmt | let_stmt | assign_stmt | break_stmt | continue_stmt | return_stmt | fn_stmt | expr_stmt | block_stmt
import_stmt ::= 'import' import_spec ';'
if_stmt  ::= 'if' '(' expr ')' statement ['else' statement]
while_stmt ::= 'while' '(' expr ')' statement
let_stmt ::= 'let' id [':' type] '=' expr ';'
assign_stmt ::= id '=' expr ';'
break_stmt ::= 'break' ';'
continue_stmt ::= 'continue' ';'
return_stmt ::= 'return' [expr] ';'
fn_stmt ::= 'fn' id '(' [id {',' id}] ')' block_stmt
expr_stmt ::= expr ';'
block_stmt ::= '{' statement* '}'

import_spec ::= module | string | items_from_source | namespace_import | module_alias
module ::= identifier
string ::= string_literal
items_from_source ::= '{' id {',' id} '}' 'from' (module | string)
namespace_import ::= '*' 'as' id 'from' (module | string)
module_alias ::= module 'as' id
```

## Features

### Import System
- `import math;` - Import entire stdlib module
- `import "path/to/file.qcl";` - Import from file
- `import { abs, sqrt } from math;` - Import specific functions
- `import * as math from math;` - Import as namespace
- `import math as m;` - Import module with alias

### Standard Library Modules
- `math` - Mathematical functions (abs, sqrt, sin, cos, etc.)
- `string` - String manipulation functions
- `datetime` - Date and time operations
- `os` - Operating system interface

### Control Flow
- `if (condition) { ... } else { ... }` - Conditional execution
- `while (condition) { ... }` - Loop with condition
- `break;` - Exit loop
- `continue;` - Skip to next iteration

### Functions
- `fn name(param1, param2) { ... }` - Function definition
- `return [value];` - Return from function
- First-class functions with closures
- Recursive function calls

### Variables
- `let name [: type] = value;` - Variable declaration with optional type annotation
- `name = value;` - Variable assignment
- Lexical scoping with nested environments

### Context Access
- `@path.to.value` - Access nested context values
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
