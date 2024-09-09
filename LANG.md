## Types

### Basic
- `String`: UTF-8 encoded.
```
"Hello, World!"
'Hello, World!'
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

## Operators

### Arithmetic
```
+ - * / %
```

### Comparison
```
== != < > <= >=
```

### Logical
```
&& || ! in ()
```

### Access
```
@
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

### Access
```
@req.user.role == 'admin'
```

### Complex
```
@req.user.role == 'admin' || @req.user.id in @record.granted
```

## Grammar
```ebnf
exp     ::= or
or      ::= and {’||’ and}
and     ::= cmp {’&&’ cmp}
cmp     ::= addsub {(‘<’ | ‘>’ | ‘<=’ | ‘>=’ | ‘!=’ | ‘==’) addsub}
addsub  ::= muldiv {(‘+’ | ‘-’) muldiv}
muldiv  ::= unary {(‘*’ | ‘/’ | ‘%’) unary}
unary   ::= {‘!’} primary
primary ::= nil | false | true | int | float | string | at
paren   ::= {‘(’} exp {‘)’}
at      ::= ‘@’ field {‘.’ field}
field   ::= id | int
```
