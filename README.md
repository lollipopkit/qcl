<div align="center">
    <h2>QCL</h2>
    <h5>a Query Check Language</h5>
</div>

## Intro

It's a simple language that allows you to check the eval result of a query.  
It's designed to be used in ACL (Access Control List) systems, where you need to check if a user has access to a resource.

### Example

```qcl
@req.user.name == 'bar' && @record.files.0.public == true
```

Explanation:

- both `req` and `record` are objects that are passed to the QCL engine as [Context].
- `@req.user.name` is a query that will be evaluated to the user's name.
- `@record.published` is a query that will be evaluated to the record's published status.

Syntax in this example:

- `@` is used to query the context object.
- `.` is used to access the object's properties.
- `==` is used to check the equality of two values.
- `&&` is used to check the logical AND of two values.

### Usage

#### Integration

```rust
let ctx = json!({
    "req": {
        "user": "foo"
    },
    "files": [
        {
            "name": "file1",
            "published": true
        }
    ]
});

let query = "@req.user.name in 'foobar' && @record.published == true";

let result = Expr::try_from(query)?.eval(ctx)?;
assert!(result);
```

#### CLI

<div height="100px" align="center">
    <img src="https://cdn.lpkt.cn/img/capture/qcl.jpg" alt="QCL" />
</div>

### Grammar
```ebnf
exp     ::= or
or      ::= and {’||’ and}
and     ::= cmp {’&&’ cmp}
cmp     ::= addsub {(‘<’ | ‘>’ | ‘<=’ | ‘>=’ | ‘!=’ | ‘==’) addsub}
addsub  ::= muldiv {(‘+’ | ‘-’) muldiv}
muldiv  ::= unary {(‘*’ | ‘/’ | ‘%’) unary}
unary   ::= {‘!’} primary
primary ::= nil | false | true | int | float | string | at
paren   ::= {‘(’} or {‘)’}
at      ::= ‘@’ field {‘.’ field}
field   ::= id | int
```
