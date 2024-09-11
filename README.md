<div align="center">
    <h2>QCL</h2>
    <h5>a simple language that allows you to check the eval result of a query. </h5>
</div>

## Intro

It's designed to be used in ACL (Access Control List) systems, where you need to check if a user has access to a resource.

### Example

```qcl
(
    @req.user.role == 'admin' 
    ||
    @req.user.id in @record.granted
)
&&
(
    @record.published
    ||
    @record.owner == @req.user.id
)
```

Let's break it down:
- `@req.user.role == 'admin'`: Check if the user has the role of `admin`.
- `@req.user.id in @record.granted`: Check if the user's id is in the `granted` list of the record.
- `@record.published`: Check if the record is published.
- `@record.owner == @req.user.id`: Check if the record's owner is the user.

The above example is a simple ACL system that checks if the user has access to a record.

More language details can be found in [LANG.md](LANG.md).

### Usage

#### Integration

```rust
// Parse expr
let expr = "@req.user.name in 'foobar' && @files.0.published == true";
let expr = Expr::try_from(expr)?;

// Construct context
let ctx_names = expr.requested_ctx(); // ["req", "files"]
// You can construct the context indeed, but we use json! for simplicity
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

// Eval
let result = expr.eval(ctx.into())?; // Val::Bool(true)
match result {
    Val::Bool(b) => {
        assert!(b);
    }
    _ => {
        panic!("unexpected result");
    }
}
```

#### CLI

<div height="100px" align="center">
    <img src="https://cdn.lpkt.cn/img/capture/qcl.jpg" alt="QCL" />
</div>


## License
```
Apache-2.0
2024 @lollipopkit
```
