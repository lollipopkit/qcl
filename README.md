English | [简体中文](README.zh-CN.md)

<div align="center">
    <h2>LKR</h2>
    <h5>a Rust-like scripting language written in Rust</h5>
</div>

## Intro

### Example

More language details: [LANG.md](LANG.md).

## Features

### Usage

#### Integration (library)

```rust
use lkr_core::{expr::Expr, stmt::Environment, val::Val};

// Parse expr
let expr_src = "data.req.user.name in 'foobar' && data.files.0.published == true";
let expr = Expr::try_from(expr_src)?;

// Provide variables via the lexical environment
let mut env = Environment::new();
let data_val: Val = serde_json::json!({
    "req": { "user": { "name": "foo" } },
    "files": [ { "name": "file1", "published": true } ]
}).into();
env.define("data".to_string(), data_val);

// Eval
let result = expr.eval_with_env(Some(&env))?; // Val::Bool(true)
assert_eq!(result, Val::Bool(true));
```

## License

```plaintext
Apache-2.0 lollipopkit
```
