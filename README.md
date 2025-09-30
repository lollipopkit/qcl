English | [简体中文](README.zh-CN.md)

<div align="center">
    <h2>QCL</h2>
    <h5>a simple language that allows you to check the eval result of a query. </h5>
</div>

## Intro

It's designed to be used in ACL (Access Control List) systems, where you need to check if a user has access to a resource.

### Example (statements)

```rust
import io;
import json;
let data = json.parse(io.read());

fn user_can_access(record, req) {
    return record.owner == req.user.id || record.published;
}
fn admin_can_access(req, record) {
    return req.user.role == 'admin' || req.user.id in record.granted;
}
return user_can_access(data.record, data.req) || admin_can_access(data.req, data.record);
```

Let's break it down:

- `req.user.role == 'admin'`: Check if the user has the role of `admin`.
- `req.user.id in record.granted`: Check if the user's id is in the `granted` list of the record.
- `record.published`: Check if the record is published.
- `record.owner == req.user.id`: Check if the record's owner is the user.

The above example is a simple ACL system that checks if the user has access to a record.

More language details: [LANG.md](LANG.md) · [LANG_zh.md](LANG_zh.md).

## Features

- `json` (enabled by default)
- `yaml`
- `toml`

At least one input format feature must be enabled. The default configuration enables `json`.

### Usage

#### Integration (library)

```rust
use qcl_core::{expr::Expr, stmt::Environment, val::Val};

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

#### CLI

<div height="100px" align="center">
    <img src="https://cdn.lpkt.cn/img/capture/qcl.png" alt="QCL" />
</div>

- Input handling:
  - There is no implicit context. Read from stdin explicitly via `io.read()` and parse manually using stdlib modules (`json/yaml/toml`).
  - Example: `cat test.json | qcl --stmt -- "import io; import json; let d = json.parse(io.read()); return d.user.id == 1;"`
- File import safety and resolution:
  - Only relative, sanitized paths are allowed (no `..`, no absolute paths).
  - When importing files, resolution tries `${MOD_NAME}.qcl` then `${MOD_NAME}/mod.qcl` relative to the current directory. Quoted paths with `.qcl` are used directly if they exist.
 - Output behavior: the CLI and REPL print results only when the value is not `nil`. Use `println(...)` to explicitly print `nil`.

## License

```plaintext
Apache-2.0 lollipopkit
```
