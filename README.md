<div align="center">
    <h2>QCL</h2>
    <h5>a simple language that allows you to check the eval result of a query.</h5>
</div>

English version. Chinese version: [README.zh.md](README.zh.md).

## Intro

It's designed to be used in ACL (Access Control List) systems, where you need to check if a user has access to a resource.

### Example

```js
(@record.published || @record.owner == @req.user.id) // Normal case
|| // Or operator
(@req.user.role == 'admin' || @req.user.id in @record.granted) // Special case
```

Let's break it down:

- `@req.user.role == 'admin'`: Check if the user has the role of `admin`.
- `@req.user.id in @record.granted`: Check if the user's id is in the `granted` list of the record.
- `@record.published`: Check if the record is published.
- `@record.owner == @req.user.id`: Check if the record's owner is the user.

The above example is a simple ACL system that checks if the user has access to a record.

More language details can be found in [LANG.md](LANG.md).

## Features

Default:
- `json` — JSON input format
- `sem_arith` — semantic integer division (`3 / 2 = 1.5`)
- `std` — expression cache and deserialization

Optional:
- `yaml` — YAML input format
- `toml` — TOML input format
- `adv_arith` — advanced arithmetic (`List + List`, `Map + Map`, etc.)

Bindings:
- `wasm` — WebAssembly via wasm-bindgen ([docs](docs/wasm.md))
- `ffi` — C shared library ([docs](docs/ffi.md))
- `python` — Python module via PyO3 ([docs](docs/python.md))

The library also supports `no_std` (with `alloc`) when `std` is disabled.

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
    <img src="https://cdn.lpkt.cn/img/capture/qcl.png" alt="QCL" />
</div>

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'

# Check mode (exit code 0/1)
echo '{"x": 1}' | cargo run -- --check '@x == 1' && echo ok

# Dump AST
echo '{}' | cargo run -- --ast '1 + 2'
```

#### Build Bindings

```bash
make wasm    # WASM package  → pkg/
make ffi     # C shared lib  → target/release/libqcl.so
make python  # Python module → installed into active venv
```

See [docs/](docs/) for detailed binding documentation.

## Ports

- [Golang SDK / CLI](https://github.com/lollipopkit/gqcl)

## License

```plaintext
Apache-2.0 lollipopkit
```
