# WASM Bindings

QCL can be compiled to WebAssembly via `wasm-bindgen`.

## Prerequisites

```bash
cargo install wasm-pack
rustup target add wasm32-unknown-unknown
```

## Build

```bash
make wasm
# or directly:
wasm-pack build --target web --release -- --features wasm
```

Output is generated in the `pkg/` directory.

## API

### `eval_json(expression, json_ctx)`

Evaluate a QCL expression against a JSON context string. Returns the result as a string.

```typescript
import init, { eval_json } from "./pkg/qcl.js";

await init();
const result = eval_json('@user.role == "admin"', '{"user": {"role": "admin"}}');
console.log(result); // "true"
```

### `eval(expression, ctx)`

Evaluate a QCL expression against a JS object. Returns the result as a JS value.

```typescript
import init, { eval as qclEval } from "./pkg/qcl.js";

await init();
const result = qclEval("@x + 1", { x: 2 });
console.log(result); // 3
```

## Build Targets

`wasm-pack` supports different targets:

| Target | Flag | Use case |
|--------|------|----------|
| Web (ESM) | `--target web` | `<script type="module">` |
| Bundler | `--target bundler` | Webpack / Vite |
| Node.js | `--target nodejs` | Server-side Node |

Example for bundler:

```bash
wasm-pack build --target bundler --release -- --features wasm
```
