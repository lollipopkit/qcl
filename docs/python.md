# Python Bindings

QCL provides Python bindings via [PyO3](https://pyo3.rs) and [maturin](https://www.maturin.rs).

## Prerequisites

```bash
pip install maturin
```

## Build & Install

```bash
# Into the active virtualenv (development):
make python
# or directly:
maturin develop --features python --release

# Build a distributable wheel:
maturin build --features python --release
# wheel is written to target/wheels/
```

## API

### `qcl.eval(expression, context)`

Evaluate a QCL expression against a Python dict. Returns a native Python value.

```python
import qcl

result = qcl.eval("@x + 1", {"x": 2})
assert result == 3

result = qcl.eval("@req.user.role", {"req": {"user": {"role": "admin"}}})
assert result == "admin"
```

### `qcl.eval_json(expression, json_ctx)`

Same as `eval`, but accepts a JSON string as context.

```python
import qcl

result = qcl.eval_json("@name", '{"name": "alice"}')
assert result == "alice"
```

### `qcl.check(expression, context)`

Return `True` if the expression result is truthy, `False` otherwise.

```python
import qcl

assert qcl.check("@role == 'admin'", {"role": "admin"}) is True
assert qcl.check("@role == 'admin'", {"role": "user"}) is False
```

## Type Mapping

| Python | QCL Val |
|--------|---------|
| `None` | `Nil` |
| `bool` | `Bool` |
| `int` | `Int` |
| `float` | `Float` |
| `str` | `Str` |
| `list` | `List` |
| `dict` (string keys) | `Map` |

## Error Handling

All functions raise `ValueError` on parse or evaluation errors:

```python
import qcl

try:
    qcl.eval("@x +", {"x": 1})
except ValueError as e:
    print(f"Error: {e}")
```
