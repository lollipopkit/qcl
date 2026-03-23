# C FFI

QCL provides a C-compatible shared library.

## Build

```bash
make ffi
# or directly:
cargo build --lib --features ffi --release
```

Output:
- Linux: `target/release/libqcl.so`
- macOS: `target/release/libqcl.dylib`

## API

### `qcl_eval_json`

```c
char* qcl_eval_json(const char* expression, const char* json_ctx);
```

Parse and evaluate a QCL expression against a JSON context. Returns a heap-allocated result string, or `NULL` on error. The caller must free the returned string with `qcl_free`.

### `qcl_check_json`

```c
int qcl_check_json(const char* expression, const char* json_ctx);
```

Evaluate and return a truthiness code:
- `1` — truthy (`true`, non-nil value)
- `0` — falsy (`false`, `nil`)
- `-1` — error (call `qcl_last_error` for details)

### `qcl_last_error`

```c
char* qcl_last_error(void);
```

Retrieve the last error message (thread-local). Returns `NULL` if no error. The returned string must be freed with `qcl_free`.

### `qcl_free`

```c
void qcl_free(char* ptr);
```

Free a string previously returned by any QCL function. Safe to call with `NULL`.

## Example

```c
#include <stdio.h>

// Declarations (or use a generated header)
extern char* qcl_eval_json(const char* expression, const char* json_ctx);
extern int   qcl_check_json(const char* expression, const char* json_ctx);
extern char* qcl_last_error(void);
extern void  qcl_free(char* ptr);

int main(void) {
    // Evaluate
    char* result = qcl_eval_json("@x + 1", "{\"x\": 2}");
    if (result) {
        printf("result: %s\n", result); // "3"
        qcl_free(result);
    } else {
        char* err = qcl_last_error();
        fprintf(stderr, "error: %s\n", err);
        qcl_free(err);
    }

    // Check
    int ok = qcl_check_json("@user.role == \"admin\"",
                            "{\"user\": {\"role\": \"admin\"}}");
    printf("check: %d\n", ok); // 1

    return 0;
}
```

Compile and link:

```bash
cc -o example example.c -L target/release -lqcl -Wl,-rpath,target/release
./example
```
