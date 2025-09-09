# QCL Examples

This directory contains comprehensive examples demonstrating all features of the QCL (Query Check Language) language.

## Files

- `comprehensive_test.qcl` - A complete program showcasing all QCL language features
- `test_context.json` - Sample JSON context data for testing
- `simple_expressions.md` - Collection of simple expression examples
- `README.md` - This file

## How to Run

### Running the Comprehensive Test

Execute the complete program with the test context:

```bash
cat examples/test_context.json | cargo run -- --stmt "$(cat examples/comprehensive_test_simple.qcl)"
```

### Running Individual Expressions

Test individual expressions from the simple_expressions.md file:

```bash
# Basic role check
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- --expr '@req.user.role == "admin"'

# Permission check
echo '{"req": {"user": {"role": "user", "permissions": {"read": true}}}}' | \
  cargo run -- --expr '@req.user.role == "admin" || @req.user.permissions.read == true'

# Age verification
echo '{"user": {"age": 25}}' | cargo run -- --expr '@user.age >= 18 && @user.age <= 65'
```

### Running with Different Formats

#### JSON (default)
```bash
echo '{"user": {"name": "Alice"}}' | cargo run -- --expr '@user.name == "Alice"'
```

#### YAML
```bash
echo 'user:\n  name: Alice' | cargo run -- --yaml --expr '@user.name == "Alice"'
```

#### TOML
```bash
echo '[user]\nname = "Alice"' | cargo run -- --toml --expr '@user.name == "Alice"'
```

### Running with Standard Library Features

Enable stdlib features for mathematical and string operations:

```bash
# With math stdlib
cargo run --features stdlib-math -- --stmt 'import math; return math.sqrt(16);'

# With string stdlib
cargo run --features stdlib-string -- --stmt 'import string; return string.upper("hello");'

# With all stdlib modules
cargo run --features stdlib-all -- --stmt '
import math;
import string;
let result = math.abs(-42) + string.len("hello");
return result;
'
```

## Language Features Demonstrated

### 1. Variable Declarations
- Type annotations (`let x: Int = 42`)
- Untyped variables (`let x = 42`)
- All basic types: String, Int, Float, Bool, Nil, List, Map

### 2. Context Access
- Simple access (`@user.name`)
- Nested access (`@user.profile.address.street`)
- Array access (`@user.skills[0]`)

### 3. Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `in`
- Logical: `&&`, `||`, `!`

### 4. Control Flow
- If/else statements
- While loops
- Break/continue
- Goto/labels
- Return statements

### 5. Functions
- Function definitions (`fn name(params) { ... }`)
- Function calls (`name(args)`)
- Recursive functions
- Functions with context access
- Parameter scoping

### 6. Standard Library
- Math functions: `abs`, `sqrt`, `sin`, `cos`, `max`, `min`, etc.
- String functions: `len`, `lower`, `upper`, `trim`, `contains`, etc.
- Constants: `math.pi`, `math.e`

### 7. Import System
- Module imports (`import math;`)
- Selective imports (`import { abs, sqrt } from math;`)
- Aliased imports (`import math as m;`)

### 8. Real-world Use Cases
- Role-based access control (RBAC)
- Attribute-based access control (ABAC)
- Document permissions
- API rate limiting
- Geo-location restrictions

## Context Structure

The `test_context.json` file demonstrates a typical context structure for access control:

```json
{
  "user": {
    "id": "user123",
    "role": "manager",
    "department": "engineering",
    "permissions": { ... }
  },
  "request": {
    "action": "read",
    "endpoint": "/api/documents"
  },
  "resource": {
    "document": { ... },
    "allowed_teams": [...],
    "geo_restrictions": { ... }
  },
  "context": {
    "timestamp": "2024-03-15T14:30:00Z",
    "user_requests": { ... }
  }
}
```

## Testing Different Scenarios

You can modify the context data to test different scenarios:

1. **Change user role** to test role-based access
2. **Modify permissions** to test fine-grained access control
3. **Update geo restrictions** to test location-based access
4. **Adjust request counts** to test rate limiting
5. **Change resource ownership** to test ownership-based access

## Error Testing

Test error conditions:

```bash
# Undefined variable
echo '{}' | cargo run -- --expr 'undefined_var == true'

# Type mismatch
echo '{"user": {"age": "not_a_number"}}' | cargo run -- --expr '@user.age > 18'

# Invalid syntax
echo '{}' | cargo run -- --expr '@user.name =='
```