# QCL Rust Syntax Sugar Implementation TODOs

This document lists unimplemented Rust-like syntax sugar features for the QCL language, prioritized by importance and implementation complexity.

## High Priority (Core Language Features)

### 1. Match Expressions
**Status:** Not implemented (only `select/case` for concurrency exists)
**Priority:** Critical
**Complexity:** High
**Description:** Implement Rust-style pattern matching with `match` expressions
```qcl
let result = match value {
    1 => "one",
    2 | 3 => "two or three",
    x if x > 10 => "big number",
    [first, ..rest] => "array destructuring",
    {"key": val} => "object destructuring",
    _ => "default"
};
```
**Implementation:**
- Add `Match` token and expression type
- Extend parser to handle match syntax
- Implement pattern matching evaluation
- Add comprehensive pattern matching tests

### 2. Compound Assignment Operators
**Status:** Not implemented
**Priority:** High
**Complexity:** Low
**Description:** Shorthand assignment operators
```qcl
x += 5;   // x = x + 5
y -= 3;   // y = y - 3
z *= 2;   // z = z * 2
w /= 4;   // w = w / 4
```
**Implementation:**
- Add compound assignment tokens (`+=`, `-=`, `*=`, `/=`, `%=`)
- Add `CompoundAssign` statement type
- Implement evaluation logic

### 3. If Let Expressions
**Status:** Not implemented
**Priority:** High
**Complexity:** Medium
**Description:** Conditional binding with pattern matching
```qcl
if let [first, ..rest] = some_list {
    print(first);
}

if let {"name": name} = user {
    print(name);
}
```

### 4. While Let Loops
**Status:** Not implemented
**Priority:** High
**Complexity:** Medium
**Description:** Loop with pattern matching condition
```qcl
while let [item, ..rest] = queue {
    process(item);
    queue = rest;
}
```

## Medium Priority (Quality of Life)

### 5. Enhanced String Interpolation
**Status:** Basic template strings implemented
**Priority:** Medium
**Complexity:** Low
**Description:** More advanced template string features
```qcl
let name = "Alice";
let greeting = f"Hello {name}!";           // f-string syntax
let formatted = f"Value: {value:2}";       // format specifiers
```

### 6. Range Patterns in Match
**Status:** Basic ranges implemented but not in patterns
**Priority:** Medium
**Complexity:** Medium
**Description:** Match against ranges
```qcl
match age {
    0..=12 => "child",
    13..=19 => "teenager",
    20..=64 => "adult",
    65.. => "senior"
}
```

### 7. Destructuring Assignment
**Status:** Basic pattern matching in for loops exists
**Priority:** Medium
**Complexity:** Medium
**Description:** Assign multiple variables from structures
```qcl
let [x, y, z] = [1, 2, 3];
let {"name": name, "age": age} = user;
let (a, b) = (1, 2);
```

### 8. Method Call Syntax Sugar
**Status:** Only function calls implemented
**Priority:** Medium
**Complexity:** Medium
**Description:** Method-style calls on values
```qcl
"hello".length()        // instead of length("hello")
[1, 2, 3].push(4)      // instead of push([1, 2, 3], 4)
numbers.map(|x| x * 2) // instead of map(numbers, |x| x * 2)
```

## Medium-Low Priority (Convenience Features)

### 9. List/Map Comprehensions
**Status:** Not implemented
**Priority:** Medium-Low
**Complexity:** High
**Description:** Functional-style collection construction
```qcl
let squares = [x * x for x in 1..10];
let evens = [x for x in numbers if x % 2 == 0];
let lookup = {k: v.upper() for (k, v) in pairs};
```

### 10. Pipe Operator
**Status:** Not implemented
**Priority:** Medium-Low
**Complexity:** Medium
**Description:** Function chaining operator
```qcl
let result = value
    |> func1()
    |> func2(arg)
    |> func3();
```

### 11. Null Propagation Extensions
**Status:** Basic `?.` implemented
**Priority:** Medium-Low
**Complexity:** Low
**Description:** Enhanced null-safe operations
```qcl
let result = obj?.method()?.field?.value;
obj?.method(arg)?;  // null-safe method call
```

### 12. Error Propagation Operator (?)
**Status:** `?` only used for optional types
**Priority:** Medium-Low
**Complexity:** High
**Description:** Early return for Result/Option types
```qcl
fn parseNumber(s: String) -> Result<Int, String> {
    let trimmed = s.trim()?;
    let num = trimmed.parseInt()?;
    return Ok(num);
}
```

## Low Priority (Advanced Features)

### 13. Struct Definitions
**Status:** Only map literals exist
**Priority:** Low
**Complexity:** High
**Description:** Named struct types
```qcl
struct Point {
    x: Float,
    y: Float
}

let p = Point { x: 1.0, y: 2.0 };
let Point { x, y } = p;  // destructuring
```

### 14. Enum Types
**Status:** Not implemented
**Priority:** Low
**Complexity:** High
**Description:** Algebraic data types
```qcl
enum Option<T> {
    Some(T),
    None
}

let value = Option::Some(42);
match value {
    Option::Some(x) => x,
    Option::None => 0
}
```

### 15. Generic Function Syntax
**Status:** Type system supports generics but no syntax
**Priority:** Low
**Complexity:** High
**Description:** Generic functions and types
```qcl
fn map<T, U>(list: List<T>, f: fn(T) -> U) -> List<U> {
    // implementation
}
```

### 16. Raw String Literals
**Status:** Not implemented
**Priority:** Low
**Complexity:** Low
**Description:** Escape-free string literals
```qcl
let path = r"C:\Users\file.txt";    // no need to escape backslashes
let regex = r"\d{3}-\d{3}-\d{4}";   // no need to escape regex
```

### 17. Character Literals
**Status:** Not implemented
**Priority:** Low
**Complexity:** Low
**Description:** Single character values
```qcl
let ch = 'a';
let newline = '\n';
```

## Implementation Strategy

### Phase 1: Quick Wins (High Impact, Low Complexity)
1. **Compound Assignment Operators** - Easy to implement, commonly used
2. **Raw String Literals** - Simple tokenizer enhancement
3. **Character Literals** - Basic tokenizer addition
4. **Enhanced String Interpolation** - Build on existing template strings

### Phase 2: Core Language Features (High Impact, Medium-High Complexity)
5. **Match Expressions** - Foundation for pattern matching
6. **If Let/While Let** - Essential control flow
7. **Destructuring Assignment** - Core language feature
8. **Method Call Syntax** - Major ergonomic improvement

### Phase 3: Advanced Features (Medium Impact)
9. **List/Map Comprehensions** - Functional programming support
10. **Pipe Operator** - Function composition
11. **Null Propagation Extensions** - Safety improvements
12. **Range Patterns in Match** - Completes pattern matching

### Phase 4: Type System Extensions (Lower Priority)
13. **Error Propagation Operator** - Requires Result types
14. **Struct Definitions** - Major type system addition
15. **Enum Types** - Algebraic data types
16. **Generic Function Syntax** - Advanced type features

## Implementation Progress

### High Priority
- [x] Match expressions ✅ **COMPLETED**
- [x] Compound assignment operators (`+=`, `-=`, etc.) ✅ **COMPLETED**
- [x] If let expressions ✅ **COMPLETED**
- [x] While let loops ✅ **COMPLETED**

### Medium Priority
- [x] Enhanced string interpolation ✅ **COMPLETED**
- [x] Range patterns in match ✅ **COMPLETED**
- [ ] Destructuring assignment
- [ ] Method call syntax sugar

### Medium-Low Priority
- [ ] List/Map comprehensions
- [ ] Pipe operator (`|>`)
- [ ] Null propagation extensions
- [ ] Error propagation operator (`?`)

### Low Priority
- [ ] Struct definitions
- [ ] Enum types
- [ ] Generic function syntax
- [ ] Raw string literals (`r"..."`)
- [ ] Character literals (`'c'`)

## Implementation Notes

### Parser Changes
Most syntax sugars will require updates to:
- `core/src/token/token.rs` - Add new tokens
- `core/src/ast/ast.rs` - Add new AST node types
- `core/src/expr/expr.rs` - Add new expression types
- `core/src/stmt/stmt.rs` - Add new statement types

### Type System Extensions
Some features require extensions in:
- `core/src/val/val.rs` - New value types (Result, Option, etc.)
- `core/src/op/op.rs` - New operations
- `core/src/typ/typ.rs` - New type definitions

### Testing Strategy
For each feature:
1. Create unit tests in appropriate `*_test.rs` files
2. Test parsing, evaluation, and error cases
3. Use existing test infrastructure (`test_expr!`, `test_stmt!`)
4. Run `cargo test` to verify implementation
5. Ensure all existing tests continue to pass
6. Add integration tests with other features

### Compatibility
- Maintain backward compatibility with existing QCL code
- Ensure new syntax doesn't conflict with existing tokens
- Consider deprecation path for conflicting features
- Document breaking changes if unavoidable

## Next Steps

1. **Start with Compound Assignment Operators** - lowest complexity, highest value
2. **Implement comprehensive tests** for each feature before moving to the next
3. **Run full test suite** after each implementation
4. **Update documentation** for each completed feature
5. **Consider performance impact** of new features on existing code