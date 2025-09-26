# QCL Rust Syntax Sugars Implementation TODO

## Priority 1: Critical Implementation

### 1. `..=` Inclusive Range Operator
- **Status**: Only exclusive `..` ranges implemented
- **File**: `core/src/ast.rs:180` (TODO comment exists)
- **Importance**: Fundamental for iteration and slice operations
- **Test Plan**:
  ```rust
  #[test]
  fn test_inclusive_range() {
      test_expr("1..=5", Ok(val!(5))); // Should evaluate to range 1-5
      test_expr("for i in 1..=5 { print(i); }", Ok(val!()));
  }
  ```

### 2. `|param| expr` Closure Syntax
- **Status**: `|` currently used for union types
- **Importance**: Essential for functional programming patterns
- **Test Plan**:
  ```rust
  #[test]
  fn test_closure_syntax() {
      test_expr("let add = |x, y| x + y; add(2, 3)", Ok(val!(5)));
      test_expr("[1, 2, 3].map(|x| x * 2)", Ok(val!([2, 4, 6])));
  }
  ```

### 3. `?` Error Propagation Operator
- **Status**: `?` only used for optional types currently
- **Importance**: Critical for error handling ergonomics
- **Test Plan**:
  ```rust
  #[test]
  fn test_error_propagation() {
      test_expr("fn may_fail() -> Result<int, string> { if @cond { Ok(42) } else { Err(\"fail\") } } may_fail()?", Ok(val!(42)));
  }
  ```

### 4. `match` Expressions
- **Status**: Only `select/case` for concurrency exists
- **Importance**: Fundamental Rust control flow construct
- **Test Plan**:
  ```rust
  #[test]
  fn test_match_expression() {
      test_expr("match x { 1 => \"one\", 2 => \"two\", _ => \"other\" }", Ok(val!(\"one\")));
      test_expr("match @opt { Some(v) => v, None => 0 }", Ok(val!(42)));
  }
  ```

## Priority 2: High Importance

### 5. Method Call Syntax
- **Status**: Functions called as `func(args)` not `obj.method(args)`
- **Importance**: Improves code readability and OO patterns
- **Test Plan**:
  ```rust
  #[test]
  fn test_method_call_syntax() {
      test_expr("\"hello\".length()", Ok(val!(5)));
      test_expr("[1, 2, 3].push(4)", Ok(val!([1, 2, 3, 4])));
  }
  ```

### 6. Struct Literals with Field Shorthand
- **Status**: No struct syntax, only map literals
- **Importance**: Important for data modeling
- **Test Plan**:
  ```rust
  #[test]
  fn test_struct_literals() {
      test_expr("struct Point { x: int, y: int } Point { x: 1, y: 2 }", Ok(val!({\"x\": 1, \"y\": 2})));
      test_expr("let x = 1; let y = 2; Point { x, y }", Ok(val!({\"x\": 1, \"y\": 2})));
  }
  ```

### 7. `if let`/`while let` Expressions
- **Status**: Not implemented
- **Importance**: Common pattern matching shorthand
- **Test Plan**:
  ```rust
  #[test]
  fn test_if_let_expression() {
      test_expr("if let Some(v) = @opt { v * 2 } else { 0 }", Ok(val!(84)));
      test_expr("while let Some(v) = @iterator { print(v); }", Ok(val!()));
  }
  ```

## Priority 3: Medium Importance

### 8. `&` Borrow Operator
- **Status**: Only used for `&&` logical AND
- **Importance**: Would enable reference semantics
- **Test Plan**:
  ```rust
  #[test]
  fn test_borrow_operator() {
      test_expr("let x = 42; let y = &x; *y", Ok(val!(42)));
  }
  ```

### 9. `*` Dereference Operator
- **Status**: Only used for multiplication
- **Importance**: Needed for pointer operations
- **Test Plan**:
  ```rust
  #[test]
  fn test_dereference_operator() {
      test_expr("let x = 42; let y = &x; *y", Ok(val!(42)));
  }
  ```

### 10. Turbofish Syntax `::<T>`
- **Status**: Not implemented
- **Importance**: Needed for explicit type annotation
- **Test Plan**:
  ```rust
  #[test]
  fn test_turbofish_syntax() {
      test_expr("Vec::<int>::new()", Ok(val!([])));
      test_expr("0.5::<float>", Ok(val!(0.5)));
  }
  ```

### 11. Enum Variants with Data
- **Status**: No enum syntax
- **Importance**: Important for algebraic data types
- **Test Plan**:
  ```rust
  #[test]
  fn test_enum_syntax() {
      test_expr("enum Option<T> { Some(T), None } Option::Some(42)", Ok(val!(42)));
      test_expr("match @opt { Option::Some(v) => v, Option::None => 0 }", Ok(val!(42)));
  }
  ```

## Priority 4: Low Importance

### 12. Generic Parameters in Functions/Structs
- **Status**: Type system supports generics but no function syntax
- **Importance**: Needed for reusable code
- **Test Plan**:
  ```rust
  #[test]
  fn test_generic_functions() {
      test_expr("fn id<T>(x: T) -> T { x } id::<int>(42)", Ok(val!(42)));
  }
  ```

### 13. Lifetime Syntax `'a`
- **Status**: Type variables use `'T` but not for lifetimes
- **Importance**: Advanced type system feature
- **Test Plan**:
  ```rust
  #[test]
  fn test_lifetime_syntax() {
      test_expr("fn borrow<'a>(x: &'a int) -> &'a int { x }", Ok(val!(42)));
  }
  ```

### 14. Raw String Literals `r"..."`
- **Status**: Not implemented
- **Importance**: Quality of life feature
- **Test Plan**:
  ```rust
  #[test]
  fn test_raw_string_literals() {
      test_expr(r#"r"C:\path\to\file""#, Ok(val!("C:\\path\\to\\file")));
  }
  ```

### 15. Character Literals `'c'`
- **Status**: Not implemented
- **Importance**: Can use strings instead
- **Test Plan**:
  ```rust
  #[test]
  fn test_character_literals() {
      test_expr("'a'", Ok(val!("a")));
      test_expr("'\n'", Ok(val!("\n")));
  }
  ```

## Implementation Progress

- [ ] `..=` inclusive range operator
- [ ] `|param| expr` closure syntax
- [ ] `?` error propagation operator
- [ ] `match` expressions
- [ ] Method call syntax
- [ ] Struct literals
- [ ] `if let`/`while let` expressions
- [ ] `&` borrow operator
- [ ] `*` dereference operator
- [ ] Turbofish syntax `::<T>`
- [ ] Enum variants with data
- [ ] Generic parameters in functions
- [ ] Lifetime syntax `'a`
- [ ] Raw string literals
- [ ] Character literals

## Implementation Notes

1. **Parser Changes**: Most syntax sugars will require updates to:
   - `core/src/token.rs` (tokenizer)
   - `core/src/ast.rs` (AST nodes)
   - `core/src/expr.rs` (expression parsing)
   - `core/src/stmt.rs` (statement parsing)

2. **Type System**: Some features require type system extensions in:
   - `core/src/val.rs` (value types)
   - `core/src/op.rs` (operations)

3. **Testing Strategy**: For each feature:
   - Add `#[test]` functions in appropriate `*_test.rs` files
   - Use existing `test_expr!` and `test_stmt!` macros
   - Run `cargo test` to verify implementation
   - Ensure all existing tests still pass

4. **Compatibility**: Maintain backward compatibility with existing QCL code