# QCL Type System User Manual

## Table of Contents

1. [Overview](#overview)
2. [Type Basics](#type-basics)
3. [Type Annotations](#type-annotations)
4. [Type Inference](#type-inference)
5. [Advanced Types](#advanced-types)
6. [Type Checking Rules](#type-checking-rules)
7. [Generic Types](#generic-types)
8. [Union Types](#union-types)
9. [Optional Types](#optional-types)
10. [Function Types](#function-types)
11. [Traits System](#traits-system)
12. [Common Patterns](#common-patterns)
13. [Error Messages](#error-messages)
14. [Best Practices](#best-practices)

## Overview

QCL features a modern static type system with type inference, generic types, union types, and a traits system. The type system provides compile-time type safety while maintaining the flexibility needed for access control query language scenarios.

### Key Features

- **Static Typing**: Catch type errors before execution
- **Type Inference**: Reduce boilerplate with automatic type deduction
- **Generic Types**: Write reusable code with type parameters
- **Union Types**: Handle multiple possible types safely
- **Optional Types**: Null safety with optional types
- **Traits System**: Define shared behaviors across types
- **Language Server Integration**: Real-time type checking and autocompletion

## Type Basics

### Primitive Types

QCL provides several primitive types:

```qcl
let age: Int = 25;                    // 64-bit integers
let price: Float = 19.99;             // 64-bit floating point
let name: String = "Alice";           // Strings
let is_active: Bool = true;           // Booleans
let empty: Nil = nil;                 // Null/empty values
let anything: Any = 42;               // Accepts any type
```

### Container Types

```qcl
let numbers: List<Int> = [1, 2, 3, 4, 5];
let user_data: Map<String, Any> = {
    "name": "Alice",
    "age": 30,
    "active": true
};
```

### Basic Operations

```qcl
// Numeric operations
let sum: Int = 10 + 20;
let product: Float = 3.14 * 2.0;

// String operations
let greeting: String = "Hello, " + "World!";
let contains: Bool = greeting.contains("World");

// List operations
let first: Int = numbers[0];
let length: Int = numbers.length();
```

## Type Annotations

### Variable Declarations

Type annotations are optional but recommended for clarity:

```qcl
// With explicit type annotation
let count: Int = 100;
let username: String = "admin";

// Without annotation (type inferred)
let count = 100;           // Inferred as Int
let username = "admin";    // Inferred as String
```

### Function Parameters

```qcl
fn calculate_total(price: Float, quantity: Int) -> Float {
    return price * quantity;
}

fn greet(name: String, greeting: String = "Hello") -> String {
    return greeting + ", " + name + "!";
}
```

### Return Type Annotations

```qcl
fn get_user_name(user_id: Int) -> String {
    // Function implementation
    return "User_" + user_id.to_string();
}

fn is_admin(user: Map<String, Any>) -> Bool {
    return user.get("role") == "admin";
}
```

## Type Inference

QCL uses Hindley-Milner type inference to automatically deduce types:

```qcl
// Basic inference
let x = 42;                    // Inferred as Int
let y = 3.14;                  // Inferred as Float
let name = "Alice";            // Inferred as String
let active = true;             // Inferred as Bool

// Container inference
let numbers = [1, 2, 3];       // Inferred as List<Int>
let user = {                   // Inferred as Map<String, Any>
    "name": "Bob",
    "age": 25
};

// Expression inference
let sum = 10 + 20;            // Inferred as Int
let result = sum > 15;         // Inferred as Bool
```

### Complex Inference Examples

```qcl
// Function return type inference
fn create_user() {
    return {
        "id": 1,
        "name": "Alice"
    };  // Return type inferred as Map<String, Any>
}

// Generic type inference
let items = [1, 2, 3];        // List<Int>
let first = items[0];          // Int

// Union type inference
let value = if condition() { 42 } else { "hello" };  // Int | String
```

## Advanced Types

### Any Type

The `Any` type accepts values of any type:

```qcl
let flexible: Any = 42;                    // Int
flexible = "hello";                        // Now String
flexible = [1, 2, 3];                      // Now List<Int>
```

### Named Types

Create type aliases for better readability:

```qcl
type UserId = Int;
type UserName = String;
type UserRole = String;

let user_id: UserId = 12345;
let user_name: UserName = "alice";
let user_role: UserRole = "admin";
```

### Variable Types (for Inference)

Type variables used during inference:

```qcl
// These are mostly internal to the type system
// 'T, 'K, 'V are type variables
fn identity<T>(value: T) -> T {
    return value;
}
```

## Type Checking Rules

### Assignment Compatibility

```qcl
// Same types are always compatible
let x: Int = 42;
let y: Int = x;           // OK

// Any accepts all types
let anything: Any = 42;    // OK
anything = "hello";       // OK
anything = [1, 2, 3];     // OK

// Optional subtyping
let optional_int: Int | Nil = 42;      // OK: Int -> ?Int
let optional_int: Int | Nil = nil;    // OK: Nil -> ?Int

// Union types
let union: Int | String = 42;           // OK
let union: Int | String = "hello";      // OK
let union: Int | String | Bool = true;  // OK
```

### Numeric Conversion

```qcl
// Automatic numeric conversion
let int_val: Int = 42;
let float_val: Float = int_val;    // OK: Int -> Float

let float_num: Float = 3.14;
// let int_num: Int = float_num;   // Error: Float -> Int not automatic
```

### Container Type Rules

```qcl
// List covariance
let int_list: List<Int> = [1, 2, 3];
let any_list: List<Any> = int_list;     // OK: List<Int> -> List<Any>

// Map covariance
let string_map: Map<String, Int> = {"a": 1, "b": 2};
let any_map: Map<String, Any> = string_map;  // OK
```

## Generic Types

### Generic Functions

```qcl
// Generic function with type parameter
fn first<T>(items: List<T>) -> ?T {
    if items.length() > 0 {
        return items[0];
    }
    return nil;
}

// Usage
let numbers = [1, 2, 3];
let first_num = first(numbers);    // Inferred as ?Int

let strings = ["a", "b", "c"];
let first_str = first(strings);    // Inferred as ?String
```

### Generic Containers

```qcl
// Lists with specific types
let int_list: List<Int> = [1, 2, 3];
let string_list: List<String> = ["a", "b", "c"];

// Maps with specific types
let int_map: Map<String, Int> = {"one": 1, "two": 2};
let user_map: Map<String, Map<String, Any>> = {
    "user1": {"name": "Alice", "age": 30},
    "user2": {"name": "Bob", "age": 25}
};
```

### Generic Type Constraints

```qcl
// Functions with trait constraints (when traits are implemented)
fn process_displayable<T: Display>(item: T) -> String {
    return item.display();
}
```

## Union Types

### Basic Union Types

```qcl
// Define variables that can hold multiple types
let result: Int | String = 42;            // Can be Int or String
let flexible: Int | String | Bool = true; // Can be Int, String, or Bool

// Assignment examples
result = 100;                            // OK: Int
result = "hello";                        // OK: String
// result = true;                        // Error: Bool not in union
```

### Union Type Patterns

```qcl
// Pattern matching with union types
fn process_value(value: Int | String) -> String {
    if value is Int {
        return "Number: " + value.to_string();
    } else if value is String {
        return "Text: " + value;
    }
}

// Using union types in function returns
fn get_config(key: String) -> String | Nil {
    return config_map.get(key);  // Returns String or Nil
}
```

### Common Union Type Use Cases

```qcl
// Configuration values that can be different types
let config: Map<String, Int | String | Bool> = {
    "port": 8080,
    "host": "localhost",
    "debug": true
};

// API responses that can succeed or fail
type Response<T, E> = {
    "success": Bool,
    "data": ?T,
    "error": ?E
};
```

## Optional Types

### Optional Type Syntax

```qcl
// Optional types are sugar for union with Nil
let optional_int: ?Int = 42;        // Same as Int | Nil
let optional_string: ?String = nil; // Same as String | Nil

// Function returning optional
fn find_user(id: Int) -> ?Map<String, Any> {
    let users = get_all_users();
    for user in users {
        if user.get("id") == id {
            return user;
        }
    }
    return nil;
}
```

### Optional Operations

```qcl
// Safe property access
let user: ?Map<String, Any> = find_user(123);
let username = user?.get("name");  // Safe access, returns Nil if user is Nil

// Optional chaining
let config: ?Map<String, Any> = get_config();
let value = config?.get("nested")?.get("value");  // Chain of safe accesses

// Default values
let username = user?.get("name") ?? "Guest";     // Default if Nil
```

### Optional Pattern Matching

```qcl
// Checking for presence
fn process_user(user: ?Map<String, Any>) -> String {
    if user != nil {
        return "User: " + user.get("name");
    } else {
        return "No user found";
    }
}

// Using optional in conditions
if let user = find_user(123) {
    // user is guaranteed non-Nil here
    return user.get("name");
} else {
    return "User not found";
}
```

## Function Types

### Function Type Syntax

```qcl
// Function type annotations
let operation: (Int, Int) -> Int = fn(a, b) { return a + b; };
let predicate: (String) -> Bool = fn(s) { return s.length() > 0; };

// Higher-order functions
fn apply<T, U>(func: (T) -> U, value: T) -> U {
    return func(value);
}

// Usage
let double = fn(x: Int) -> Int { return x * 2; };
let result = apply(double, 5);  // Returns 10
```

### Function Type Variance

```qcl
// Function types follow contravariant parameter, covariant return rules
let specific: (Int) -> Int = fn(x) { return x * 2; };
let general: (Any) -> Any = specific;  // OK: More general parameter, more general return
```

## Traits System

### Trait Definitions

```qcl
// Define a trait with method signatures
trait Display {
    fn display() -> String;
}

trait Comparable {
    fn compare(other: Self) -> Int;  // -1, 0, 1
    fn equals(other: Self) -> Bool;
}
```

### Trait Implementations

```qcl
// Implement trait for a type
impl Display for Int {
    fn display() -> String {
        return self.to_string();
    }
}

impl Display for String {
    fn display() -> String {
        return self;
    }
}

impl Comparable for Int {
    fn compare(other: Int) -> Int {
        if self < other { return -1; }
        if self > other { return 1; }
        return 0;
    }
    
    fn equals(other: Int) -> Bool {
        return self == other;
    }
}
```

### Using Traits

```qcl
// Functions with trait bounds
fn format_item<T: Display>(item: T) -> String {
    return "Item: " + item.display();
}

// Usage
let number = 42;
let text = "hello";
let formatted_num = format_item(number);  // "Item: 42"
let formatted_text = format_item(text);   // "Item: hello"
```

## Common Patterns

### Builder Pattern with Types

```qcl
type UserBuilder = {
    "name": ?String,
    "age": ?Int,
    "role": ?String
};

fn create_user() -> UserBuilder {
    return {
        "name": nil,
        "age": nil,
        "role": nil
    };
}

fn with_name(builder: UserBuilder, name: String) -> UserBuilder {
    builder.name = name;
    return builder;
}

fn build(builder: UserBuilder) -> Map<String, Any> {
    return {
        "name": builder.name ?? "Unknown",
        "age": builder.age ?? 0,
        "role": builder.role ?? "user"
    };
}
```

### Result Type Pattern

```qcl
type Result<T, E> = {
    "success": Bool,
    "value": ?T,
    "error": ?E
};

fn ok<T>(value: T) -> Result<T, String> {
    return {"success": true, "value": value, "error": nil};
}

fn err<E>(error: E) -> Result<Any, E> {
    return {"success": false, "value": nil, "error": error};
}

fn divide(a: Float, b: Float) -> Result<Float, String> {
    if b == 0.0 {
        return err("Division by zero");
    }
    return ok(a / b);
}
```

### Type-Safe Configuration

```qcl
type DatabaseConfig = {
    "host": String,
    "port": Int,
    "username": String,
    "password": String,
    "database": String,
    "ssl": Bool
};

fn validate_config(config: Map<String, Any>) -> Result<DatabaseConfig, String> {
    // Validation logic
    if !config.has("host") || config.get("host") == "" {
        return err("Host is required");
    }
    // ... more validation
    return ok(config as DatabaseConfig);
}
```

## Error Messages

### Common Type Errors

```qcl
// Type mismatch error
let x: Int = "hello";  // Error: Expected Int, got String

// Missing property error
let user = {"name": "Alice"};
let age = user.age;    // Error: Map has no property 'age'

// Union type error
let value: Int | String = true;  // Error: Bool not assignable to Int | String

// Generic constraint error
fn process<T: Display>(item: T) {
    // ...
}
process(42);          // OK if Int implements Display
process({"a": 1});    // Error: Map doesn't implement Display
```

### Understanding Type Error Messages

```
Error: Type mismatch
  Expected: Int
  Found: String
  Location: examples.qcl:10:15
  let x: Int = "hello";
               ^^^^^^

Error: Missing required property
  Property 'age' not found in Map<String, Any>
  Available properties: name, email
  Location: examples.qcl:25:10
  let age = user.age;
           ^^^^^^^^^
```

## Best Practices

### Type Annotation Guidelines

```qcl
// DO: Add type annotations for public APIs
fn calculate_tax(income: Float, rate: Float) -> Float {
    return income * rate;
}

// DO: Use explicit types for complex expressions
let result: Map<String, List<Int>> = complex_function();

// OK: Omit annotations for simple local variables
let count = 42;
let name = "Alice";

// DO: Annotate function parameters and returns
fn process_user(user_id: Int) -> Map<String, Any> {
    // ...
}
```

### Type Safety Patterns

```qcl
// Use optional types for potentially missing values
fn get_user(id: Int) -> ?Map<String, Any> {
    // Return nil if not found
}

// Use union types for multiple valid types
fn parse_value(input: String) -> Int | String | Bool {
    // Parse and return appropriate type
}

// Use specific types instead of Any when possible
let items: List<String> = ["a", "b", "c"];  // Better than List<Any>
```

### Performance Considerations

```qcl
// Prefer specific types over Any for better performance
let numbers: List<Int> = [1, 2, 3];         // Better than List<Any>

// Use appropriate numeric types
let count: Int = 100;                       // Better than Float for integers

// Consider memory usage for large collections
let large_dataset: List<Int> = get_data();   // More efficient than List<Any>
```

### Error Handling Patterns

```qcl
// Use Result types for operations that can fail
fn read_file(path: String) -> Result<String, String> {
    // Return success or error
}

// Use optional chaining for safe access
let value = config?.get("nested")?.get("value") ?? "default";

// Provide meaningful error messages
fn validate_user(user: Map<String, Any>) -> Result<User, String> {
    if !user.has("name") {
        return err("User name is required");
    }
    // ...
}
```

## Advanced Topics

### Recursive Types

```qcl
// Self-referential types (when supported)
type TreeNode = {
    "value": Int,
    "left": ?TreeNode,
    "right": ?TreeNode
};
```

### Higher-Kinded Types

```qcl
// Types that take type parameters (advanced usage)
type Functor<F> = {
    "map": fn<F<A>, A, B>(fn(A) -> B) -> F<B>
};
```

### Type-Level Programming

```qcl
// Using types for compile-time validation
type PositiveInt = Int where self > 0;
type NonEmptyString = String where self.length() > 0;
```

This manual provides a comprehensive guide to QCL's type system. For specific API documentation and examples, refer to the standard library documentation and language server integration features.