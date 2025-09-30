# QCL 语法糖设计参考

基于 TypeScript 和 Rust 的优秀语法糖特性，以下是适合在 QCL 中实现的语法糖建议。

## 1. 可选链操作符 (Optional Chaining)

**来自 TypeScript**

```qcl
// 当前写法
if (@req.user && @req.user.profile && @req.user.profile.age > 18) { ... }

// 语法糖
if (@req.user?.profile?.age > 18) { ... }
```

**优势**：简化深层嵌套属性访问，避免空值错误

## 2. 空值合并运算符 (Nullish Coalescing)

**来自 TypeScript**

```qcl
// 当前写法
let name = @req.user.name || "Guest";

// 语法糖
let name = @req.user.name ?? "Guest";
```

**优势**：更精确的空值判断（只处理 null 和 undefined）

## 3. 模板字符串 (Template Literals)

**来自 TypeScript**

```qcl
// 当前写法
"User " + @req.user.name + " has role " + @req.user.role

// 语法糖
`User ${@req.user.name} has role ${@req.user.role}`
```

**优势**：字符串拼接更简洁，支持表达式嵌入

## 4. 解构赋值 (Destructuring)

**来自 TypeScript/Rust**

```qcl
// 当前写法
let user = @req.user;
let name = user.name;
let age = user.age;

// 语法糖
let { name, age } = @req.user;

// 数组解构
let [first, second] = items;
```

**优势**：快速提取对象或数组的属性

## 5. 展开运算符 (Spread Operator)

**来自 TypeScript**

```qcl
// 当前写法
let newItems = [];
for (item in items) {
    newItems.push(item);
}
newItems.push(newItem);

// 语法糖
let newItems = [...items, newItem];

// 对象展开
let newUser = { ...@req.user, role: "admin" };
```

**优势**：简化数组和对象的复制与合并

## 6. 箭头函数 (Arrow Functions)

**来自 TypeScript/Rust**

```qcl
// 当前写法
fn add(a, b) {
    return a + b;
}

// 语法糖
let add = (a, b) => a + b;

// 多行箭头函数
let process = (data) => {
    let result = data * 2;
    return result + 1;
};
```

**优势**：函数定义更简洁，适合作为回调

## 7. 模式匹配 (Pattern Matching)

**来自 Rust**

```qcl
// 当前写法
if (value == 1) {
    print("One");
} else if (value == 2) {
    print("Two");
} else {
    print("Other");
}

// 语法糖
match value {
    1 => print("One"),
    2 => print("Two"),
    _ => print("Other")
}

// 复杂模式匹配
match user {
    { name: "admin", role: "admin" } => grantAdminAccess(),
    { name: name, age: age } if age >= 18 => grantAccess(name),
    _ => denyAccess()
}
```

**优势**：更强大的条件分支处理

## 8. 类型推断 (Type Inference)

**来自 Rust**

```qcl
// 当前写法
let name: string = "John";
let age: int = 25;

// 语法糖
let name = "John";  // 自动推断为 string
let age = 25;       // 自动推断为 int
```

**优势**：减少重复的类型注解

## 9. 运算符重载 (Operator Overloading)

**来自 Rust**

```qcl
// 自定义类型的运算符
impl Vector {
    fn +(other: Vector) -> Vector {
        return Vector {
            x: self.x + other.x,
            y: self.y + other.y
        };
    }
}

// 使用
let v1 = Vector { x: 1, y: 2 };
let v2 = Vector { x: 3, y: 4 };
let v3 = v1 + v2;  // 使用自定义的 + 运算符
```

**优势**：让自定义类型支持自然运算

## 10. 链式调用 (Method Chaining)

**来自 Rust**

```qcl
// 当前写法
let data = parseJson(input);
let filtered = filter(data, item => item.active);
let result = map(filtered, item => item.name);

// 语法糖
let result = input.parseJson()
                   .filter(item => item.active)
                   .map(item => item.name);
```

**优势**：更流畅的数据处理流程

## 11. 闭包和捕获 (Closures)

**来自 Rust**

```qcl
// 闭包自动捕获环境变量
let multiplier = 10;
let multiply = |x| x * multiplier;

// 使用
print(multiply(5));  // 输出 50
```

**优势**：简化函数式编程模式

## 12. 枚举与数据关联 (Enums with Data)

**来自 Rust**

```qcl
// 定义带数据的枚举
enum Result<T, E> {
    Ok(T),
    Err(E)
}

// 使用
let result = parseUser(input);
match result {
    Ok(user) => print("User: " + user.name),
    Err(error) => print("Error: " + error.message)
}
```

**优势**：更安全的错误处理和数据建模

## 13. 生成器函数 (Generator Functions)

**来自 TypeScript**

```qcl
// 生成器函数
function* range(start, end) {
    for (let i = start; i <= end; i++) {
        yield i;
    }
}

// 使用
for (let num in range(1, 5)) {
    print(num);
}
```

**优势**：简化迭代器的创建

## 14. 异步/等待 (Async/Await)

**来自 TypeScript**

```qcl
// 异步函数
async function fetchData() {
    let response = await fetch("https://api.example.com/data");
    let data = await response.json();
    return data;
}

// 使用
fetchData().then(data => print(data));
```

**优势**：简化异步编程模式

## 15. 记录更新语法 (Record Update Syntax)

**来自 Rust**

```qcl
// 当前写法
let newUser = {
    name: @req.user.name,
    age: @req.user.age,
    role: "admin"  // 更新 role
};

// 语法糖
let newUser = { ..@req.user, role: "admin" };
```

**优势**：基于现有对象创建新对象更简洁

## 优先级建议

### 高优先级 (立即实现)
1. **可选链操作符** - 大幅简化嵌套访问
2. **空值合并运算符** - 改善空值处理
3. **模板字符串** - 最常用的字符串处理

### 中优先级 (下个版本)
4. **解构赋值** - 提升代码可读性
5. **展开运算符** - 简化数据操作
6. **箭头函数** - 简化函数定义

### 低优先级 (未来版本)
7. **模式匹配** - 需要较大语法改动
8. **类型推断** - 需要类型系统增强
9. **异步/等待** - 需要运行时支持

## 实现注意事项

1. **向后兼容**：所有语法糖都应有对应的传统写法
2. **性能考虑**：语法糖不应影响运行时性能
3. **错误处理**：提供清晰的错误信息
4. **LSP 支持**：确保语言服务器能正确处理新语法
5. **文档更新**：及时更新语法文档和示例

这些语法糖将显著提升 QCL 的开发体验，使其更现代化和易用。