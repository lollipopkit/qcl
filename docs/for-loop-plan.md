# For 循环语法实现计划

## 1. 目标与语言设计

### 1.1 语法目标
实现 `for` 循环语法，支持自定义迭代器函数，语法形式：
```qcl
for item in iterable_expr {
    // 循环体
}
```

### 1.2 迭代器机制设计
- **内置迭代器支持**：数组/列表、Map、字符串、范围表达式（如 `0..10`）
- **自定义迭代器函数**：支持返回 `List` 或实现特定迭代协议的函数
- **惰性求值**：迭代器函数在每次循环时调用，支持动态生成序列

### 1.3 语法示例
```qcl
// 简单遍历数组
for item in [1, 2, 3] {
    print(item);
}

// 忽略值 (类似 Rust 的 _)
for _ in 0..3 {
    print("Hello!");
}

// 元组解构 - 遍历键值对
for (key, value) in {"a": 1, "b": 2} {
    print(key + ": " + value);
}

// 数组解构 - 固定长度
for [x, y] in [[1, 2], [3, 4], [5, 6]] {
    print("Point: (" + x + ", " + y + ")");
}

// 嵌套解构 - 复杂数据结构
for [name, (age, city)] in [["Alice", [25, "NYC"]], ["Bob", [30, "LA"]]] {
    print(name + " is " + age + " years old, lives in " + city);
}

// 部分解构，剩余忽略 (类似 Rust 的 ..)
for [first, ..] in [["a", "b", "c"], ["x", "y", "z"]] {
    print("First: " + first);
}

// 使用范围语法
for i in 0..10 {
    print("Number: " + i);
}

// 解构结合范围
for (index, value) in enumerate([10, 20, 30]) {
    print(index + ": " + value);
}

// 模式匹配守卫条件 (未来扩展)
for item in items if item > 0 {
    print("Positive: " + item);
}
```

## 2. 语法扩展与 AST 设计

### 2.1 Token 扩展
在 `core/src/token.rs` 中添加：
```rust
pub enum Token {
    // ... 现有 tokens
    For,    // for 关键字
    In,     // in 关键字 (已存在，用于成员检查)
    // ... 其他
}
```

### 2.2 语法规则扩展
更新语句语法，参考 Rust 的模式匹配：
```
statement ::= ... | for_stmt
for_stmt  ::= 'for' pattern 'in' expr statement
pattern   ::= ident_pattern | tuple_pattern | array_pattern | ignore_pattern
ident_pattern ::= IDENT
tuple_pattern ::= '(' pattern {',' pattern} [','] ')'
array_pattern ::= '[' pattern {',' pattern} [','] ['..' [IDENT]] ']'
ignore_pattern ::= '_'
```

### 2.3 AST 节点扩展
在 `core/src/stmt.rs` 中添加：
```rust
pub enum Stmt {
    // ... 现有语句类型
    /// for pattern in iterable { body }
    For {
        pattern: ForPattern,
        iterable: Box<Expr>,
        body: Box<Stmt>,
    },
}

/// For 循环的模式匹配 (类似 Rust 的 Pattern)
#[derive(Debug, Clone, PartialEq)]
pub enum ForPattern {
    /// 简单变量绑定：for x in iter
    Variable(String),
    /// 忽略模式：for _ in iter
    Ignore,
    /// 元组解构：for (a, b, c) in iter
    Tuple(Vec<ForPattern>),
    /// 数组解构：for [a, b] in iter
    Array {
        patterns: Vec<ForPattern>,
        rest: Option<String>, // for [a, b, ..rest] or [a, b, ..]
    },
}
```

## 3. 词法和语法解析

### 3.1 词法分析器更新
在 `core/src/token.rs` 的 tokenizer 中添加 `for` 关键字识别：
```rust
impl Tokenizer {
    fn tokenize(&mut self) -> Result<Vec<Token>> {
        // ... 现有逻辑
        match word.as_str() {
            // ... 现有关键字
            "for" => self.push_token(Token::For),
            // ... 其他
        }
    }
}
```

### 3.2 语句解析器更新
在 `core/src/stmt_parser.rs` 中添加：
```rust
impl StmtParser {
    pub fn parse_statement(&mut self) -> Result<Stmt> {
        match &self.tokens[self.pos] {
            // ... 现有语句解析
            Token::For => self.parse_for_stmt(),
            // ... 其他
        }
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt> {
        self.expect(Token::For)?;  // 消费 'for'
        
        // 解析模式 (变量名或解构)
        let pattern = self.parse_for_pattern()?;
        
        self.expect(Token::In)?;   // 消费 'in'
        
        // 解析可迭代表达式
        let mut expr_parser = ExprParser::new(&self.tokens[self.pos..]);
        let iterable = Box::new(expr_parser.parse_expr()?);
        self.pos += expr_parser.pos(); // 更新位置
        
        // 解析循环体
        let body = Box::new(self.parse_statement()?);
        
        Ok(Stmt::For { pattern, iterable, body })
    }

    fn parse_for_pattern(&mut self) -> Result<ForPattern> {
        match &self.tokens[self.pos] {
            // 忽略模式: _
            Token::Ident(name) if name == "_" => {
                self.pos += 1;
                Ok(ForPattern::Ignore)
            }
            // 简单变量: identifier
            Token::Ident(name) => {
                let var_name = name.clone();
                self.pos += 1;
                Ok(ForPattern::Variable(var_name))
            }
            // 元组模式: (a, b, c)
            Token::LParen => {
                self.pos += 1; // 消费 '('
                let mut patterns = Vec::new();
                
                // 处理空元组 ()
                if self.tokens[self.pos] == Token::RParen {
                    self.pos += 1;
                    return Ok(ForPattern::Tuple(patterns));
                }
                
                loop {
                    patterns.push(self.parse_for_pattern()?);
                    
                    match &self.tokens[self.pos] {
                        Token::Comma => {
                            self.pos += 1; // 消费 ','
                            // 允许尾随逗号: (a, b,)
                            if self.tokens[self.pos] == Token::RParen {
                                break;
                            }
                            continue;
                        }
                        Token::RParen => break,
                        _ => return Err(anyhow!("Expected ',' or ')' in tuple pattern")),
                    }
                }
                
                self.pos += 1; // 消费 ')'
                Ok(ForPattern::Tuple(patterns))
            }
            // 数组模式: [a, b] 或 [a, b, ..rest]
            Token::LBracket => {
                self.pos += 1; // 消费 '['
                let mut patterns = Vec::new();
                let mut rest = None;
                
                // 处理空数组 []
                if self.tokens[self.pos] == Token::RBracket {
                    self.pos += 1;
                    return Ok(ForPattern::Array { patterns, rest });
                }
                
                loop {
                    // 检查剩余模式 ..
                    if self.tokens[self.pos] == Token::Range { // 假设 .. 是 Range token
                        self.pos += 1; // 消费 '..'
                        
                        // 可选的剩余变量名
                        if let Token::Ident(name) = &self.tokens[self.pos] {
                            rest = Some(name.clone());
                            self.pos += 1;
                        }
                        
                        // 剩余模式后不能再有其他模式
                        match &self.tokens[self.pos] {
                            Token::RBracket => break,
                            Token::Comma => {
                                self.pos += 1;
                                if self.tokens[self.pos] == Token::RBracket {
                                    break;
                                } else {
                                    return Err(anyhow!("No patterns allowed after rest pattern"));
                                }
                            }
                            _ => return Err(anyhow!("Expected ']' or ',' after rest pattern")),
                        }
                    } else {
                        patterns.push(self.parse_for_pattern()?);
                    }
                    
                    match &self.tokens[self.pos] {
                        Token::Comma => {
                            self.pos += 1; // 消费 ','
                            // 允许尾随逗号: [a, b,]
                            if self.tokens[self.pos] == Token::RBracket {
                                break;
                            }
                            continue;
                        }
                        Token::RBracket => break,
                        _ => return Err(anyhow!("Expected ',' or ']' in array pattern")),
                    }
                }
                
                self.pos += 1; // 消费 ']'
                Ok(ForPattern::Array { patterns, rest })
            }
            _ => Err(anyhow!("Expected pattern after 'for'")),
        }
    }
}
```

## 4. 执行引擎实现

### 4.1 For 循环执行逻辑
在 `core/src/stmt.rs` 的 `Stmt::execute` 中添加：
```rust
impl Stmt {
    pub fn execute(&self, env: &mut Environment, ctx: &Val) -> Result<ControlFlow> {
        match self {
            // ... 现有语句执行
            Stmt::For { pattern, iterable, body } => {
                // 求值可迭代表达式
                let iter_val = iterable.eval_with_env(ctx, Some(env))?;
                
                // 获取迭代器
                let iterator = create_iterator(&iter_val)?;
                
                // 执行循环
                for item in iterator {
                    // 进入新作用域用于模式绑定
                    env.push_scope();
                    
                    // 根据模式绑定变量 (类似 Rust 的模式匹配)
                    if let Err(e) = bind_pattern(pattern, &item, env) {
                        env.pop_scope(); // 清理作用域
                        return Err(e);
                    }
                    
                    // 执行循环体
                    let result = body.execute(env, ctx);
                    env.pop_scope(); // 清理循环变量作用域
                    
                    match result? {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::None => {}
                    }
                }
                
                Ok(ControlFlow::None)
            }
        }
    }
}
```

### 4.2 迭代器创建函数
```rust
/// 从值创建迭代器
fn create_iterator(val: &Val) -> Result<Box<dyn Iterator<Item = Val>>> {
    match val {
        Val::List(list) => {
            Ok(Box::new(list.iter().cloned()))
        }
        Val::Map(map) => {
            // 返回 [key, value] 对的迭代器
            let pairs: Vec<Val> = map.iter()
                .map(|(k, v)| Val::List(vec![Val::Str(k.clone()), v.clone()]))
                .collect();
            Ok(Box::new(pairs.into_iter()))
        }
        Val::Str(s) => {
            // 按字符迭代
            let chars: Vec<Val> = s.chars()
                .map(|c| Val::Str(c.to_string()))
                .collect();
            Ok(Box::new(chars.into_iter()))
        }
        _ => {
            // 检查是否为函数调用结果，如果是则尝试调用
            if let Ok(result) = val.try_call(&[]) {
                create_iterator(&result)
            } else {
                Err(anyhow!("Value is not iterable: {:?}", val))
            }
        }
    }
}

/// 模式匹配绑定函数 (类似 Rust 的模式匹配语义)
fn bind_pattern(pattern: &ForPattern, value: &Val, env: &mut Environment) -> Result<()> {
    match pattern {
        ForPattern::Variable(name) => {
            env.define(name.clone(), value.clone());
            Ok(())
        }
        ForPattern::Ignore => {
            // _ 模式不绑定任何变量
            Ok(())
        }
        ForPattern::Tuple(patterns) => {
            match value {
                Val::List(list) => {
                    if patterns.len() != list.len() {
                        return Err(anyhow!(
                            "Tuple pattern length mismatch: expected {}, got {}",
                            patterns.len(),
                            list.len()
                        ));
                    }
                    for (pattern, val) in patterns.iter().zip(list.iter()) {
                        bind_pattern(pattern, val, env)?;
                    }
                    Ok(())
                }
                _ => Err(anyhow!("Cannot match tuple pattern against non-list value: {:?}", value)),
            }
        }
        ForPattern::Array { patterns, rest } => {
            match value {
                Val::List(list) => {
                    // 检查最小长度要求
                    if list.len() < patterns.len() {
                        return Err(anyhow!(
                            "Array too short: expected at least {}, got {}",
                            patterns.len(),
                            list.len()
                        ));
                    }
                    
                    // 绑定前面的固定模式
                    for (i, pattern) in patterns.iter().enumerate() {
                        bind_pattern(pattern, &list[i], env)?;
                    }
                    
                    // 处理剩余模式 (如果有)
                    if let Some(rest_var) = rest {
                        let rest_values: Vec<Val> = list[patterns.len()..].to_vec();
                        env.define(rest_var.clone(), Val::List(rest_values));
                    }
                    
                    Ok(())
                }
                _ => Err(anyhow!("Cannot match array pattern against non-list value: {:?}", value)),
            }
        }
    }
}
```

## 5. 范围表达式支持

### 5.1 范围语法扩展
在表达式中支持范围操作符 `..`：
```rust
// 在 token.rs 中添加
pub enum Token {
    // ... 现有 tokens
    Range,  // .. 操作符
}

// 在 expr.rs 中添加
pub enum Expr {
    // ... 现有表达式
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,  // .. vs ..=
    },
}
```

### 5.2 范围求值
```rust
impl Expr {
    fn eval_with_env(&self, ctx: &Val, env: Option<&mut Environment>) -> Result<Val> {
        match self {
            // ... 现有表达式求值
            Expr::Range { start, end, inclusive } => {
                let start_val = match start {
                    Some(expr) => expr.eval_with_env(ctx, env)?,
                    None => Val::Int(0),
                };
                let end_val = match end {
                    Some(expr) => expr.eval_with_env(ctx, env)?,
                    None => return Err(anyhow!("Open-ended ranges not supported in for loops")),
                };
                
                // 生成范围列表
                match (start_val, end_val) {
                    (Val::Int(s), Val::Int(e)) => {
                        let range: Vec<Val> = if *inclusive {
                            (s..=e).map(Val::Int).collect()
                        } else {
                            (s..e).map(Val::Int).collect()
                        };
                        Ok(Val::List(range))
                    }
                    _ => Err(anyhow!("Range bounds must be integers")),
                }
            }
        }
    }
}
```

## 6. 标准库扩展

### 6.1 迭代器工具函数
在 `stdlib/` 中添加迭代器相关的工具模块：

```rust
// stdlib/src/iter.rs
use qcl_core::val::Val;
use anyhow::Result;

/// enumerate - 为序列添加索引
pub fn enumerate(args: Vec<Val>) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("enumerate expects 1 argument"));
    }
    
    match &args[0] {
        Val::List(list) => {
            let enumerated: Vec<Val> = list.iter()
                .enumerate()
                .map(|(i, v)| Val::List(vec![Val::Int(i as i64), v.clone()]))
                .collect();
            Ok(Val::List(enumerated))
        }
        _ => Err(anyhow!("enumerate expects a list")),
    }
}

/// range - 生成整数范围
pub fn range(args: Vec<Val>) -> Result<Val> {
    let (start, end, step) = match args.len() {
        1 => (0, extract_int(&args[0])?, 1),
        2 => (extract_int(&args[0])?, extract_int(&args[1])?, 1),
        3 => (extract_int(&args[0])?, extract_int(&args[1])?, extract_int(&args[2])?),
        _ => return Err(anyhow!("range expects 1-3 arguments")),
    };
    
    let mut result = Vec::new();
    let mut current = start;
    
    if step > 0 {
        while current < end {
            result.push(Val::Int(current));
            current += step;
        }
    } else if step < 0 {
        while current > end {
            result.push(Val::Int(current));
            current += step;
        }
    }
    
    Ok(Val::List(result))
}

fn extract_int(val: &Val) -> Result<i64> {
    match val {
        Val::Int(i) => Ok(*i),
        _ => Err(anyhow!("Expected integer")),
    }
}
```

### 6.2 注册到模块系统
```rust
// stdlib/src/lib.rs 中添加
#[cfg(feature = "stdlib-iter")]
pub mod iter;

// 在模块注册中添加
registry.register_function("enumerate", iter::enumerate);
registry.register_function("range", iter::range);
```

## 7. 错误处理与类型检查

### 7.1 静态分析增强
在 LSP 中为 for 循环添加：
- **类型检查**：验证迭代表达式返回可迭代类型
- **解构模式验证**：检查解构变量数量与预期匹配
- **作用域分析**：正确处理循环变量的作用域

### 7.2 运行时错误
- **不可迭代错误**：当表达式不返回可迭代值时的友好错误信息
- **解构错误**：解构模式与实际值不匹配时的错误
- **控制流错误**：在 for 循环外使用 break/continue 的错误

## 8. 测试策略

### 8.1 单元测试
```rust
// core/src/stmt_test.rs 中添加
#[cfg(test)]
mod for_loop_tests {
    use super::*;

    #[test]
    fn test_for_loop_simple_list() {
        let program = r#"
            let sum = 0;
            for x in [1, 2, 3] {
                sum = sum + x;
            }
            return sum;
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new())).unwrap();
        assert_eq!(result, Val::Int(6));
    }

    #[test]
    fn test_for_loop_range() {
        let program = r#"
            let sum = 0;
            for i in 0..5 {
                sum = sum + i;
            }
            return sum;
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new())).unwrap();
        assert_eq!(result, Val::Int(10)); // 0+1+2+3+4
    }

    #[test]
    fn test_for_loop_tuple_destructure() {
        let program = r#"
            let keys = [];
            let values = [];
            for (k, v) in [("a", 1), ("b", 2)] {
                keys.push(k);
                values.push(v);
            }
            return [keys, values];
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new())).unwrap();
        // 应该返回 [["a", "b"], [1, 2]]
    }

    #[test]
    fn test_for_loop_array_destructure() {
        let program = r#"
            let firsts = [];
            for [first, ..] in [["a", "b", "c"], ["x", "y", "z"]] {
                firsts.push(first);
            }
            return firsts;
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new())).unwrap();
        // 应该返回 ["a", "x"]
    }

    #[test]
    fn test_for_loop_nested_pattern() {
        let program = r#"
            let results = [];
            for [name, (age, city)] in [["Alice", [25, "NYC"]], ["Bob", [30, "LA"]]] {
                results.push(name + " from " + city);
            }
            return results;
        "#;
        // 验证嵌套解构结果...
    }

    #[test]
    fn test_for_loop_ignore_pattern() {
        let program = r#"
            let count = 0;
            for _ in [1, 2, 3, 4, 5] {
                count = count + 1;
            }
            return count;
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new())).unwrap();
        assert_eq!(result, Val::Int(5));
    }

    #[test]
    fn test_for_loop_pattern_mismatch() {
        let program = r#"
            for [a, b] in [[1, 2, 3], [4, 5]] { // 长度不匹配
                // 应该在第一次迭代时报错
            }
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new()));
        assert!(result.is_err()); // 应该因为模式不匹配而失败
    }

    #[test]
    fn test_for_loop_break_continue() {
        let program = r#"
            let result = [];
            for i in 0..10 {
                if i == 3 { continue; }
                if i == 7 { break; }
                result.push(i);
            }
            return result;
        "#;
        let result = execute_program(program, &Val::Map(HashMap::new())).unwrap();
        // 应该返回 [0, 1, 2, 4, 5, 6]
    }
}
```

### 8.2 集成测试
在 `tests/integration_test.rs` 中添加复杂场景测试：
- 嵌套循环
- 在函数中使用 for 循环
- 与 import 系统结合使用迭代器函数
- 错误恢复和边界情况

## 9. 文档和工具支持

### 9.1 语言规范更新
在 `docs/language-reference.md` 中添加：
- For 循环语法规范
- 迭代器协议说明
- 内置迭代器类型
- 解构赋值规则

### 9.2 LSP 支持
在 `lsp/src/analyzer.rs` 中添加：
- For 循环语法高亮
- 循环变量的 hover 信息
- 作用域内变量补全
- 迭代器类型推断

### 9.3 VS Code 扩展
在 `vscode-qcl/` 中添加：
- `for` 关键字语法高亮
- 代码片段模板
- 错误诊断显示

## 10. 性能考虑

### 10.1 迭代器优化
- **惰性求值**：仅在需要时生成迭代器元素
- **内存管理**：避免为大型序列创建完整的中间 Vec
- **缓存机制**：重复使用的范围表达式结果缓存

### 10.2 编译时优化
- **常量折叠**：编译时确定的范围表达式预计算
- **循环展开**：小范围循环的优化展开
- **死代码消除**：永不执行的循环体检测

## 11. 实现里程碑

### M1: 基础语法解析 (1-2 天)
- [ ] 添加 `For` token 和词法识别
- [ ] 实现 `ForPattern` AST 节点
- [ ] 实现基础的 for 循环解析器
- [ ] 单元测试覆盖解析逻辑

### M2: 执行引擎核心 (2-3 天)  
- [ ] 实现 `create_iterator` 函数
- [ ] 基本迭代器支持（List, Map, String）
- [ ] 循环执行逻辑和控制流处理
- [ ] 变量绑定和解构赋值

### M3: 范围表达式 (1-2 天)
- [ ] 添加 `Range` token 和解析
- [ ] 实现范围表达式求值
- [ ] 支持包含式和排他式范围

### M4: 标准库和工具 (1-2 天)
- [ ] 实现迭代器工具函数 (enumerate, range)
- [ ] LSP 支持和错误诊断
- [ ] VS Code 语法高亮更新

### M5: 测试和文档 (1 天)
- [ ] 完整测试套件
- [ ] 文档更新
- [ ] 示例程序和最佳实践

## 12. 未来扩展方向

### 12.1 高级迭代器特性
- **生成器函数**：`yield` 关键字支持
- **无限序列**：惰性无限迭代器
- **并行迭代**：与并发特性结合的并行 for 循环

### 12.2 函数式编程特性
- **链式操作**：`map`, `filter`, `reduce` 等方法
- **迭代器组合器**：`zip`, `chain`, `take`, `skip` 等
- **管道操作符**：`|>` 用于函数组合

### 12.3 模式匹配扩展
- **嵌套解构**：支持更复杂的解构模式
- **守卫条件**：`for item in list if item > 0`
- **类型模式**：基于类型的模式匹配
