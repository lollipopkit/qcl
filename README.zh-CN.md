中文 | [English](README.md)

<div align="center">
    <h2>RLL</h2>
    <h5>使用 Rust 编写的，类似 Rust 的脚本语言</h5>
</div>

## 简介

### 示例（语句）

更多语言细节： [LANG_zh.md](LANG_zh.md)

## 特性

### 用法

#### 集成（库）

```rust
use qcl_core::{expr::Expr, stmt::Environment, val::Val};

// 解析表达式
let expr_src = "data.req.user.name in 'foobar' && data.files.0.published == true";
let expr = Expr::try_from(expr_src)?;

// 通过词法环境提供变量
let mut env = Environment::new();
let data_val: Val = serde_json::json!({
    "req": { "user": { "name": "foo" } },
    "files": [ { "name": "file1", "published": true } ]
}).into();
env.define("data".to_string(), data_val);

// 求值
let result = expr.eval_with_env(Some(&env))?; // Val::Bool(true)
assert_eq!(result, Val::Bool(true));
```

## 许可证

```plaintext
Apache-2.0 lollipopkit
```
