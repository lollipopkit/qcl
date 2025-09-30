中文 | [English](README.md)

<div align="center">
    <h2>QCL</h2>
    <h5>用于检查查询求值结果的简单语言</h5>
</div>

## 简介

QCL 设计用于 ACL（访问控制列表）等场景，用表达式来判断用户是否有权访问某个资源。

### 示例（语句）

```rust
import io;
import json;
let data = json.parse(io.read());

fn user_can_access(record, req) {
    return record.owner == req.user.id || record.published;
}
fn admin_can_access(req, record) {
    return req.user.role == 'admin' || req.user.id in record.granted;
}
return user_can_access(data.record, data.req) || admin_can_access(data.req, data.record);
```

说明：

- `req.user.role == 'admin'`：判断用户角色是否为 `admin`。
- `req.user.id in record.granted`：判断用户 ID 是否在记录的 `granted` 列表中。
- `record.published`：判断记录是否已公开。
- `record.owner == req.user.id`：判断记录的所有者是否为该用户。

以上示例展示了一个简化的访问控制判断。

更多语言细节： [LANG_zh.md](LANG_zh.md) · [LANG.md](LANG.md)

## 特性

- `json`（默认启用）
- `yaml`
- `toml`

至少需要启用一种输入格式特性；默认启用 `json`。

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

#### CLI

<div height="100px" align="center">
    <img src="https://cdn.lpkt.cn/img/capture/qcl.png" alt="QCL" />
    
</div>

文件导入安全与解析：
- 仅允许相对且已净化的路径（不允许 `..`，不允许绝对路径）。
- 导入文件时按顺序尝试 `${MOD_NAME}.qcl` 与 `${MOD_NAME}/mod.qcl`（相对于当前工作目录）。若传入已带 `.qcl` 的相对路径，将在存在时直接使用。

- 输出行为：CLI 与 REPL 仅在结果非 `nil` 时打印输出；如需输出 `nil`，请使用 `println(...)` 显式打印。

示例（在项目根目录运行）：

```bash
echo '{"req":{"user":{"id":1}}}' | cargo run -p qcl-cli -- --stmt -- "import io; import json; let d = json.parse(io.read()); return d.req.user.id == 1;"
```

## 许可证

```plaintext
Apache-2.0 lollipopkit
```
