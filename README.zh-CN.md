中文 | [English](README.md)

<div align="center">
    <h2>QCL</h2>
    <h5>用于检查查询求值结果的简单语言</h5>
</div>

## 简介

QCL 设计用于 ACL（访问控制列表）等场景，用表达式来判断用户是否有权访问某个资源。

### 示例

```rust
fn user_can_access() {
    return @record.owner == @req.user.id
        || @record.published;
}
fn admin_can_access() {
    return @req.user.role == 'admin' || @req.user.id in @record.granted;
}
return user_can_access() || admin_can_access();
```

说明：

- `@req.user.role == 'admin'`：判断用户角色是否为 `admin`。
- `@req.user.id in @record.granted`：判断用户 ID 是否在记录的 `granted` 列表中。
- `@record.published`：判断记录是否已公开。
- `@record.owner == @req.user.id`：判断记录的所有者是否为该用户。

以上示例展示了一个简化的访问控制判断。

更多语言细节： [LANG_zh.md](LANG_zh.md) · [LANG.md](LANG.md)

## 特性

- `json`（默认启用）
- `yaml`
- `toml`

至少需要启用一种输入格式特性；默认启用 `json`。

### 用法

#### 集成

```rust
// 解析表达式
let expr = "@req.user.name in 'foobar' && @files.0.published == true";
let expr = Expr::try_from(expr)?;

// 构造上下文（从表达式可推断所需键）
let ctx_names = expr.requested_ctx(); // ["req", "files"]
// 实际构造可自行完成，这里用 json! 简化
let ctx = json!({
    "req": {
        "user": "foo"
    },
    "files": [
        {
            "name": "file1",
            "published": true
        }
    ]
});

// 求值
let result = expr.eval(ctx.into())?; // Val::Bool(true)
match result {
    Val::Bool(b) => {
        assert!(b);
    }
    _ => {
        panic!("unexpected result");
    }
}
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
echo '{"req":{"user":{"id":1}}}' | cargo run -p qcl-cli -- --expr "@req.user.id == 1"
```

## 许可证

```plaintext
Apache-2.0 lollipopkit
```
