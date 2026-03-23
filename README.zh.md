<div align="center">
    <h2>QCL</h2>
    <h5>一种用于检查 query 求值结果的简洁语言。</h5>
</div>

中文版。英文版见 [README.md](README.md)。

## 简介

QCL 主要面向 ACL (Access Control List) 场景，用来判断某个用户是否有权访问某个资源。

### 示例

```js
(@record.published || @record.owner == @req.user.id) // 普通情况
|| // 或运算符
(@req.user.role == 'admin' || @req.user.id in @record.granted) // 特殊情况
```

可以拆成下面几部分：

- `@req.user.role == 'admin'`: 检查用户角色是否为 `admin`。
- `@req.user.id in @record.granted`: 检查用户 id 是否在记录的 `granted` 列表中。
- `@record.published`: 检查记录是否已发布。
- `@record.owner == @req.user.id`: 检查记录所有者是否为当前用户。

上面的表达式演示了一个简单的 ACL 规则：判断用户是否有权访问一条记录。

更完整的语言说明见 [LANG.zh.md](LANG.zh.md)。

## 特性

默认启用：
- `json` — JSON 输入格式
- `sem_arith` — 语义整数除法（`3 / 2 = 1.5`）
- `std` — 表达式缓存和反序列化

可选：
- `yaml` — YAML 输入格式
- `toml` — TOML 输入格式
- `adv_arith` — 高级算术（`List + List`、`Map + Map` 等）

绑定：
- `wasm` — 通过 wasm-bindgen 的 WebAssembly 绑定（[文档](docs/wasm.md)）
- `ffi` — C 共享库（[文档](docs/ffi.md)）
- `python` — 通过 PyO3 的 Python 模块（[文档](docs/python.md)）

库还支持 `no_std`（需要 `alloc`），在禁用 `std` 时可用。

### 用法

#### 集成

```rust
// 解析表达式
let expr = "@req.user.name in 'foobar' && @files.0.published == true";
let expr = Expr::try_from(expr)?;

// 构造上下文
let ctx_names = expr.requested_ctx(); // ["req", "files"]
// 当然也可以自己构造上下文，这里为了简洁使用 json!
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

```bash
echo '{"req": {"user": {"role": "admin"}}}' | cargo run -- '@req.user.role == "admin"'

# 检查模式（退出码 0/1）
echo '{"x": 1}' | cargo run -- --check '@x == 1' && echo ok

# 打印 AST
echo '{}' | cargo run -- --ast '1 + 2'
```

#### 构建绑定

```bash
make wasm    # WASM 包      → pkg/
make ffi     # C 共享库      → target/release/libqcl.so
make python  # Python 模块   → 安装到当前 venv
```

详细的绑定文档见 [docs/](docs/)。

## 相关项目

- [Golang SDK / CLI](https://github.com/lollipopkit/gqcl)

## 许可证

```plaintext
Apache-2.0 lollipopkit
```
