[English](README.md) | 中文

<div align="center">
    <h2>QCL</h2>
    <h5>一种用于检查 query 求值结果的语言。</h5>
</div>

## 简介

QCL 主要面向 ACL (Access Control List) 场景，用来判断某个用户是否有权访问某个资源。

## 表达式语言

- 字面量：string、int、float、bool、nil、list、map
- 支持嵌套 list/map，以及尾随逗号
- 支持注释：`//` 和可嵌套的 `/* ... */`
- 支持 `@path` 上下文访问，包含带引号的字段名、整数下标、负数下标、计算得到的路径段，以及嵌套 `@` 访问
- 任意主表达式都可以继续做后缀访问：`.field` / `.index`
- 支持运算符：`!`、一元 `-`、`+ - * / %`、`== != < > <= >=`、`in`、`&&`、`||`、`??`、`?:`
- `in` 支持字符串子串、list 成员/子集匹配、map 键查找
- 访问缺失时返回 `nil`
- `@` 外的裸标识符按字符串值处理，`@` 内则按字段名处理
- 默认算术在能精确表示时保留整数除法，不能精确表示时提升为浮点数
- 可选的高级算术扩展支持字符串、list 和 map 的 `+` / `-`

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

## 能力

### 输入格式

- 默认构建支持 JSON 输入
- YAML 和 TOML 作为可选输入后端提供
- 当对应后端已编译进来时，可通过 `--json`、`--yaml`、`--toml` 强制选择解析格式

### CLI

- 从 `stdin` 读取上下文，从 argv 读取表达式
- `--check` / `-c` 在结果为真时退出码为 `0`，为假时退出码为 `1`
- `--ast` 直接输出解析后的 AST
- `--version` / `-V` 输出版本号
- `--help` / `-h` 输出帮助信息

### Rust API

- 使用 `Expr::try_from` 或缓存版 `Expr::parse_cached_arc` 解析表达式
- 使用 `Expr::eval` 求值
- 使用 `requested_ctx` 获取表达式依赖的上下文名
- 使用 `is_ctx_independent` 判断表达式是否不依赖上下文

### 绑定

- 通过 `wasm-bindgen` 提供 WASM 绑定（[文档](docs/wasm.md)）
- 提供 C 共享库（[文档](docs/ffi.md)）
- 通过 PyO3 提供 Python 模块（[文档](docs/python.md)）
- 在禁用 `std` 时，库仍可在 `no_std` + `alloc` 下使用

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
