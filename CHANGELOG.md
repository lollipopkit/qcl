# Changelog

## [0.1.5] - 2026-03-23

相较 `0.1.4`，`0.1.5` 主要带来语法扩展、CLI 增强、绑定补齐和一轮性能优化。

### 新增

- 三元表达式：`cond ? true_expr : false_expr`
- 空值合并：`expr ?? fallback`
- 整数字面量支持十六进制 `0x...` / `0X...` 和八进制 `0o...` / `0O...`
- 字符串支持 Unicode 转义 `\uXXXX`
- 列表支持负索引，`-1` 表示最后一个元素
- 支持嵌套块注释 `/* ... */`
- `in` 现在支持 map 的 key 成员判断
- 新增 `--check`、`--ast`、`--help`、`--version` 以及 `--json` / `--yaml` / `--toml` CLI 选项
- 新增 WebAssembly、C FFI、Python 绑定及对应文档

### 变更

- 一元 `-` 现在可用于 `Int` 和 `Float`
- `??` 与三元表达式都支持明确的优先级与结合性
- 默认特性调整为包含 `std`，以提供表达式缓存和格式化反序列化支持
- `std` 关闭时可在 `alloc` 下使用 `no_std`

### 优化

- 解析缓存从 `VecDeque` LRU 改为基于时间戳的淘汰策略
- `in` 的 `HashSet` 阈值下调，提升常见成员判断的性能
- 解析阶段对部分表达式进行常量折叠
- tokenizer 错误信息改为按需计算行列号，降低额外开销

### 文档

- 更新 `LANG.md` 和 `LANG.zh.md`，补充新语法与文法
- 更新 `README.md` 和 `README.zh.md`，同步 CLI 与功能说明

