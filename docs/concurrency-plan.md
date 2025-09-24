# 并发支持计划

## 1. 目标与约束
- 引入受 Go 启发的轻量级并发原语（任务、通道、select），保持语言的一致语义风格与可读性。
- 新特性默认关闭；启用后对旧代码零影响，且不会改变顺序求值的确定性保证。
- 遵循现有模块边界：语法/类型检查在 `core`，运行时支持在 `core`，辅助 API 在 `stdlib`，工具链在 `cli`、`lsp` 与 `vscode-qcl`。
- 避免引入 `unsafe`；若必须使用底层并发结构，需单独评审并附带基准。

### 成功判定
- 至少三个示例程序展示 fan-in、超时、错误传播场景，运行结果稳定重复。
- `cargo test --workspace` 与 `cargo clippy --workspace --all-features -D warnings` 在启用并发特性后均通过。
- LSP 在典型误用场景（类型不匹配、关闭后发送、未消费的任务）提供明确诊断。
- CLI 支持在 REPL 退出前等待所有后台任务或提示未完成任务。

### 里程碑（初稿）
- M1：语言规范草案 + 类型系统设计讨论稿（含示例和开放问题）。
- M2：AST/类型检查实现 + 最小求值器原型 + 基础单元测试。
- M3：调度器 & 通道运行时完成，标准库辅助 API 就绪。
- M4：工具链、文档、示例与发布准备完成。

### 外部依赖
- 引入 `tokio` 作为多线程运行时，提供 Go 风格的并发原语支持；许可证兼容（MIT），二进制体积增长可接受。
- 使用 `crossbeam` 提供高性能的并发数据结构（通道、队列）。
- 与 VS Code 扩展的自动补全模块同步迭代；需要前端团队配合。

## 2. 语言表面设计
### 2.1 任务原语
- 内建函数 `spawn(expr)`：在逻辑上启动独立求值任务，返回 `Task<T>` 句柄。
- 支持结构化并发：句柄提供 `await`、`detach`、`cancel` 等方法（方法最终由 `stdlib` 暴露）。
- 语法片段：`Expr ::= "spawn" "(" Expr ")"`。

### 2.2 通道类型
- 新类型构造：`chan(capacity: Int?, type: Type)` 返回 `channel<Type>` 值。
- 通道值具备所有权语义：可复制（若元素可复制且容量有限制），否则移动。
- 关闭语义：`close(channel)` 在 `stdlib` 中实现，关闭后 `recv` 返回 `(false, default)`。
- 示例：
  ```qcl
  let numbers = chan(0, int);
  let producer = spawn {
    for n in 0..10 { send(numbers, n); }
    close(numbers);
  };
  ```

### 2.3 发送 / 接收
- `send(channel, value)` 阻塞当前任务直到成功或通道关闭；返回布尔表示是否成功。
- `recv(channel)` 阻塞等待消息，返回 `(ok: bool, value: T)`；通道关闭时返回 `(false, default(T))`。
- 推导语法：`Expr ::= IDENT "(" ExprList? ")"` 与现有调用合并，无需新增运算符。

### 2.4 Select 构造
- 结构：`select { case pattern => expr; ...; default => expr }`，pattern 允许 `value <- recv(channel)` 或 `send(channel, expr)`。
- 分支体在命中后返回结果，所有分支类型需统一。
- `default` 分支当所有候选阻塞时执行，确保可选超时/取消流程。
- 示例：
  ```qcl
  select {
    case (true, value) <= recv(ch) => handle(value);
    case _ <= recv(cancel) => abort();
    default => log("no work");
  }
  ```

### 2.5 取消与超时
- 借助 `stdlib::time::after(duration)` 返回一次性触发的通道，用于 `select` 超时分支。
- 任务取消通过向专用通道发送信号实现，语言层面不提供关键字，保持语义简单。

### 2.6 兼容性与开放问题
- 确认 `select` 语法与现有块表达式冲突情况（尤其是缩进/换行规则）。
- 评估是否允许 `spawn { ... }` 块语法糖，或保持函数调用形式保证解析简单。
- 讨论 `Task<T>` 的 `Drop` 行为：默认在作用域结束时是否等待、取消或 detach。

## 3. 类型系统与静态分析
### 3.1 AST 节点
- 新增节点：`Expr::Spawn`, `Expr::ChanLiteral`, `Expr::Send`, `Expr::Recv`, `Expr::Select`。
- `SelectCase` 结构包含 `pattern`, `guard`, `body`，为类型推断提供上下文。

### 3.2 类型规则
- `spawn(expr)`：若 `expr: T`，则结果类型 `Task<T>`。
- `chan(cap, ty)`：要求 `cap` 为 `Int` 或 `Nil`，返回 `Channel<ty>`。
- `send(channel, value)`：需满足 `channel: Channel<T>` 且 `value: T` 可赋值；返回 `Bool`。
- `recv(channel)`：返回 `Tuple<Bool, T>` 或自定义 `RecvResult<T>` 枚举（待确认）。
- `select`：所有分支返回值统一；若某分支无返回值则整个表达式类型为 `Unit`。

### 3.3 推断与借用规则
- 通道默认是可变引用语义；在类型推断中要求 `send` 持有可变借用，`recv` 为可变或不可变（依据实现）。
- `Task<T>` 的内部值默认是 `Send` + `'static` 风格约束（若未来支持多线程），当前阶段可不强制。
- 调整穷尽性检查以识别 `select` 中重复的通道操作并提示死锁风险。

### 3.4 静态分析增强
- 新增 lint：检测未消费的 `Task`（创建后未 `await/recv`），提示潜在资源泄漏。
- 通道容量静态检查：常量容量为 0 时提醒阻塞风险（可作为 clippy lint）。
- 在类型错误信息中提供修复建议（例如“考虑使用 `chan(cap, type)` 明确类型”）。

## 4. 运行时与求值器
### 4.1 调度器
- Go 风格多线程实现：基于 `tokio` 运行时，支持工作窃取调度器，提供真正的并行执行。
- 任务调度：每个 `spawn` 创建独立的 `tokio::task`，支持抢占式调度和跨线程执行。
- 兼容性：提供单线程模式用于测试，通过 `tokio::runtime::Builder::new_current_thread()` 确保可重复性。
- 调度策略：默认多线程，可通过环境变量 `QCL_SINGLE_THREAD=1` 切换到单线程模式。

### 4.2 任务生命周期  
- 任务结构体：封装 `tokio::task::JoinHandle<Result<Val>>`，提供 Go 风格的并发原语。
- 跨线程安全：任务间共享数据使用 `Arc<Mutex<T>>` 或 `Arc<RwLock<T>>`，确保线程安全。
- `Task::await`：异步等待任务完成，使用 `tokio::task::JoinHandle::await`。
- `Task::cancel`：使用 `tokio::task::JoinHandle::abort()` 实现任务取消。
- Goroutine 语义：每个任务运行在独立的 OS 线程上，支持真正的并行执行。

### 4.3 通道实现
- Go 风格通道：使用 `tokio::sync::mpsc` 和 `crossbeam-channel` 实现高性能多生产者多消费者通道。
- 无缓冲通道：`chan(0, type)` 映射到 `tokio::sync::mpsc::unbounded_channel()`。
- 有缓冲通道：`chan(n, type)` 映射到 `tokio::sync::mpsc::channel(n)`。
- `send` 流程：异步发送，支持背压和流控，使用 `Sender::send().await`。
- `recv` 流程：异步接收，支持优雅关闭，使用 `Receiver::recv().await`。
- `close` 语义：丢弃发送端，所有接收者收到 `None` 表示通道关闭。
- 跨线程安全：通道天然支持跨线程传递，无需额外同步。

### 4.4 性能与可观测性
- Tokio 指标集成：使用 `tokio-metrics` 监控任务调度、线程池使用情况。
- 通道指标：统计发送/接收速率、缓冲区使用率、等待时间分布。
- CPU 亲和性：支持绑定工作线程到特定 CPU 核心，优化 NUMA 性能。
- 内存管理：使用 `tokio` 的内存池减少分配开销，支持大规模并发场景。
- 基准测试：对比 Go goroutines 性能，确保在相似负载下性能可比。

## 5. 标准库与内建函数
- `task` 模块：`await`, `try_await`, `join_all`, `map`, `is_done`, `detach`, `spawn_blocking`（CPU 密集任务）。
- `chan` 模块：`close`, `len`, `capacity`, `is_closed`, `select!`（宏风格 select）。
- `time` 模块：`sleep(duration)`, `timeout(duration, task)`, `interval(duration)` 基于 `tokio::time`。
- `sync` 模块：`Mutex`, `RwLock`, `Barrier`, `Semaphore` 等同步原语。
- 错误类型：定义 `TaskError`、`ChannelError`，集成 `tokio::task::JoinError`。
- Feature flag：新增 `concurrency`，默认开启；支持 `tokio-rt-multi-thread` 和 `tokio-rt-current-thread`。

## 6. 工具与用户体验
### 6.1 CLI (`qcl-cli`)
- REPL 在用户输入结束时等待未完成任务，超时后提示使用 `task::detach`。
- `--expr` 模式允许显式传入并发表达式；输出格式提示任务状态。

### 6.2 LSP (`qcl-lsp`)
- 类型错误高亮：通道类型不匹配、select 无效分支、未关闭通道警告。
- Completion：提供 `spawn`, `chan`, `select` 模板，自动补全 `case` 关键字与占位符。
- Hover：详细展示 `Task<T>` 和 `Channel<T>` 描述，包括常见用法和链接到文档。

### 6.3 VS Code 扩展
- 语法高亮新增 `select`、`spawn`。
- 代码片段：`select` 模板生成基本分支结构。
- 命令面板动作：一键运行并发示例集合。

### 6.4 调试与日志
- 为 CLI 与 LSP 增加 `--concurrency-trace` 诊断标志，输出调度事件日志。
- 运行时支持可选的 `TRACE` 级日志，帮助定位死锁。

## 7. 测试策略
- 解析/语法测试：`core/src/parser*_test.rs` 覆盖 `spawn`、`select` 正常与错误路径。
- 类型系统测试：验证 `Task<T>` 推断、通道类型匹配、非法关闭等。
- 运行时测试：使用虚拟调度器验证阻塞/唤醒顺序、select 公平性、取消传播。
- 标准库测试：`stdlib/src/*_test.rs` 检查 `task::join_all`、`time::after` 与通道工具函数。
- 集成测试：
  - CLI：在 `tests/` 添加脚本运行 `qcl-cli` 并校验输出。
  - LSP：`lsp/tests/` 新增并发诊断案例。
- CI 增强：增加并发特性构建矩阵、运行 `cargo miri`（若可行）确保内存安全。

## 8. 文档与发布
- 更新 `docs/language-reference.md`：新增并发章节、提供语法、示例、最佳实践与常见陷阱。
- 在 `docs/design-notes/` 记录调度器设计与选择过程，便于未来维护。
- `examples/concurrency/`：添加 `pipeline.qcl`、`fan_in.qcl`、`timeout.qcl` 等示例，并在 README 中解释运行方式。
- 发布说明：在 `docs/changelog.md` 撰写条目，列出新特性、向后兼容声明与迁移建议。

## 9. 实现现状与进度

### 已完成 (✅)
- **架构设计**: 完成从单线程协作式到多线程抢占式的架构转换
- **AST 节点**: 实现所有并发相关的 AST 节点 (`Expr::Spawn`, `Expr::ChanLiteral`, `Expr::Send`, `Expr::Recv`)
- **解析器支持**: 添加对 spawn, chan, send, recv 语法的完整解析支持
- **类型系统**: 扩展类型系统支持 `Task<T>` 和 `Channel<T>` 类型
- **tokio 运行时**: 完成基于 tokio 的多线程运行时调度器和任务管理系统
- **通道操作**: 实现基于 `tokio::sync::mpsc` 的发送、接收和关闭操作
- **依赖配置**: 更新 Cargo.toml 配置，添加 tokio 和 crossbeam 依赖
- **特性标志**: 配置 concurrency 相关的 feature flags 和构建选项

### 进行中 (🚧)
- **Select 语句**: 正在实现 select 语句的模式匹配和分支选择逻辑
- **错误处理**: 完善并发场景下的错误传播和资源清理机制

### 待完成 (📋)
- **标准库模块**: 实现 `task`, `chan`, `time`, `sync` 等标准库并发模块
- **测试覆盖**: 添加全面的并发功能单元测试和集成测试
- **CLI 集成**: 更新 CLI 工具以支持 tokio 运行时的并发执行
- **LSP 支持**: 扩展语言服务器协议以支持并发语法的诊断和补全
- **性能优化**: 实现通道缓冲区优化、任务调度优化和内存池机制
- **文档更新**: 完善语言参考文档中的并发章节
- **示例程序**: 创建 pipeline、fan-in、超时等典型并发模式的示例

### 技术债务与改进点 (⚠️)
- **类型安全**: 通道的类型擦除使用 `Any`，需要更优雅的类型安全机制
- **资源管理**: 任务和通道的生命周期管理需要进一步优化
- **错误消息**: 并发相关的错误消息需要更用户友好的表述
- **调试支持**: 需要添加并发任务的调试和观测能力

### 下一步工作优先级
1. **高优先级**: 完成 Select 语句实现，这是 Go 风格并发的核心特性
2. **中优先级**: 实现标准库并发模块，提供完整的 API 接口
3. **低优先级**: CLI 和 LSP 工具链集成，提升开发体验

### 里程碑状态
- **M1** (语言规范草案): ✅ 已完成
- **M2** (AST/类型检查实现): ✅ 已完成  
- **M3** (调度器 & 通道运行时): 🚧 进行中 (80% 完成)
- **M4** (工具链、文档、示例): 📋 待开始
