# QCL 性能优化方案（参考 Lua 设计）

本文档描述三项面向高频路径的结构性优化方案，并给出可落地的实现步骤、数据结构设计与迁移策略。三项优化相互独立、可渐进交付；两两配合时能形成叠加收益。

- 轻量函数调用帧（避免环境深拷贝）
- 寄存器字节码 VM（减少解释器分派和装箱/拆箱）
- 名称解析到槽位（移除运行期 HashMap 查找）

> 备注：当前已落地的两点优化（数值 for 快路径、作用域开销削减/快速环境表）已显著改善 `examples/fib.qcl` 的热点，但函数调用与变量查找仍是主热点。本方案聚焦进一步消除这两类开销。

---

## 1) 轻量函数调用帧（避免环境深拷贝）

目标：调用 `Val::Closure::call` 时不再克隆整份 `Environment`，而是以“栈帧/父链”的方式创建轻量帧，仅为本地变量/参数分配连续的槽位向量。闭包通过 upvalue 捕获外层变量引用（而非复制值）。

### 设计要点（对标 Lua Upvalue 模型）
- 引入帧结构 `EnvFrame`：
  - `parent: Option<Rc<EnvFrame>>`
  - `locals: Vec<Val>`（或 `Vec<Rc<RefCell<Val>>>` 用于被闭包捕获的可变 upvalue）
- 将 `Environment` 重构为对当前帧的轻量包装，并保留现有 API 以兼容解释器路径：
  - `Environment { current: Rc<EnvFrame>, /* resolver/import/type_checker 仍旧在此 */ }`
  - 继续支持旧的 `get/assign`，但为“未解析变量”或“动态绑定”兜底；优化路径将直接走槽位访问（见第三节）。
- 闭包捕获（Upvalue）：
  - 在函数定义时分析自由变量，记录其相对帧深度与槽位索引；
  - 调用创建闭包时，将这些槽位封装为 `Upvalue`（`Rc<RefCell<Val>>`）存入闭包对象；
  - 闭包体访问自由变量时通过 upvalue 直接读写。

### 最小可行实现（MVP）
1. 新增数据结构（新文件建议）：（已完成）
   - `core/src/rt/frame.rs`：`EnvFrame`、`Upvalue`、`FrameSlot` 等
   - 在 `core/src/val/values.rs` 的 `Val::Closure` 中增加对 upvalues 的存储：
     ```rust
     Closure {
       params: Arc<Vec<String>>,
       body: Arc<stmt::Stmt>,
       env: Arc<stmt::Environment>,
       // 新增：捕获的 upvalues（编译期填充，运行期传递）
       upvalues: Arc<Vec<Upvalue>>,
    }
    ```
2. 函数调用路径修改：（已完成）
   - 现状改造：`Val::call` 不再克隆整份 `Environment`，改为：
     - 通过 `Environment::shallow_call_env(cap)` 仅克隆全局作用域并复用 resolver/import/type_checker；
     - 预先为参数分配容量的作用域并绑定参数；
     - 调用时创建/释放 `EnvFrame` 父链帧（`push_call_frame`/`pop_frame` 已接入）；
     - 执行函数体后丢弃该轻量环境。
   - 后续可演进为基于 `EnvFrame` 的全链表全局复用（结合槽位解析可彻底移除 HashMap 路径）。
3. 兼容解释执行：
   - 在解释器路径（`Expr::Var/Stmt::Let/Assign` 等）仍可通过现有 `get/assign` 落后兼容；
   - 后续配合“名称解析到槽位”实现，即可把这些节点替换为直接槽位访问（见第三节）。

### 代码片段（示意）
```rust
pub struct EnvFrame {
    pub parent: Option<std::rc::Rc<EnvFrame>>,
    pub locals: Vec<Val>,
}

impl Environment {
    pub fn push_call_frame(&mut self, nlocals: usize) {
        let new_frame = EnvFrame { parent: Some(self.current.clone()), locals: vec![Val::Nil; nlocals] };
        self.current = std::rc::Rc::new(new_frame);
    }
    pub fn pop_frame(&mut self) { /* 恢复 self.current = parent */ }
}
```

### 注意事项
- 递归与可重入：帧以 `Rc` 串联，不涉及深拷贝；
- 逃逸/闭包：被捕获的局部由 `Rc<RefCell<Val>>` 持有；
- 线程安全：如后续需要在并发特性下共享 upvalue，应换用 `Arc<Mutex<Val>>` 或设计“不可变 + 写时拷贝”。

---

## 2) 寄存器字节码 VM

目标：将 AST 编译为简洁的寄存器字节码，使用连续的寄存器数组执行，降低解释器分派与临时 `Val` 分配。

### 指令集草案（示意）
- 数据/载入：`LOADK rX, const_i`，`MOVE rX, rY`，`GETFIELD rX, rY, key`，`SETFIELD rX, rY, key`
- 算术/比较：`ADD rX, rA, rB`，`SUB`，`MUL`，`DIV`，`MOD`，`EQ`，`LT`，`LE`
- 逻辑跳转：`JMP ofs`，`JMP_FALSE rC, ofs`
- 调用：`CALL rFunc, rBase, argc, retc`，`RET rBase, retc`
- 范围循环（数值 for 的专用指令）：`FORPREP rIdx, rLimit, step`，`FORLOOP rIdx, rLimit, step, ofs`

### 模块划分
- `core/src/vm/bytecode.rs`：常量池、指令编码（可选紧凑 u32 编码）
- `core/src/vm/compiler.rs`：AST → Bytecode（一次分配寄存器，DFS 编译）
- `core/src/vm/vm.rs`：执行循环（`loop { match opcode { .. } }`），寄存器向量复用（小函数使用 stack-alloc 或 `smallvec`）

### 编译策略
- 表达式按子树分配目标寄存器，尽量原地就地计算减少 `Val` 克隆；
- `for a in 2..=n` 编译为 `FORPREP/FORLOOP`，无需构造列表；
- 函数调用：
  - 形参与临时值占用连续寄存器区间；
  - 闭包捕获通过 upvalue 表访问（与第 1 节一致）。

### 与解释器并存
- 在 `Expr::eval`/`Stmt::execute` 前新增一个选择：
  - 体量小或频繁执行的函数/程序，编译为 Bytecode 后执行；
  - 其余沿用解释执行；
- 可通过 feature/环境变量控制启用 VM：`qcl-core/vm`。（最小接入已完成：常量表达式在启用 `feature=vm` 且设置 `QCL_VM_LITE` 环境变量时走 VM）

### 示例执行循环（简化）
```rust
loop {
    match decode(opcode) {
        Op::LoadK(dst, k) => regs[dst] = consts[k].clone(),
        Op::Add(dst, a, b) => regs[dst] = (&regs[a] + &regs[b])?,
        Op::Jmp(ofs) => pc = ((pc as isize) + ofs as isize) as usize,
        Op::JmpFalse(r, ofs) => if is_falsey(&regs[r]) { pc = jump(pc, ofs) },
        Op::ForPrep(idx, limit, step) => { /* 预处理 */ }
        Op::ForLoop(idx, limit, step, ofs) => { /* 递增/边界检测/跳转 */ }
        Op::Call(f, base, argc, retc) => { /* 建立子帧/参数传递/返回 */ }
        Op::Ret(base, retc) => break,
    }
}
```

### 注意事项
- 错误定位：编译时记录指令→源 Span 的映射，运行时出错回溯原文位置；
- 常量折叠/内联：可在编译阶段进行轻量优化（常量表达式、短路）；
- 兼容现有类型系统/特性开关（并发、算术语义等）。

---

## 3) 名称解析到槽位（移除运行期 HashMap 查找）

目标：在编译/解析阶段完成作用域分析与槽位分配，将变量读写从 `env.get("x")/assign("x")` 转为 `env.get_slot(depth, index)` 直接寻址，彻底避开 HashMap 查找与字符串分配。

### 作用域与槽位分配
- 新增解析/解义（resolver）遍历（可复用/扩展现有 `TypeChecker` 的作用域栈）：
  - 进入作用域时为新声明的变量分配递增 index；
  - 记录 `name -> (depth, index)`；
  - 函数/块退出时弹栈；
- 对闭包的自由变量，记录其 `(depth, index)` 以便转成 upvalue（见第 1 节）。

### AST 注解
- 将 `Expr::Var(String)` 拓展为 `Expr::Var { name: String, slot: Option<VarSlot> }`：
  - `VarSlot { depth: u16, index: u16 }`（或更紧凑类型）
  - 解析完成后尽可能填充 `slot`；找不到（如动态导入符号）则保留 `None`，运行期回退到原有 HashMap 查找
- `Stmt::Let/Assign/Function` 等同理，持有槽位信息以支持快速写入

### 运行期支持
- 为 `Environment`/`EnvFrame` 增加槽位 API：（已完成：运行期脚手架，返回/设置克隆值）
  ```rust
  fn get_slot(&self, depth: u16, index: u16) -> Option<Val>
  fn set_slot(&mut self, depth: u16, index: u16, val: Val) -> Result<()>
  ```
- 闭包捕获的槽位在创建闭包时封装为 upvalue；闭包体通过 upvalue 访问而非爬父链

### 渐进式落地
1. 第一步：仅为函数参数与局部 `let` 分配槽位；全局/导入符号仍走 HashMap（风险小、收益高）——已完成（运行期路径：动态分配槽位并在求值时优先读取槽位）
2. 第二步：为 `for`/`if let` 等解构模式生成临时槽位，再按需写回（已完成：作用域 push/pop 同步 slot 映射，绑定经 `define` 自动落槽）
3. 第三步：对方法调用、匹配表达式等复杂场景补完（已完成：`Expr::Call`/复合赋值/`while let` 前缀推进等路径优先使用槽位读取；VM 侧初步支持 And/Or 逻辑短路的字节码化）

### 边界与注意事项
- 遮蔽与重名：槽位分配基于作用域栈天然处理遮蔽；
- 模式匹配绑定：可先使用 HashMap 生成中间绑定表，再落槽，保持实现简单；
- 诊断信息：保留 `name` 以便报错信息友好（即便已用槽位访问）。

---

## 集成与验证

- 代码组织建议：
  - `core/src/rt/frame.rs`（帧与 upvalue）
  - `core/src/vm/{bytecode.rs,compiler.rs,vm.rs}`（VM 子系统）（已完成）
  - `core/src/resolve/slots.rs`（名称解析与槽位分配）（已完成）
- 特性开关：
  - `qcl-core` 新增 `vm`/`slots` feature，默认保持解释器路径可用（已完成）
- 基准与对比：
  - 现有：`cargo bench -p qcl-core`
  - 端到端：
    - `time cargo run -p qcl-cli -- examples/fib.qcl`
    - 对比 `lua examples/fib.lua`、`dart examples/fib.dart`

## 里程碑建议

1. MVP：轻量调用帧（不含 upvalue 写时共享，捕获值先按复制语义处理）
2. 槽位解析（参数/局部）+ 调用帧整合，去除函数体中的 HashMap 查找
3. 数值 for/逻辑短路等热点的字节码化；VM 与解释器并行可选
4. 完整 upvalue 语义（引用捕获），补齐闭包与递归场景
5. 基于基准结果做指令/寄存器分配微调

---

如需我在代码中起步搭建第 1 步的 `EnvFrame` 骨架与 `Val::Closure` 结构调整，可在一个小分支内增量提交，并附带针对 `fib.qcl` 的对比基准。
