use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::val::Val;

#[cfg(feature = "slots")]
type PreassignedByDepth = Vec<Vec<(String, u16)>>;

/// 轻量运行时栈帧（EnvFrame）与 Upvalue 骨架
///
/// 说明：为后续名称槽位访问提供父链帧；当前用于：
/// - 函数参数与局部 let 的槽位分配（动态）
/// - 按作用域栈维护 name->slot 映射，支持遮蔽
#[derive(Debug)]
pub struct EnvFrame {
    pub parent: Option<Arc<EnvFrame>>,
    /// 局部槽位（后续由 resolver 分配索引）；此处用 Mutex 以便运行期写入
    pub locals: Mutex<Vec<Val>>,
    /// 当前函数内的槽位作用域栈（与 Environment.scopes 对齐），用于 name->slot 映射
    pub slot_scopes: Mutex<Vec<HashMap<String, u16>>>,
    /// 下一个可写入的槽位索引（优先复用预分配区，超出后追加）
    pub next: Mutex<u16>,
    /// 预分配的槽位映射，按块深度分组（仅在 `slots` 特性启用时使用）。
    ///
    /// 索引 0 表示函数级（参数作用域，与 push_scope_with_capacity 预先建立的参数作用域对齐），
    /// 后续索引依次对应更深的块作用域。
    #[cfg(feature = "slots")]
    pub preassigned_by_depth: Mutex<Option<PreassignedByDepth>>,
}

#[derive(Debug, Clone)]
pub struct Upvalue(pub Arc<Mutex<Val>>);
