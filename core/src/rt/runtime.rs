//! Runtime scheduler and task management system
//!
//! Provides Go-style concurrency primitives using tokio for multithreading support.

#[cfg(feature = "concurrency")]
use anyhow::{Result, anyhow};
#[cfg(feature = "concurrency")]
use std::collections::HashMap;
#[cfg(feature = "concurrency")]
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(feature = "concurrency")]
use std::sync::{Arc, Mutex};
#[cfg(feature = "concurrency")]
use tokio::sync::{Mutex as TokioMutex, mpsc};
#[cfg(feature = "concurrency")]
use tokio::task::JoinHandle;

#[cfg(feature = "concurrency")]
use crate::val::Val;

/// Task handle for spawned concurrent tasks
#[cfg(feature = "concurrency")]
#[derive(Debug)]
pub struct Task {
    pub id: u64,
    pub handle: JoinHandle<Result<Val>>,
    pub result: Option<Result<Val>>,
}

/// Channel sender wrapper to handle both bounded and unbounded channels
#[cfg(feature = "concurrency")]
#[derive(Debug)]
pub enum ChannelSender {
    Bounded(mpsc::Sender<Val>),
    Unbounded(mpsc::UnboundedSender<Val>),
}

#[cfg(feature = "concurrency")]
impl ChannelSender {
    fn clone_sender(&self) -> Self {
        match self {
            ChannelSender::Bounded(sender) => ChannelSender::Bounded(sender.clone()),
            ChannelSender::Unbounded(sender) => ChannelSender::Unbounded(sender.clone()),
        }
    }
}

/// Channel receiver wrapper to handle both bounded and unbounded channels
#[cfg(feature = "concurrency")]
#[derive(Debug)]
pub enum ChannelReceiver {
    Bounded(mpsc::Receiver<Val>),
    Unbounded(mpsc::UnboundedReceiver<Val>),
}

/// Channel for inter-task communication
#[cfg(feature = "concurrency")]
#[derive(Debug)]
pub struct Channel {
    pub id: u64,
    pub capacity: Option<usize>,
    pub sender: ChannelSender,
    pub receiver: Arc<TokioMutex<ChannelReceiver>>,
    pub closed: Arc<AtomicBool>,
}

/// Runtime scheduler managing concurrent tasks and channels
#[cfg(feature = "concurrency")]
#[derive(Debug)]
pub struct Runtime {
    tasks: Arc<Mutex<HashMap<u64, Task>>>,
    channels: Arc<Mutex<HashMap<u64, Channel>>>,
    next_task_id: Arc<Mutex<u64>>,
    next_channel_id: Arc<Mutex<u64>>,
    tokio_runtime: tokio::runtime::Runtime,
}

#[cfg(feature = "concurrency")]
impl Runtime {
    /// Create a new multi-threaded runtime
    pub fn new_multi_thread() -> Result<Self> {
        let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .map_err(|e| anyhow!("Failed to create tokio runtime: {}", e))?;

        Ok(Runtime {
            tasks: Arc::new(Mutex::new(HashMap::new())),
            channels: Arc::new(Mutex::new(HashMap::new())),
            next_task_id: Arc::new(Mutex::new(1)),
            next_channel_id: Arc::new(Mutex::new(1)),
            tokio_runtime,
        })
    }

    /// Create a new current-thread runtime for testing
    pub fn new_current_thread() -> Result<Self> {
        let tokio_runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| anyhow!("Failed to create tokio runtime: {}", e))?;

        Ok(Runtime {
            tasks: Arc::new(Mutex::new(HashMap::new())),
            channels: Arc::new(Mutex::new(HashMap::new())),
            next_task_id: Arc::new(Mutex::new(1)),
            next_channel_id: Arc::new(Mutex::new(1)),
            tokio_runtime,
        })
    }

    /// Spawn a new task
    pub fn spawn<F>(&self, future: F) -> Result<u64>
    where
        F: std::future::Future<Output = Result<Val>> + Send + 'static,
    {
        let task_id = {
            let mut next_id = self.next_task_id.lock().unwrap();
            let id = *next_id;
            *next_id += 1;
            id
        };

        let handle = self.tokio_runtime.spawn(future);

        let task = Task {
            id: task_id,
            handle,
            result: None,
        };

        let mut tasks = self.tasks.lock().unwrap();
        tasks.insert(task_id, task);

        Ok(task_id)
    }

    /// Create a new channel
    pub fn create_channel(&self, capacity: Option<usize>) -> Result<u64> {
        let channel_id = {
            let mut next_id = self.next_channel_id.lock().unwrap();
            let id = *next_id;
            *next_id += 1;
            id
        };

        let channel = if let Some(cap) = capacity {
            let (sender, receiver) = mpsc::channel::<Val>(cap);
            Channel {
                id: channel_id,
                capacity: Some(cap),
                sender: ChannelSender::Bounded(sender),
                receiver: Arc::new(TokioMutex::new(ChannelReceiver::Bounded(receiver))),
                closed: Arc::new(AtomicBool::new(false)),
            }
        } else {
            let (sender, receiver) = mpsc::unbounded_channel::<Val>();
            Channel {
                id: channel_id,
                capacity: None,
                sender: ChannelSender::Unbounded(sender),
                receiver: Arc::new(TokioMutex::new(ChannelReceiver::Unbounded(receiver))),
                closed: Arc::new(AtomicBool::new(false)),
            }
        };

        let mut channels = self.channels.lock().unwrap();
        channels.insert(channel_id, channel);

        Ok(channel_id)
    }

    /// Attempt to send a value without blocking.
    pub fn try_send(&self, channel_id: u64, value: Val) -> Result<bool> {
        let (sender, closed_flag) = {
            let channels = self.channels.lock().unwrap();
            let channel = channels.get(&channel_id).ok_or_else(|| anyhow!("Channel not found"))?;
            (channel.sender.clone_sender(), channel.closed.clone())
        };

        match sender {
            ChannelSender::Bounded(sender) => match sender.try_send(value) {
                Ok(()) => Ok(true),
                Err(mpsc::error::TrySendError::Full(_)) => Ok(false),
                Err(mpsc::error::TrySendError::Closed(_)) => {
                    closed_flag.store(true, Ordering::SeqCst);
                    Err(anyhow!("Channel is closed"))
                }
            },
            ChannelSender::Unbounded(sender) => match sender.send(value) {
                Ok(()) => Ok(true),
                Err(_) => {
                    closed_flag.store(true, Ordering::SeqCst);
                    Err(anyhow!("Channel is closed"))
                }
            },
        }
    }

    /// Send a value to a channel, awaiting until it can be delivered or the channel closes.
    pub async fn send_async(&self, channel_id: u64, value: Val) -> Result<bool> {
        let (sender, closed_flag) = {
            let channels = self.channels.lock().unwrap();
            let channel = channels.get(&channel_id).ok_or_else(|| anyhow!("Channel not found"))?;
            (channel.sender.clone_sender(), channel.closed.clone())
        };

        match sender {
            ChannelSender::Bounded(sender) => match sender.send(value).await {
                Ok(()) => Ok(true),
                Err(_) => {
                    closed_flag.store(true, Ordering::SeqCst);
                    Ok(false)
                }
            },
            ChannelSender::Unbounded(sender) => match sender.send(value) {
                Ok(()) => Ok(true),
                Err(_) => {
                    closed_flag.store(true, Ordering::SeqCst);
                    Ok(false)
                }
            },
        }
    }

    /// Try to receive a value without blocking. Returns None if no value is ready.
    pub fn try_recv(&self, channel_id: u64) -> Result<Option<(bool, Val)>> {
        let (receiver_arc, closed_flag) = {
            let channels = self.channels.lock().unwrap();
            let channel = channels.get(&channel_id).ok_or_else(|| anyhow!("Channel not found"))?;
            (channel.receiver.clone(), channel.closed.clone())
        };

        let mut receiver = match receiver_arc.try_lock() {
            Ok(guard) => guard,
            Err(_) => return Ok(None),
        };

        let result = match &mut *receiver {
            ChannelReceiver::Bounded(recv) => match recv.try_recv() {
                Ok(value) => Some((true, value)),
                Err(mpsc::error::TryRecvError::Empty) => {
                    if closed_flag.load(Ordering::SeqCst) {
                        Some((false, Val::Nil))
                    } else {
                        None
                    }
                }
                Err(mpsc::error::TryRecvError::Disconnected) => {
                    closed_flag.store(true, Ordering::SeqCst);
                    Some((false, Val::Nil))
                }
            },
            ChannelReceiver::Unbounded(recv) => match recv.try_recv() {
                Ok(value) => Some((true, value)),
                Err(mpsc::error::TryRecvError::Empty) => {
                    if closed_flag.load(Ordering::SeqCst) {
                        Some((false, Val::Nil))
                    } else {
                        None
                    }
                }
                Err(mpsc::error::TryRecvError::Disconnected) => {
                    closed_flag.store(true, Ordering::SeqCst);
                    Some((false, Val::Nil))
                }
            },
        };

        Ok(result)
    }

    /// Receive a value from a channel, waiting until a value is available or the channel closes.
    pub async fn recv_async(&self, channel_id: u64) -> Result<(bool, Val)> {
        let (receiver_arc, closed_flag) = {
            let channels = self.channels.lock().unwrap();
            let channel = channels.get(&channel_id).ok_or_else(|| anyhow!("Channel not found"))?;
            (channel.receiver.clone(), channel.closed.clone())
        };

        let mut receiver = receiver_arc.lock().await;
        let value = match &mut *receiver {
            ChannelReceiver::Bounded(recv) => recv.recv().await,
            ChannelReceiver::Unbounded(recv) => recv.recv().await,
        };

        match value {
            Some(value) => Ok((true, value)),
            None => {
                closed_flag.store(true, Ordering::SeqCst);
                Ok((false, Val::Nil))
            }
        }
    }

    /// Close a channel
    pub fn close_channel(&self, channel_id: u64) -> Result<()> {
        let mut channels = self.channels.lock().unwrap();
        if let Some(channel) = channels.remove(&channel_id) {
            channel.closed.store(true, Ordering::SeqCst);
            Ok(())
        } else {
            Err(anyhow!("Channel not found"))
        }
    }

    /// Wait for a task to complete
    pub async fn join_task(&self, task_id: u64) -> Result<Val> {
        let mut task = {
            let mut tasks = self.tasks.lock().unwrap();
            tasks.remove(&task_id).ok_or_else(|| anyhow!("Task not found"))?
        };

        // If result is already available, return it
        if let Some(result) = task.result.take() {
            return result;
        }

        // Otherwise await the handle
        match task.handle.await {
            Ok(result) => result,
            Err(e) => Err(anyhow!("Task failed: {}", e)),
        }
    }

    /// Cancel a task
    pub fn cancel_task(&self, task_id: u64) -> Result<()> {
        let mut tasks = self.tasks.lock().unwrap();

        if let Some(task) = tasks.remove(&task_id) {
            task.handle.abort();
            Ok(())
        } else {
            Err(anyhow!("Task not found"))
        }
    }

    /// Get runtime statistics
    pub fn stats(&self) -> RuntimeStats {
        let tasks = self.tasks.lock().unwrap();
        let channels = self.channels.lock().unwrap();

        RuntimeStats {
            active_tasks: tasks.len(),
            active_channels: channels.len(),
            is_multi_threaded: matches!(
                self.tokio_runtime.handle().runtime_flavor(),
                tokio::runtime::RuntimeFlavor::MultiThread
            ),
        }
    }

    /// Block on a future using the tokio runtime
    pub fn block_on<F>(&self, future: F) -> F::Output
    where
        F: std::future::Future,
    {
        self.tokio_runtime.block_on(future)
    }
}

/// Runtime statistics
#[cfg(feature = "concurrency")]
#[derive(Debug, Clone)]
pub struct RuntimeStats {
    pub active_tasks: usize,
    pub active_channels: usize,
    pub is_multi_threaded: bool,
}

/// Global runtime instance
#[cfg(feature = "concurrency")]
use once_cell::sync::Lazy;

#[cfg(feature = "concurrency")]
static GLOBAL_RUNTIME: Lazy<Arc<Mutex<Option<Runtime>>>> = Lazy::new(|| Arc::new(Mutex::new(None)));

/// Initialize the global runtime
#[cfg(feature = "concurrency")]
pub fn init_runtime() -> Result<()> {
    let mut runtime = GLOBAL_RUNTIME.lock().unwrap();
    if runtime.is_none() {
        let rt = if std::env::var("QCL_SINGLE_THREAD").is_ok() {
            Runtime::new_current_thread()?
        } else {
            Runtime::new_multi_thread()?
        };
        *runtime = Some(rt);
    }
    Ok(())
}

/// Get a reference to the global runtime, initializing if needed
#[cfg(feature = "concurrency")]
pub fn with_runtime<F, R>(f: F) -> Result<R>
where
    F: FnOnce(&Runtime) -> Result<R>,
{
    let mut runtime_guard = GLOBAL_RUNTIME.lock().unwrap();

    if runtime_guard.is_none() {
        // Initialize runtime automatically
        let rt = if std::env::var("QCL_SINGLE_THREAD").is_ok() {
            Runtime::new_current_thread()?
        } else {
            Runtime::new_multi_thread()?
        };
        *runtime_guard = Some(rt);
    }

    if let Some(runtime) = runtime_guard.as_ref() {
        f(runtime)
    } else {
        Err(anyhow!("Runtime initialization failed"))
    }
}

/// Select operation result
#[cfg(feature = "concurrency")]
#[derive(Debug, Clone)]
pub struct SelectResult {
    pub case_index: Option<usize>,
    pub recv_payload: Option<(bool, Val)>,
    pub is_default: bool,
}

#[cfg(feature = "concurrency")]
#[derive(Debug, Clone)]
pub enum SelectArm {
    Recv {
        case_index: usize,
        channel_id: u64,
    },
    Send {
        case_index: usize,
        channel_id: u64,
        value: Val,
    },
}

/// Select operation for multiple channel operations
#[cfg(feature = "concurrency")]
pub struct SelectOperation {
    arms: Vec<SelectArm>,
}

#[cfg(feature = "concurrency")]
impl Default for SelectOperation {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "concurrency")]
impl SelectOperation {
    pub fn new() -> Self {
        Self { arms: Vec::new() }
    }

    pub fn add_recv(&mut self, case_index: usize, channel_id: u64) {
        self.arms.push(SelectArm::Recv { case_index, channel_id });
    }

    pub fn add_send(&mut self, case_index: usize, channel_id: u64, value: Val) {
        self.arms.push(SelectArm::Send {
            case_index,
            channel_id,
            value,
        });
    }

    pub fn is_empty(&self) -> bool {
        self.arms.is_empty()
    }

    /// Execute select operation and return the first available operation
    pub async fn execute(&self, runtime: &Runtime, has_default: bool) -> Result<SelectResult> {
        // Try fast path first to avoid awaiting when an operation is ready
        for arm in &self.arms {
            match arm {
                SelectArm::Recv { case_index, channel_id } => {
                    if let Some((ok, value)) = runtime.try_recv(*channel_id)? {
                        return Ok(SelectResult {
                            case_index: Some(*case_index),
                            recv_payload: Some((ok, value)),
                            is_default: false,
                        });
                    }
                }
                SelectArm::Send {
                    case_index,
                    channel_id,
                    value,
                } => match runtime.try_send(*channel_id, value.clone()) {
                    Ok(true) => {
                        return Ok(SelectResult {
                            case_index: Some(*case_index),
                            recv_payload: None,
                            is_default: false,
                        });
                    }
                    Ok(false) => {}
                    Err(e) => return Err(e),
                },
            }
        }

        if has_default {
            return Ok(SelectResult {
                case_index: None,
                recv_payload: None,
                is_default: true,
            });
        }

        if self.arms.is_empty() {
            return Ok(SelectResult {
                case_index: None,
                recv_payload: None,
                is_default: true,
            });
        }

        use futures::future::select_all;
        use std::pin::Pin;

        let mut futures: Vec<Pin<Box<dyn futures::Future<Output = Result<SelectResult>> + Send>>> =
            Vec::with_capacity(self.arms.len());

        for arm in &self.arms {
            match arm.clone() {
                SelectArm::Recv { case_index, channel_id } => {
                    let fut = async move {
                        let (ok, value) = runtime.recv_async(channel_id).await?;
                        Ok(SelectResult {
                            case_index: Some(case_index),
                            recv_payload: Some((ok, value)),
                            is_default: false,
                        })
                    };
                    futures.push(Box::pin(fut));
                }
                SelectArm::Send {
                    case_index,
                    channel_id,
                    value,
                } => {
                    let fut = async move {
                        let sent = runtime.send_async(channel_id, value).await?;
                        if sent {
                            Ok(SelectResult {
                                case_index: Some(case_index),
                                recv_payload: None,
                                is_default: false,
                            })
                        } else {
                            Err(anyhow!("Channel closed during send"))
                        }
                    };
                    futures.push(Box::pin(fut));
                }
            }
        }

        let (result, _index, _remaining) = select_all(futures).await;
        result
    }
}

/// Shutdown the global runtime
#[cfg(feature = "concurrency")]
pub fn shutdown_runtime() {
    let mut runtime = GLOBAL_RUNTIME.lock().unwrap();
    *runtime = None;
}

// Stub implementations for when concurrency feature is disabled
#[cfg(not(feature = "concurrency"))]
pub fn init_runtime() -> anyhow::Result<()> {
    Ok(())
}

#[cfg(not(feature = "concurrency"))]
pub fn shutdown_runtime() {
    // No-op when concurrency is disabled
}
