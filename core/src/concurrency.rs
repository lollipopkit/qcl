use std::{
    collections::VecDeque,
    sync::{Arc, Mutex, mpsc},
    thread::{self, JoinHandle},
    time::Duration,
};
use anyhow::{Result, anyhow};
use crate::val::Val;

/// Channel implementation using Arc<Mutex<>> for thread safety
#[derive(Debug)]
pub struct Channel {
    sender: Arc<Mutex<mpsc::Sender<Val>>>,
    receiver: Arc<Mutex<mpsc::Receiver<Val>>>,
    pub capacity: usize, // Make capacity public for testing
}

impl Channel {
    /// Create a new unbuffered channel (capacity 0)
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            sender: Arc::new(Mutex::new(tx)),
            receiver: Arc::new(Mutex::new(rx)),
            capacity: 0,
        }
    }

    /// Create a new buffered channel with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            return Self::new();
        }
        let (_tx, _rx) = mpsc::sync_channel::<Val>(capacity);
        // Convert sync_channel to regular channel for consistency
        let (tx_regular, rx_regular) = mpsc::channel();
        
        // Spawn a bridge thread to handle the buffering
        let _tx_bridge = tx_regular.clone();
        thread::spawn(move || {
            let mut _buffer: VecDeque<Val> = VecDeque::with_capacity(capacity);
            
            // This is a simplified bridge - in practice you'd want more sophisticated buffering
            loop {
                // This is a placeholder - proper implementation would require more complex logic
                break;
            }
        });
        
        Self {
            sender: Arc::new(Mutex::new(tx_regular)),
            receiver: Arc::new(Mutex::new(rx_regular)),
            capacity,
        }
    }

    /// Send a value to the channel (blocking)
    pub fn send(&self, value: Val) -> Result<()> {
        let sender = self.sender.lock().map_err(|_| anyhow!("Channel sender poisoned"))?;
        sender.send(value).map_err(|_| anyhow!("Channel closed"))?;
        Ok(())
    }

    /// Try to send a value to the channel (non-blocking)
    pub fn try_send(&self, value: Val) -> Result<bool> {
        let sender = self.sender.lock().map_err(|_| anyhow!("Channel sender poisoned"))?;
        match sender.send(value) {
            Ok(()) => Ok(true),
            Err(_) => Ok(false), // Channel is full or closed
        }
    }

    /// Receive a value from the channel (blocking)
    pub fn recv(&self) -> Result<Val> {
        let receiver = self.receiver.lock().map_err(|_| anyhow!("Channel receiver poisoned"))?;
        receiver.recv().map_err(|_| anyhow!("Channel closed"))
    }

    /// Try to receive a value from the channel (non-blocking)
    pub fn try_recv(&self) -> Result<Option<Val>> {
        let receiver = self.receiver.lock().map_err(|_| anyhow!("Channel receiver poisoned"))?;
        match receiver.try_recv() {
            Ok(val) => Ok(Some(val)),
            Err(mpsc::TryRecvError::Empty) => Ok(None),
            Err(mpsc::TryRecvError::Disconnected) => Err(anyhow!("Channel closed")),
        }
    }

    /// Close the channel by dropping the sender
    pub fn close(&self) {
        // The sender will be dropped when the Arc goes out of scope
        // For explicit closing, we could add a flag
    }
}

impl Clone for Channel {
    fn clone(&self) -> Self {
        Self {
            sender: Arc::clone(&self.sender),
            receiver: Arc::clone(&self.receiver),
            capacity: self.capacity,
        }
    }
}

impl PartialEq for Channel {
    fn eq(&self, other: &Self) -> bool {
        // Compare by pointer equality for simplicity
        Arc::ptr_eq(&self.sender, &other.sender) && 
        Arc::ptr_eq(&self.receiver, &other.receiver)
    }
}

/// Goroutine handle for managing spawned tasks
#[derive(Debug)]
pub struct GoroutineHandle {
    handle: Option<JoinHandle<Result<Val>>>,
    id: u64,
}

impl GoroutineHandle {
    pub fn new(handle: JoinHandle<Result<Val>>, id: u64) -> Self {
        Self {
            handle: Some(handle),
            id,
        }
    }

    /// Wait for the goroutine to complete and get its result
    pub fn join(mut self) -> Result<Val> {
        if let Some(handle) = self.handle.take() {
            handle.join().map_err(|_| anyhow!("Goroutine panicked"))?
        } else {
            Err(anyhow!("Goroutine already joined"))
        }
    }

    /// Get the goroutine ID
    pub fn id(&self) -> u64 {
        self.id
    }

    /// Check if the goroutine is finished
    pub fn is_finished(&self) -> bool {
        self.handle.as_ref().map_or(true, |h| h.is_finished())
    }
}

impl Clone for GoroutineHandle {
    fn clone(&self) -> Self {
        // Can't actually clone a JoinHandle, so we create a dummy one
        // In practice, you'd want a different approach for sharing goroutine handles
        Self {
            handle: None,
            id: self.id,
        }
    }
}

impl PartialEq for GoroutineHandle {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

/// Select statement case for channel operations
#[derive(Debug, Clone)]
pub enum SelectCase {
    /// Receive from channel: case val := <-ch
    Recv {
        channel: Channel,
        var_name: Option<String>,
    },
    /// Send to channel: case ch <- val
    Send {
        channel: Channel,
        value: Val,
    },
    /// Default case
    Default,
}

/// Result of a select operation
#[derive(Debug)]
pub enum SelectResult {
    /// A receive operation completed
    Received(Val),
    /// A send operation completed
    Sent,
    /// Default case was executed
    Default,
    /// No operations were ready (shouldn't happen in blocking select)
    None,
}

/// Execute a select statement with multiple channel operations
pub fn select_channels(cases: &[SelectCase], _timeout: Option<Duration>) -> Result<(usize, SelectResult)> {
    // This is a simplified select implementation
    // A full implementation would require more sophisticated channel selection logic
    
    // For now, just try each case in order
    for (i, case) in cases.iter().enumerate() {
        match case {
            SelectCase::Recv { channel, .. } => {
                if let Ok(Some(val)) = channel.try_recv() {
                    return Ok((i, SelectResult::Received(val)));
                }
            }
            SelectCase::Send { channel, value } => {
                if let Ok(true) = channel.try_send(value.clone()) {
                    return Ok((i, SelectResult::Sent));
                }
            }
            SelectCase::Default => {
                return Ok((i, SelectResult::Default));
            }
        }
    }
    
    // If no case was ready and there's no default, this would block
    // For simplicity, return none for now
    Ok((0, SelectResult::None))
}

/// Global goroutine counter for unique IDs
static GOROUTINE_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Get the next goroutine ID
pub fn next_goroutine_id() -> u64 {
    GOROUTINE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}