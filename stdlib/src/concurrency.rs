use anyhow::{Result, anyhow};
use qcl_core::{
    concurrency::Channel,
    stmt::Environment,
    val::Val,
};

/// Create a new channel with optional capacity
/// make_chan() - unbuffered channel
/// make_chan(size) - buffered channel with given capacity
pub fn make_chan(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
    match args.len() {
        0 => {
            // Unbuffered channel
            let channel = Channel::new();
            Ok(Val::Channel(channel))
        }
        1 => {
            // Buffered channel
            if let Val::Int(capacity) = &args[0] {
                if *capacity < 0 {
                    return Err(anyhow!("Channel capacity cannot be negative"));
                }
                let channel = Channel::with_capacity(*capacity as usize);
                Ok(Val::Channel(channel))
            } else {
                Err(anyhow!(
                    "Channel capacity must be an integer, got {}",
                    args[0].type_name()
                ))
            }
        }
        _ => Err(anyhow!(
            "make_chan expects 0 or 1 arguments, got {}",
            args.len()
        )),
    }
}

/// Send a value to a channel (blocking)
/// send(channel, value) -> nil
pub fn send(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("send expects 2 arguments, got {}", args.len()));
    }

    if let Val::Channel(channel) = &args[0] {
        channel.send(args[1].clone())?;
        Ok(Val::Nil)
    } else {
        Err(anyhow!(
            "First argument to send must be a channel, got {}",
            args[0].type_name()
        ))
    }
}

/// Receive a value from a channel (blocking)
/// recv(channel) -> value
pub fn recv(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("recv expects 1 argument, got {}", args.len()));
    }

    if let Val::Channel(channel) = &args[0] {
        let value = channel.recv()?;
        Ok(value)
    } else {
        Err(anyhow!(
            "Argument to recv must be a channel, got {}",
            args[0].type_name()
        ))
    }
}

/// Try to send a value to a channel (non-blocking)
/// try_send(channel, value) -> bool (true if sent, false if channel full/closed)
pub fn try_send(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 2 {
        return Err(anyhow!("try_send expects 2 arguments, got {}", args.len()));
    }

    if let Val::Channel(channel) = &args[0] {
        let success = channel.try_send(args[1].clone())?;
        Ok(Val::Bool(success))
    } else {
        Err(anyhow!(
            "First argument to try_send must be a channel, got {}",
            args[0].type_name()
        ))
    }
}

/// Try to receive a value from a channel (non-blocking)
/// try_recv(channel) -> [value, ok] where ok is true if value received
pub fn try_recv(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("try_recv expects 1 argument, got {}", args.len()));
    }

    if let Val::Channel(channel) = &args[0] {
        match channel.try_recv()? {
            Some(value) => {
                // Return [value, true]
                Ok(Val::List(std::sync::Arc::new(vec![value, Val::Bool(true)])))
            }
            None => {
                // Return [nil, false]
                Ok(Val::List(std::sync::Arc::new(vec![Val::Nil, Val::Bool(false)])))
            }
        }
    } else {
        Err(anyhow!(
            "Argument to try_recv must be a channel, got {}",
            args[0].type_name()
        ))
    }
}

/// Close a channel
/// close(channel) -> nil
pub fn close(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
    if args.len() != 1 {
        return Err(anyhow!("close expects 1 argument, got {}", args.len()));
    }

    if let Val::Channel(channel) = &args[0] {
        channel.close();
        Ok(Val::Nil)
    } else {
        Err(anyhow!(
            "Argument to close must be a channel, got {}",
            args[0].type_name()
        ))
    }
}