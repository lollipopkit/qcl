#[cfg(feature = "stdlib-tcp")]
use qcl_core::{
    module::Module, 
    val::Val, 
    stmt::Environment,
    concurrency::Channel
};
#[cfg(feature = "stdlib-tcp")]
use anyhow::Result;
#[cfg(feature = "stdlib-tcp")]
use std::collections::HashMap;
#[cfg(feature = "stdlib-tcp")]
use std::sync::{Arc, Mutex};
#[cfg(feature = "stdlib-tcp")]
use std::net::{TcpStream, TcpListener as StdTcpListener, SocketAddr, ToSocketAddrs};
#[cfg(feature = "stdlib-tcp")]
use std::io::{Read, Write};
#[cfg(feature = "stdlib-tcp")]
use std::time::Duration;
#[cfg(feature = "stdlib-tcp")]
use std::sync::OnceLock;
#[cfg(feature = "stdlib-tcp")]
use std::thread;

#[cfg(feature = "stdlib-tcp")]
static CONNECTION_STORE: OnceLock<Mutex<HashMap<String, Arc<Mutex<TcpStream>>>>> = OnceLock::new();

#[cfg(feature = "stdlib-tcp")]
static LISTENER_STORE: OnceLock<Mutex<HashMap<String, Arc<Mutex<StdTcpListener>>>>> = OnceLock::new();

#[cfg(feature = "stdlib-tcp")]
fn get_connection_store() -> &'static Mutex<HashMap<String, Arc<Mutex<TcpStream>>>> {
    CONNECTION_STORE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(feature = "stdlib-tcp")]
fn get_listener_store() -> &'static Mutex<HashMap<String, Arc<Mutex<StdTcpListener>>>> {
    LISTENER_STORE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(feature = "stdlib-tcp")]
fn generate_connection_id() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    format!("tcp_conn_{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

#[cfg(feature = "stdlib-tcp")]
#[derive(Debug)]
pub struct TcpModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-tcp")]
impl TcpModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Connection management with channels
        functions.insert("connect".to_string(), Val::RustFunction(Self::connect));
        functions.insert("connect_async".to_string(), Val::RustFunction(Self::connect_async));
        functions.insert("send".to_string(), Val::RustFunction(Self::send));
        functions.insert("recv".to_string(), Val::RustFunction(Self::recv));
        functions.insert("send_async".to_string(), Val::RustFunction(Self::send_async));
        functions.insert("recv_async".to_string(), Val::RustFunction(Self::recv_async));
        functions.insert("close".to_string(), Val::RustFunction(Self::close));
        functions.insert("set_timeout".to_string(), Val::RustFunction(Self::set_timeout));
        
        // Server management with channels
        functions.insert("bind".to_string(), Val::RustFunction(Self::bind));
        functions.insert("accept".to_string(), Val::RustFunction(Self::accept));
        functions.insert("accept_async".to_string(), Val::RustFunction(Self::accept_async));
        functions.insert("close_listener".to_string(), Val::RustFunction(Self::close_listener));
        
        // Stream handling
        functions.insert("read_stream".to_string(), Val::RustFunction(Self::read_stream));
        functions.insert("write_stream".to_string(), Val::RustFunction(Self::write_stream));
        
        // Utility functions
        functions.insert("resolve".to_string(), Val::RustFunction(Self::resolve));

        Self { functions }
    }

    /// Connect to a remote TCP server
    /// Usage: tcp.connect("127.0.0.1:8080") or tcp.connect("127.0.0.1", 8080)
    /// Returns: connection_id string for use with other functions
    fn connect(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() < 1 || args.len() > 2 {
            return Err(anyhow::anyhow!(
                "connect() takes 1 or 2 arguments: address [, port]"
            ));
        }

        let address = match &args[0] {
            Val::Str(addr) => &**addr,
            _ => return Err(anyhow::anyhow!("address must be a string")),
        };

        let socket_addr = if args.len() == 2 {
            let port = match &args[1] {
                Val::Int(p) => *p as u16,
                _ => return Err(anyhow::anyhow!("port must be an integer")),
            };
            format!("{}:{}", address, port)
        } else {
            address.to_string()
        };

        let addr: SocketAddr = socket_addr.parse()
            .map_err(|e| anyhow::anyhow!("invalid socket address: {}", e))?;

        match TcpStream::connect(addr) {
            Ok(stream) => {
                let conn_id = generate_connection_id();
                let store = get_connection_store();
                match store.lock() {
                    Ok(mut connections) => {
                        connections.insert(conn_id.clone(), Arc::new(Mutex::new(stream)));
                        Ok(Val::Str(conn_id.into()))
                    }
                    Err(e) => Err(anyhow::anyhow!("failed to acquire connection store lock: {}", e)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("connection failed: {}", e)),
        }
    }

    /// Send data to a TCP connection
    /// Usage: tcp.send(connection_id, data)
    /// Returns: number of bytes sent
    fn send(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("send() takes exactly 2 arguments: connection_id, data"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => &**id,
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let data = match &args[1] {
            Val::Str(s) => s.as_bytes(),
            _ => return Err(anyhow::anyhow!("data must be a string")),
        };

        let store = get_connection_store();
        match store.lock() {
            Ok(connections) => {
                match connections.get(conn_id) {
                    Some(stream_arc) => {
                        match stream_arc.lock() {
                            Ok(mut stream) => {
                                match stream.write_all(data) {
                                    Ok(_) => Ok(Val::Int(data.len() as i64)),
                                    Err(e) => Err(anyhow::anyhow!("send failed: {}", e)),
                                }
                            }
                            Err(e) => Err(anyhow::anyhow!("failed to acquire stream lock: {}", e)),
                        }
                    }
                    None => Err(anyhow::anyhow!("connection not found: {}", conn_id)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("failed to acquire connection store lock: {}", e)),
        }
    }

    /// Receive data from a TCP connection
    /// Usage: tcp.recv(connection_id [, buffer_size])
    /// Returns: received data as string
    fn recv(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() < 1 || args.len() > 2 {
            return Err(anyhow::anyhow!("recv() takes 1 or 2 arguments: connection_id [, buffer_size]"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => &**id,
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let buffer_size = if args.len() == 2 {
            match &args[1] {
                Val::Int(size) => *size as usize,
                _ => return Err(anyhow::anyhow!("buffer_size must be an integer")),
            }
        } else {
            1024
        };

        let store = get_connection_store();
        match store.lock() {
            Ok(connections) => {
                match connections.get(conn_id) {
                    Some(stream_arc) => {
                        match stream_arc.lock() {
                            Ok(mut stream) => {
                                let mut buffer = vec![0u8; buffer_size];
                                match stream.read(&mut buffer) {
                                    Ok(bytes_read) => {
                                        buffer.truncate(bytes_read);
                                        match String::from_utf8(buffer) {
                                            Ok(data) => Ok(Val::Str(data.into())),
                                            Err(_) => Ok(Val::Nil),
                                        }
                                    }
                                    Err(e) => Err(anyhow::anyhow!("recv failed: {}", e)),
                                }
                            }
                            Err(e) => Err(anyhow::anyhow!("failed to acquire stream lock: {}", e)),
                        }
                    }
                    None => Err(anyhow::anyhow!("connection not found: {}", conn_id)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("failed to acquire connection store lock: {}", e)),
        }
    }

    /// Close a TCP connection
    /// Usage: tcp.close(connection_id)
    /// Returns: true on success
    fn close(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("close() takes exactly 1 argument: connection_id"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => &**id,
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let store = get_connection_store();
        match store.lock() {
            Ok(mut connections) => {
                match connections.remove(conn_id) {
                    Some(_) => Ok(Val::Bool(true)),
                    None => Err(anyhow::anyhow!("connection not found: {}", conn_id)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("failed to acquire connection store lock: {}", e)),
        }
    }

    /// Set timeout for a TCP connection
    /// Usage: tcp.set_timeout(connection_id, timeout_ms)
    /// Returns: true on success
    fn set_timeout(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("set_timeout() takes exactly 2 arguments: connection_id, timeout_ms"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => &**id,
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let timeout_ms = match &args[1] {
            Val::Int(ms) => *ms as u64,
            _ => return Err(anyhow::anyhow!("timeout must be an integer (milliseconds)")),
        };

        let timeout = Duration::from_millis(timeout_ms);
        let store = get_connection_store();
        match store.lock() {
            Ok(connections) => {
                match connections.get(conn_id) {
                    Some(stream_arc) => {
                        match stream_arc.lock() {
                            Ok(stream) => {
                                match stream.set_read_timeout(Some(timeout)) {
                                    Ok(_) => match stream.set_write_timeout(Some(timeout)) {
                                        Ok(_) => Ok(Val::Bool(true)),
                                        Err(e) => Err(anyhow::anyhow!("failed to set write timeout: {}", e)),
                                    },
                                    Err(e) => Err(anyhow::anyhow!("failed to set read timeout: {}", e)),
                                }
                            }
                            Err(e) => Err(anyhow::anyhow!("failed to acquire stream lock: {}", e)),
                        }
                    }
                    None => Err(anyhow::anyhow!("connection not found: {}", conn_id)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("failed to acquire connection store lock: {}", e)),
        }
    }

    /// Bind to a local address to create a TCP listener
    /// Usage: tcp.bind("127.0.0.1:8080") or tcp.bind("127.0.0.1", 8080)
    /// Returns: listener_id string for use with accept/close_listener
    fn bind(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() < 1 || args.len() > 2 {
            return Err(anyhow::anyhow!("bind() takes 1 or 2 arguments: address [, port]"));
        }

        let address = match &args[0] {
            Val::Str(addr) => &**addr,
            _ => return Err(anyhow::anyhow!("address must be a string")),
        };

        let socket_addr = if args.len() == 2 {
            let port = match &args[1] {
                Val::Int(p) => *p as u16,
                _ => return Err(anyhow::anyhow!("port must be an integer")),
            };
            format!("{}:{}", address, port)
        } else {
            address.to_string()
        };

        let addr: SocketAddr = socket_addr.parse()
            .map_err(|e| anyhow::anyhow!("invalid socket address: {}", e))?;

        match StdTcpListener::bind(addr) {
            Ok(listener) => {
                let listener_id = generate_connection_id();
                let store = get_listener_store();
                match store.lock() {
                    Ok(mut listeners) => {
                        listeners.insert(listener_id.clone(), Arc::new(Mutex::new(listener)));
                        Ok(Val::Str(listener_id.into()))
                    }
                    Err(e) => Err(anyhow::anyhow!("failed to acquire listener store lock: {}", e)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("bind failed: {}", e)),
        }
    }

    /// Accept a connection from a TCP listener
    /// Usage: tcp.accept(listener_id)
    /// Returns: connection_id string for the accepted connection
    fn accept(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("accept() takes exactly 1 argument: listener_id"));
        }

        let listener_id = match &args[0] {
            Val::Str(id) => &**id,
            _ => return Err(anyhow::anyhow!("listener_id must be a string")),
        };

        let listener_store = get_listener_store();
        match listener_store.lock() {
            Ok(listeners) => {
                match listeners.get(listener_id) {
                    Some(listener_arc) => {
                        match listener_arc.lock() {
                            Ok(listener) => {
                                match listener.accept() {
                                    Ok((stream, _addr)) => {
                                        let conn_id = generate_connection_id();
                                        let conn_store = get_connection_store();
                                        match conn_store.lock() {
                                            Ok(mut connections) => {
                                                connections.insert(conn_id.clone(), Arc::new(Mutex::new(stream)));
                                                Ok(Val::Str(conn_id.into()))
                                            }
                                            Err(e) => Err(anyhow::anyhow!("failed to acquire connection store lock: {}", e)),
                                        }
                                    }
                                    Err(e) => Err(anyhow::anyhow!("accept failed: {}", e)),
                                }
                            }
                            Err(e) => Err(anyhow::anyhow!("failed to acquire listener lock: {}", e)),
                        }
                    }
                    None => Err(anyhow::anyhow!("listener not found: {}", listener_id)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("failed to acquire listener store lock: {}", e)),
        }
    }

    /// Close a TCP listener
    /// Usage: tcp.close_listener(listener_id)
    /// Returns: true on success
    fn close_listener(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("close_listener() takes exactly 1 argument: listener_id"));
        }

        let listener_id = match &args[0] {
            Val::Str(id) => &**id,
            _ => return Err(anyhow::anyhow!("listener_id must be a string")),
        };

        let store = get_listener_store();
        match store.lock() {
            Ok(mut listeners) => {
                match listeners.remove(listener_id) {
                    Some(_) => Ok(Val::Bool(true)),
                    None => Err(anyhow::anyhow!("listener not found: {}", listener_id)),
                }
            }
            Err(e) => Err(anyhow::anyhow!("failed to acquire listener store lock: {}", e)),
        }
    }

    /// Resolve hostname to IP address
    /// Usage: tcp.resolve("example.com")
    /// Returns: IP address string
    fn resolve(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("resolve() takes exactly 1 argument: hostname"));
        }

        let hostname = match &args[0] {
            Val::Str(name) => &**name,
            _ => return Err(anyhow::anyhow!("hostname must be a string")),
        };

        match format!("{}:0", hostname).to_socket_addrs() {
            Ok(mut addrs) => {
                if let Some(addr) = addrs.next() {
                    Ok(Val::Str(addr.ip().to_string().into()))
                } else {
                    Ok(Val::Nil)
                }
            }
            Err(e) => Err(anyhow::anyhow!("hostname resolution failed: {}", e)),
        }
    }

    /// Connect to a remote TCP server asynchronously using channels
    /// Usage: tcp.connect_async("127.0.0.1:8080") or tcp.connect_async("127.0.0.1", 8080)
    /// Returns: channel that will receive connection_id when ready
    fn connect_async(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() < 1 || args.len() > 2 {
            return Err(anyhow::anyhow!(
                "connect_async() takes 1 or 2 arguments: address [, port]"
            ));
        }

        let address = match &args[0] {
            Val::Str(addr) => addr.to_string(),
            _ => return Err(anyhow::anyhow!("address must be a string")),
        };

        let socket_addr = if args.len() == 2 {
            let port = match &args[1] {
                Val::Int(p) => *p as u16,
                _ => return Err(anyhow::anyhow!("port must be an integer")),
            };
            format!("{}:{}", address, port)
        } else {
            address
        };

        let result_channel = Channel::new();
        let result_ch_clone = result_channel.clone();
        
        thread::spawn(move || {
            let addr_result: Result<SocketAddr> = socket_addr.parse()
                .map_err(|e| anyhow::anyhow!("invalid socket address: {}", e));
                
            match addr_result {
                Ok(addr) => {
                    match TcpStream::connect(addr) {
                        Ok(stream) => {
                            let conn_id = generate_connection_id();
                            let store = get_connection_store();
                            match store.lock() {
                                Ok(mut connections) => {
                                    connections.insert(conn_id.clone(), Arc::new(Mutex::new(stream)));
                                    let _ = result_ch_clone.send(Val::Str(conn_id.into()));
                                }
                                Err(_) => {
                                    let _ = result_ch_clone.send(Val::Nil);
                                }
                            }
                        }
                        Err(_) => {
                            let _ = result_ch_clone.send(Val::Nil);
                        }
                    }
                }
                Err(_) => {
                    let _ = result_ch_clone.send(Val::Nil);
                }
            }
        });

        Ok(Val::Channel(result_channel))
    }

    /// Send data to a TCP connection asynchronously using channels
    /// Usage: tcp.send_async(connection_id, data)
    /// Returns: channel that will receive number of bytes sent when complete
    fn send_async(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("send_async() takes exactly 2 arguments: connection_id, data"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => id.to_string(),
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let data = match &args[1] {
            Val::Str(s) => s.as_bytes().to_vec(),
            _ => return Err(anyhow::anyhow!("data must be a string")),
        };

        let result_channel = Channel::new();
        let result_ch_clone = result_channel.clone();
        
        thread::spawn(move || {
            let store = get_connection_store();
            let result = match store.lock() {
                Ok(connections) => {
                    match connections.get(&conn_id) {
                        Some(stream_arc) => {
                            match stream_arc.lock() {
                                Ok(mut stream) => {
                                    match stream.write_all(&data) {
                                        Ok(_) => Val::Int(data.len() as i64),
                                        Err(_) => Val::Nil,
                                    }
                                }
                                Err(_) => Val::Nil,
                            }
                        }
                        None => Val::Nil,
                    }
                }
                Err(_) => Val::Nil,
            };
            
            let _ = result_ch_clone.send(result);
        });

        Ok(Val::Channel(result_channel))
    }

    /// Receive data from a TCP connection asynchronously using channels
    /// Usage: tcp.recv_async(connection_id [, buffer_size])
    /// Returns: channel that will receive data when available
    fn recv_async(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() < 1 || args.len() > 2 {
            return Err(anyhow::anyhow!("recv_async() takes 1 or 2 arguments: connection_id [, buffer_size]"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => id.to_string(),
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let buffer_size = if args.len() == 2 {
            match &args[1] {
                Val::Int(size) => *size as usize,
                _ => return Err(anyhow::anyhow!("buffer_size must be an integer")),
            }
        } else {
            1024
        };

        let result_channel = Channel::new();
        let result_ch_clone = result_channel.clone();
        
        thread::spawn(move || {
            let store = get_connection_store();
            let result = match store.lock() {
                Ok(connections) => {
                    match connections.get(&conn_id) {
                        Some(stream_arc) => {
                            match stream_arc.lock() {
                                Ok(mut stream) => {
                                    let mut buffer = vec![0u8; buffer_size];
                                    match stream.read(&mut buffer) {
                                        Ok(bytes_read) => {
                                            buffer.truncate(bytes_read);
                                            match String::from_utf8(buffer) {
                                                Ok(data) => Val::Str(data.into()),
                                                Err(_) => Val::Nil,
                                            }
                                        }
                                        Err(_) => Val::Nil,
                                    }
                                }
                                Err(_) => Val::Nil,
                            }
                        }
                        None => Val::Nil,
                    }
                }
                Err(_) => Val::Nil,
            };
            
            let _ = result_ch_clone.send(result);
        });

        Ok(Val::Channel(result_channel))
    }

    /// Accept connections asynchronously using channels
    /// Usage: tcp.accept_async(listener_id)
    /// Returns: channel that will receive connection_id when a connection is accepted
    fn accept_async(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("accept_async() takes exactly 1 argument: listener_id"));
        }

        let listener_id = match &args[0] {
            Val::Str(id) => id.to_string(),
            _ => return Err(anyhow::anyhow!("listener_id must be a string")),
        };

        let result_channel = Channel::new();
        let result_ch_clone = result_channel.clone();
        
        thread::spawn(move || {
            let listener_store = get_listener_store();
            let result = match listener_store.lock() {
                Ok(listeners) => {
                    match listeners.get(&listener_id) {
                        Some(listener_arc) => {
                            match listener_arc.lock() {
                                Ok(listener) => {
                                    match listener.accept() {
                                        Ok((stream, _addr)) => {
                                            let conn_id = generate_connection_id();
                                            let conn_store = get_connection_store();
                                            match conn_store.lock() {
                                                Ok(mut connections) => {
                                                    connections.insert(conn_id.clone(), Arc::new(Mutex::new(stream)));
                                                    Val::Str(conn_id.into())
                                                }
                                                Err(_) => Val::Nil,
                                            }
                                        }
                                        Err(_) => Val::Nil,
                                    }
                                }
                                Err(_) => Val::Nil,
                            }
                        }
                        None => Val::Nil,
                    }
                }
                Err(_) => Val::Nil,
            };
            
            let _ = result_ch_clone.send(result);
        });

        Ok(Val::Channel(result_channel))
    }

    /// Create a continuous stream reader using channels
    /// Usage: tcp.read_stream(connection_id [, buffer_size])
    /// Returns: channel that continuously receives data
    fn read_stream(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() < 1 || args.len() > 2 {
            return Err(anyhow::anyhow!("read_stream() takes 1 or 2 arguments: connection_id [, buffer_size]"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => id.to_string(),
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let buffer_size = if args.len() == 2 {
            match &args[1] {
                Val::Int(size) => *size as usize,
                _ => return Err(anyhow::anyhow!("buffer_size must be an integer")),
            }
        } else {
            1024
        };

        let stream_channel = Channel::new();
        let stream_ch_clone = stream_channel.clone();
        
        thread::spawn(move || {
            let store = get_connection_store();
            if let Ok(connections) = store.lock() {
                if let Some(stream_arc) = connections.get(&conn_id) {
                    if let Ok(mut stream) = stream_arc.lock() {
                        loop {
                            let mut buffer = vec![0u8; buffer_size];
                            match stream.read(&mut buffer) {
                                Ok(0) => break, // EOF
                                Ok(bytes_read) => {
                                    buffer.truncate(bytes_read);
                                    match String::from_utf8(buffer) {
                                        Ok(data) => {
                                            if stream_ch_clone.send(Val::Str(data.into())).is_err() {
                                                break;
                                            }
                                        }
                                        Err(_) => break,
                                    }
                                }
                                Err(_) => break,
                            }
                        }
                    }
                }
            }
            
            // Signal end of stream with nil
            let _ = stream_ch_clone.send(Val::Nil);
        });

        Ok(Val::Channel(stream_channel))
    }

    /// Create a continuous stream writer using channels
    /// Usage: tcp.write_stream(connection_id)
    /// Returns: channel to send data to the connection
    fn write_stream(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("write_stream() takes exactly 1 argument: connection_id"));
        }

        let conn_id = match &args[0] {
            Val::Str(id) => id.to_string(),
            _ => return Err(anyhow::anyhow!("connection_id must be a string")),
        };

        let write_channel = Channel::new();
        let write_ch_clone = write_channel.clone();
        
        thread::spawn(move || {
            let store = get_connection_store();
            if let Ok(connections) = store.lock() {
                if let Some(stream_arc) = connections.get(&conn_id) {
                    if let Ok(mut stream) = stream_arc.lock() {
                        while let Ok(val) = write_ch_clone.recv() {
                            if val == Val::Nil {
                                break; // Signal to close
                            }
                            if let Val::Str(data) = val {
                                if stream.write_all(data.as_bytes()).is_err() {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        });

        Ok(Val::Channel(write_channel))
    }
}

#[cfg(feature = "stdlib-tcp")]
impl Module for TcpModule {
    fn name(&self) -> &str {
        "tcp"
    }

    fn description(&self) -> &str {
        "TCP networking interface with concurrency support"
    }

    fn register(&self, _registry: &mut qcl_core::module::ModuleRegistry) -> Result<()> {
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}