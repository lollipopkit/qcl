#[cfg(test)]
#[cfg(feature = "stdlib-tcp")]
mod tests {
    use crate::tcp::TcpModule;
    use qcl_core::{module::Module, val::Val, stmt::Environment, import::ModuleResolver};
    use std::sync::Arc;

    fn create_test_env() -> Environment {
        Environment::with_resolver(Arc::new(ModuleResolver::new()))
    }

    #[test]
    fn test_tcp_module_creation() {
        let tcp_module = TcpModule::new();
        assert_eq!(tcp_module.name(), "tcp");
        assert_eq!(tcp_module.description(), "TCP networking interface with concurrency support");
        
        let exports = tcp_module.exports();
        assert!(exports.contains_key("connect"));
        assert!(exports.contains_key("connect_async"));
        assert!(exports.contains_key("send"));
        assert!(exports.contains_key("recv"));
        assert!(exports.contains_key("send_async"));
        assert!(exports.contains_key("recv_async"));
        assert!(exports.contains_key("close"));
        assert!(exports.contains_key("set_timeout"));
        assert!(exports.contains_key("bind"));
        assert!(exports.contains_key("accept"));
        assert!(exports.contains_key("accept_async"));
        assert!(exports.contains_key("close_listener"));
        assert!(exports.contains_key("read_stream"));
        assert!(exports.contains_key("write_stream"));
        assert!(exports.contains_key("resolve"));
    }

    #[test]
    fn test_resolve_function() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let resolve_func = exports.get("resolve").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = resolve_func {
            // Test with localhost
            let args = vec![Val::Str("localhost".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_ok());
            
            // Test with invalid arguments
            let args = vec![];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            let args = vec![Val::Int(123)];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
        } else {
            panic!("resolve should be a RustFunction");
        }
    }

    #[test]
    fn test_connect_invalid_args() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let connect_func = exports.get("connect").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = connect_func {
            // Test with no arguments
            let args = vec![];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            // Test with too many arguments
            let args = vec![Val::Str("127.0.0.1".into()), Val::Int(8080), Val::Int(123)];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            // Test with invalid address type
            let args = vec![Val::Int(123)];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            // Test with invalid port type
            let args = vec![Val::Str("127.0.0.1".into()), Val::Str("invalid".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
        } else {
            panic!("connect should be a RustFunction");
        }
    }

    #[test]
    fn test_send_invalid_args() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let send_func = exports.get("send").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = send_func {
            // Test with wrong number of arguments
            let args = vec![Val::Str("conn_id".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            // Test with invalid connection id type
            let args = vec![Val::Int(123), Val::Str("data".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            // Test with invalid data type
            let args = vec![Val::Str("conn_id".into()), Val::Int(123)];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            // Test with non-existent connection
            let args = vec![Val::Str("non_existent".into()), Val::Str("data".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
        } else {
            panic!("send should be a RustFunction");
        }
    }

    #[test]
    fn test_connect_async_returns_channel() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let connect_async_func = exports.get("connect_async").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = connect_async_func {
            // Test with invalid address to ensure we get a channel back
            let args = vec![Val::Str("invalid:99999".into())];
            let result = func(&args, &env, &ctx);
            
            match result {
                Ok(Val::Channel(_)) => {
                    // Success - we got a channel back
                    // Test passed
                }
                _ => panic!("connect_async should return a channel"),
            }
        } else {
            panic!("connect_async should be a RustFunction");
        }
    }

    #[test]
    fn test_send_async_returns_channel() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let send_async_func = exports.get("send_async").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = send_async_func {
            // Test with non-existent connection to ensure we get a channel back
            let args = vec![Val::Str("non_existent".into()), Val::Str("data".into())];
            let result = func(&args, &env, &ctx);
            
            match result {
                Ok(Val::Channel(_)) => {
                    // Success - we got a channel back
                    // Test passed
                }
                _ => panic!("send_async should return a channel"),
            }
        } else {
            panic!("send_async should be a RustFunction");
        }
    }

    #[test]
    fn test_recv_async_returns_channel() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let recv_async_func = exports.get("recv_async").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = recv_async_func {
            // Test with non-existent connection to ensure we get a channel back
            let args = vec![Val::Str("non_existent".into())];
            let result = func(&args, &env, &ctx);
            
            match result {
                Ok(Val::Channel(_)) => {
                    // Success - we got a channel back
                    // Test passed
                }
                _ => panic!("recv_async should return a channel"),
            }
        } else {
            panic!("recv_async should be a RustFunction");
        }
    }

    #[test]
    fn test_accept_async_returns_channel() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let accept_async_func = exports.get("accept_async").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = accept_async_func {
            // Test with non-existent listener to ensure we get a channel back
            let args = vec![Val::Str("non_existent".into())];
            let result = func(&args, &env, &ctx);
            
            match result {
                Ok(Val::Channel(_)) => {
                    // Success - we got a channel back
                    // Test passed
                }
                _ => panic!("accept_async should return a channel"),
            }
        } else {
            panic!("accept_async should be a RustFunction");
        }
    }

    #[test]
    fn test_read_stream_returns_channel() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let read_stream_func = exports.get("read_stream").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = read_stream_func {
            // Test with non-existent connection to ensure we get a channel back
            let args = vec![Val::Str("non_existent".into())];
            let result = func(&args, &env, &ctx);
            
            match result {
                Ok(Val::Channel(_)) => {
                    // Success - we got a channel back
                    // Test passed
                }
                _ => panic!("read_stream should return a channel"),
            }
        } else {
            panic!("read_stream should be a RustFunction");
        }
    }

    #[test]
    fn test_write_stream_returns_channel() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let write_stream_func = exports.get("write_stream").unwrap();
        
        let env = create_test_env();
        let ctx = Val::Nil;
        
        if let Val::RustFunction(func) = write_stream_func {
            // Test with non-existent connection to ensure we get a channel back
            let args = vec![Val::Str("non_existent".into())];
            let result = func(&args, &env, &ctx);
            
            match result {
                Ok(Val::Channel(_)) => {
                    // Success - we got a channel back
                    // Test passed
                }
                _ => panic!("write_stream should return a channel"),
            }
        } else {
            panic!("write_stream should be a RustFunction");
        }
    }

    #[test]
    fn test_async_functions_argument_validation() {
        let tcp_module = TcpModule::new();
        let exports = tcp_module.exports();
        let env = create_test_env();
        let ctx = Val::Nil;

        // Test connect_async with invalid arguments
        if let Val::RustFunction(func) = exports.get("connect_async").unwrap() {
            let args = vec![];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            let args = vec![Val::Int(123)];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
        }

        // Test send_async with invalid arguments
        if let Val::RustFunction(func) = exports.get("send_async").unwrap() {
            let args = vec![Val::Str("conn".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            let args = vec![Val::Int(123), Val::Str("data".into())];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
        }

        // Test recv_async with invalid arguments
        if let Val::RustFunction(func) = exports.get("recv_async").unwrap() {
            let args = vec![];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
            
            let args = vec![Val::Int(123)];
            let result = func(&args, &env, &ctx);
            assert!(result.is_err());
        }
    }
}