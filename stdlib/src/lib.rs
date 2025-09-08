pub mod datetime;
pub mod math;
pub mod os;
pub mod string;
pub mod tcp;

#[cfg(test)]
#[cfg(feature = "stdlib-tcp")]
mod tcp_test;

use qcl_core::module::ModuleRegistry;

/// Register all stdlib modules with the given registry
pub fn register_stdlib_modules(registry: &mut ModuleRegistry) {
    #[cfg(feature = "stdlib-math")]
    {
        registry.register_module("math", Box::new(math::MathModule::new()));
    }

    #[cfg(feature = "stdlib-string")]
    {
        registry.register_module("string", Box::new(string::StringModule::new()));
    }

    #[cfg(feature = "stdlib-datetime")]
    {
        registry.register_module("datetime", Box::new(datetime::DateTimeModule::new()));
    }

    #[cfg(feature = "stdlib-os")]
    {
        registry.register_module("os", Box::new(os::OsModule::new()));
    }

    #[cfg(feature = "stdlib-tcp")]
    {
        registry.register_module("tcp", Box::new(tcp::TcpModule::new()));
    }
}
