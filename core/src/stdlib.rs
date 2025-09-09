use crate::module::Module;
use crate::val::Val;
use crate::stmt::Environment;
use anyhow::Result;
use std::collections::HashMap;

#[cfg(feature = "stdlib-math")]
#[derive(Debug)]
pub struct MathModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-math")]
impl MathModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Register basic math functions
        functions.insert("abs".to_string(), Val::RustFunction(Self::abs));
        functions.insert("sqrt".to_string(), Val::RustFunction(Self::sqrt));
        functions.insert("sin".to_string(), Val::RustFunction(Self::sin));
        functions.insert("cos".to_string(), Val::RustFunction(Self::cos));
        
        Self { functions }
    }

    fn abs(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("abs() expects exactly 1 argument, got {}", args.len()));
        }
        
        match &args[0] {
            Val::Int(n) => Ok(Val::Int(n.abs())),
            Val::Float(f) => Ok(Val::Float(f.abs())),
            _ => Err(anyhow::anyhow!("abs() expects numeric argument")),
        }
    }

    fn sqrt(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("sqrt() expects exactly 1 argument, got {}", args.len()));
        }
        
        let num = match &args[0] {
            Val::Int(n) => *n as f64,
            Val::Float(f) => *f,
            _ => return Err(anyhow::anyhow!("sqrt() expects numeric argument")),
        };
        
        if num < 0.0 {
            return Err(anyhow::anyhow!("sqrt() of negative number"));
        }
        
        Ok(Val::Float(num.sqrt()))
    }

    fn sin(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("sin() expects exactly 1 argument, got {}", args.len()));
        }
        
        let num = match &args[0] {
            Val::Int(n) => *n as f64,
            Val::Float(f) => *f,
            _ => return Err(anyhow::anyhow!("sin() expects numeric argument")),
        };
        
        Ok(Val::Float(num.sin()))
    }

    fn cos(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("cos() expects exactly 1 argument, got {}", args.len()));
        }
        
        let num = match &args[0] {
            Val::Int(n) => *n as f64,
            Val::Float(f) => *f,
            _ => return Err(anyhow::anyhow!("cos() expects numeric argument")),
        };
        
        Ok(Val::Float(num.cos()))
    }
}

#[cfg(feature = "stdlib-math")]
impl Module for MathModule {
    fn name(&self) -> &str {
        "math"
    }

    fn register(&self, registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        registry.register_module(self.name(), Box::new(MathModule::new()));
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

#[cfg(feature = "stdlib-string")]
#[derive(Debug)]
pub struct StringModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-string")]
impl StringModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        
        functions.insert("len".to_string(), Val::RustFunction(Self::len));
        functions.insert("upper".to_string(), Val::RustFunction(Self::upper));
        functions.insert("lower".to_string(), Val::RustFunction(Self::lower));
        
        Self { functions }
    }

    fn len(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("len() expects exactly 1 argument, got {}", args.len()));
        }
        
        match &args[0] {
            Val::Str(s) => Ok(Val::Int(s.len() as i64)),
            Val::List(l) => Ok(Val::Int(l.len() as i64)),
            _ => Err(anyhow::anyhow!("len() expects string or list argument")),
        }
    }

    fn upper(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("upper() expects exactly 1 argument, got {}", args.len()));
        }
        
        match &args[0] {
            Val::Str(s) => Ok(Val::Str(Arc::from(s.to_uppercase().as_str()))),
            _ => Err(anyhow::anyhow!("upper() expects string argument")),
        }
    }

    fn lower(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("lower() expects exactly 1 argument, got {}", args.len()));
        }
        
        match &args[0] {
            Val::Str(s) => Ok(Val::Str(Arc::from(s.to_lowercase().as_str()))),
            _ => Err(anyhow::anyhow!("lower() expects string argument")),
        }
    }
}

#[cfg(feature = "stdlib-string")]
impl Module for StringModule {
    fn name(&self) -> &str {
        "string"
    }

    fn register(&self, registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        registry.register_module(self.name(), Box::new(StringModule::new()));
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}

#[cfg(feature = "stdlib-datetime")]
#[derive(Debug)]
pub struct DateTimeModule {
    functions: HashMap<String, Val>,
}

#[cfg(feature = "stdlib-datetime")]
impl DateTimeModule {
    pub fn new() -> Self {
        let mut functions = HashMap::new();

        // Register datetime functions as Rust functions
        functions.insert("now".to_string(), Val::RustFunction(Self::now));
        functions.insert("format".to_string(), Val::RustFunction(Self::format));
        functions.insert("parse".to_string(), Val::RustFunction(Self::parse));
        functions.insert("add".to_string(), Val::RustFunction(Self::add_seconds));
        functions.insert("sub".to_string(), Val::RustFunction(Self::sub_seconds));
        functions.insert("day_of_week".to_string(), Val::RustFunction(Self::day_of_week));
        functions.insert("day_of_year".to_string(), Val::RustFunction(Self::day_of_year));
        functions.insert("is_weekend".to_string(), Val::RustFunction(Self::is_weekend));

        Self { functions }
    }

    /// Get current timestamp as Unix epoch
    fn now(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if !args.is_empty() {
            return Err(anyhow::anyhow!("now() takes no arguments"));
        }

        use chrono::{DateTime, Utc};
        let now: DateTime<Utc> = Utc::now();
        let timestamp = now.timestamp();
        Ok(Val::Int(timestamp))
    }

    /// Format timestamp to string
    fn format(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("format() takes exactly 2 arguments: timestamp and format_string"));
        }

        let timestamp = match &args[0] {
            Val::Int(ts) => *ts,
            _ => return Err(anyhow::anyhow!("first argument must be an integer timestamp")),
        };

        let format_str = match &args[1] {
            Val::Str(fmt) => &**fmt,
            _ => return Err(anyhow::anyhow!("second argument must be a format string")),
        };

        use chrono::{DateTime, Utc};
        let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
            .ok_or_else(|| anyhow::anyhow!("invalid timestamp"))?;
        
        let formatted = dt.format(format_str).to_string();
        Ok(Val::Str(formatted.into()))
    }

    /// Parse string to timestamp
    fn parse(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("parse() takes exactly 2 arguments: datetime_string and format_string"));
        }

        let datetime_str = match &args[0] {
            Val::Str(s) => &**s,
            _ => return Err(anyhow::anyhow!("first argument must be a datetime string")),
        };

        let format_str = match &args[1] {
            Val::Str(fmt) => &**fmt,
            _ => return Err(anyhow::anyhow!("second argument must be a format string")),
        };

        use chrono::{DateTime, NaiveDateTime, Utc};
        let naive = NaiveDateTime::parse_from_str(datetime_str, format_str)
            .map_err(|e| anyhow::anyhow!("failed to parse datetime: {}", e))?;
        let dt = DateTime::<Utc>::from_naive_utc_and_offset(naive, Utc);
        
        Ok(Val::Int(dt.timestamp()))
    }

    /// Add seconds to timestamp
    fn add_seconds(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("add_seconds() takes exactly 2 arguments: timestamp and seconds"));
        }

        let timestamp = match &args[0] {
            Val::Int(ts) => *ts,
            _ => return Err(anyhow::anyhow!("first argument must be an integer timestamp")),
        };

        let seconds = match &args[1] {
            Val::Int(s) => *s,
            _ => return Err(anyhow::anyhow!("second argument must be an integer")),
        };

        Ok(Val::Int(timestamp + seconds))
    }

    /// Subtract seconds from timestamp
    fn sub_seconds(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("sub_seconds() takes exactly 2 arguments: timestamp and seconds"));
        }
        let timestamp = match &args[0] {
            Val::Int(ts) => *ts,
            _ => return Err(anyhow::anyhow!("first argument must be an integer timestamp")),
        };
        let seconds = match &args[1] {
            Val::Int(s) => *s,
            _ => return Err(anyhow::anyhow!("second argument must be an integer")),
        };
        Ok(Val::Int(timestamp - seconds))
    }

    /// Get day of week (0 = Sunday, 1 = Monday, ..., 6 = Saturday)
    fn day_of_week(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("day_of_week() takes exactly 1 argument: timestamp"));
        }

        let timestamp = match &args[0] {
            Val::Int(ts) => *ts,
            _ => return Err(anyhow::anyhow!("argument must be an integer timestamp")),
        };

        use chrono::{DateTime, Utc, Weekday, Datelike};
        let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
            .ok_or_else(|| anyhow::anyhow!("invalid timestamp"))?;
        
        let day_num = match dt.weekday() {
            Weekday::Sun => 0,
            Weekday::Mon => 1,
            Weekday::Tue => 2,
            Weekday::Wed => 3,
            Weekday::Thu => 4,
            Weekday::Fri => 5,
            Weekday::Sat => 6,
        };
        
        Ok(Val::Int(day_num))
    }

    /// Get day of year (1-366)
    fn day_of_year(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("day_of_year() takes exactly 1 argument: timestamp"));
        }

        let timestamp = match &args[0] {
            Val::Int(ts) => *ts,
            _ => return Err(anyhow::anyhow!("argument must be an integer timestamp")),
        };

        use chrono::{DateTime, Utc, Datelike};
        let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
            .ok_or_else(|| anyhow::anyhow!("invalid timestamp"))?;
        
        Ok(Val::Int(dt.ordinal() as i64))
    }

    /// Check if date is weekend (Saturday or Sunday)
    fn is_weekend(args: &[Val], _env: &Environment, _ctx: &Val) -> Result<Val> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("is_weekend() takes exactly 1 argument: timestamp"));
        }

        let timestamp = match &args[0] {
            Val::Int(ts) => *ts,
            _ => return Err(anyhow::anyhow!("argument must be an integer timestamp")),
        };

        use chrono::{DateTime, Utc, Weekday, Datelike};
        let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
            .ok_or_else(|| anyhow::anyhow!("invalid timestamp"))?;
        
        let is_weekend = matches!(dt.weekday(), Weekday::Sat | Weekday::Sun);
        Ok(Val::Bool(is_weekend))
    }
}

#[cfg(feature = "stdlib-datetime")]
impl Module for DateTimeModule {
    fn name(&self) -> &str {
        "datetime"
    }

    fn description(&self) -> &str {
        "Date and time functions"
    }

    fn register(&self, registry: &mut crate::module::ModuleRegistry) -> Result<()> {
        registry.register_module(self.name(), Box::new(DateTimeModule::new()));
        Ok(())
    }

    fn exports(&self) -> HashMap<String, Val> {
        self.functions.clone()
    }
}