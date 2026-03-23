use alloc::string::String;
use core::fmt;

/// Error type for QCL operations.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    /// Tokenization error: invalid characters, unclosed strings, etc.
    Tokenize(String),
    /// Parse error: unexpected tokens, malformed expressions, etc.
    Parse(String),
    /// Evaluation error: type mismatches, invalid operations, etc.
    Eval(String),
    /// Deserialization error: invalid JSON/YAML/TOML input.
    Deserialize(String),
    /// I/O error (e.g. reading stdin).
    Io(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Tokenize(msg) => write!(f, "{msg}"),
            Error::Parse(msg) => write!(f, "{msg}"),
            Error::Eval(msg) => write!(f, "{msg}"),
            Error::Deserialize(msg) => write!(f, "{msg}"),
            Error::Io(msg) => write!(f, "{msg}"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

#[cfg(feature = "std")]
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e.to_string())
    }
}

pub type Result<T> = core::result::Result<T, Error>;
