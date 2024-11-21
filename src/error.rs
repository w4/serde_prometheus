use std::fmt::Display;

use thiserror::Error;

use crate::modifiers::Modifier;

#[derive(Error, Debug)]
pub enum Error {
    #[error("maximum depth of {0} exceeded on field {1} (stack: {2})")]
    TooManyNestedMetrics(usize, &'static str, String),
    #[error("serde internal error: {0}")]
    Custom(String),
    #[error("failed to write to output: {0}")]
    Write(std::io::Error),
    #[error("serialization of type `{0}` would make no sense for prometheus")]
    NotSupported(&'static str),
    #[error("failed to parse modifiers '{0}': {1}")]
    ParseModifiers(&'static str, String),
    #[error("there are no elements remaining in the path stack (stack: {0}, modifiers: {1:?})")]
    ExhaustedPathStack(String, Vec<Modifier>),
    #[error("exceeded maximum prepends to metric name")]
    ExceededPathSize,
    #[error("failed to convert output buffer to utf8: {0}")]
    Utf8(#[from] std::string::FromUtf8Error),
    #[error(
        "too many stack items pushed values to the metric name, maximum of {0} allowed (name: {1})"
    )]
    TooManyKeyPushers(usize, String),
    #[error("failed to serialize map key to string: {0}")]
    SerializeMapKey(serde_plain::Error),
    #[error("too many unique keys exist (maximum {0}), current stack {1} - attempted to push {2}")]
    TooManyKeys(usize, String, String),
    #[error("internal modifier unknown: {0}")]
    UnknownInternalModifier(String),
}

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::Custom(msg.to_string())
    }
}
