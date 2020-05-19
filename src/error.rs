use std::fmt::Display;

#[derive(Debug, snafu::Snafu)]
#[snafu(visibility = "pub(crate)")]
pub enum Error {
    /// An error thrown by the `serde::Serialize` trait on a type. This error is
    /// thrown from outside of this crate, most likely from within serde for a std
    /// library type.
    Serde {
        detail: String,
    },
    /// A stdlib io error occurred whilst writing the serialized data out.
    Io {
        context: std::io::Error,
    },
    /// Thrown when serializing in a map that doesn't contain string keys. This
    /// error indicates an issue with the struct implementing `metered::MetricRegistry`,
    /// not an issue with `metered`.
    MapKeyMustBeString,
    LabelsMustBeMap,
    LabelKeyNotInFormat,
    LabelValueMustBeString,
    /// This is thrown when the serializer encounters a value that isn't supported,
    /// this generally indicates that the value doesn't follow the expected format
    /// of a `metered::MetricRegistry`, please see the trait doc to see the expected
    /// format, or consider using the derive macro of the same name to ensure the
    /// format of the value is correct at compile time.
    UnsupportedValue,
    MetricNameMustBeUtf8 {
        source: std::str::Utf8Error,
    },
    MetricNameNotInFormat,
    MetricValueMustBeNumeric,
    UnknownHint,
    InvalidLabel,
}

impl From<std::io::Error> for Error {
    fn from(context: std::io::Error) -> Self {
        Error::Io { context }
    }
}

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Error::Serde {
            detail: msg.to_string(),
        }
    }
}
