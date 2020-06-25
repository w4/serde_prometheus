use std::fmt::Display;

#[derive(Debug, snafu::Snafu)]
#[snafu(visibility = "pub(crate)")]
pub enum Error {
    /// An error thrown by the `serde::Serialize` trait on a type. This error is
    /// thrown from outside of this crate, most likely from within serde for a std
    /// library type.
    #[snafu(display("error while serializing value: {}", context))]
    Serde {
        context: String,
    },
    /// A stdlib io error occurred whilst writing the serialized data out.
    #[snafu(display("std io error whilst writing serialized data: {}", context))]
    Io {
        context: std::io::Error,
    },
    /// The value attempting to be serialised does not belong to any metric, try
    /// serialising this value as a struct member or in a map.
    NoMetricName,
    /// Thrown when serializing in a map that doesn't contain string keys.
    MapKeyMustBeString,
    /// Caused by attempting to serialize labels from a format other than a map.
    LabelsMustBeMap,
    /// Thrown when attempting to serialize a label which doesn't conform to the format
    /// defined [here](https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels).
    LabelKeyNotInFormat,
    /// Thrown when serializing a label map that doesn't use string values.
    LabelValueMustBeString,
    /// Thrown when the main serializer encounters a value that isn't supported,
    /// value can only contain maps that contain numbers, or other structs that
    /// when followed, only lead to numbers.
    #[snafu(display("unsupported value encountered while serializing: {}", kind))]
    UnsupportedValue {
        kind: String
    },
    /// UTF-8 error when attempting to serialize strings passed in by client.
    MetricNameMustBeUtf8 {
        source: std::str::Utf8Error,
    },
    /// Thrown when attempting to serialize a metric name which doesn't conform to the format
    /// defined [here](https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels).
    #[snafu(display("unsupported metric name encountered while serializing: {}", kind))]
    MetricNameNotInFormat {
        kind: String
    },
    /// Thrown when attempting to serialize a metric value that isn't a stdlib numeric type.
    #[snafu(display("unsupported metric value encountered while serializing: {}", kind))]
    MetricValueMustBeNumeric {
        kind: String
    },
    /// Attempted to 'hint' that a value is a type that isn't defined in `crate::TypeHint`
    UnknownHint,
    /// Labels, when being passed to the serializer, must be in the format of `key1=val1,key2=val2`
    InvalidLabel,
    /// A key modifier was passed that isn't recognised. See `crate::Serializer::serialize_newtype_struct`
    /// for recognised modifiers.
    InvalidModifier,
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
            context: msg.to_string(),
        }
    }
}
