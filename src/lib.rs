//! A [serde][] implementation for Prometheus' text-based exposition format.
//!
//! Currently this library only supports serialisation to Prometheus' format
//! for exporting metrics but this might be extended to deserialisation
//! later on down the line.
//!
//! `serde_prometheus` will work with most metric libraries' structs out of the
//! box, however some work may be required to get them into a format expected
//! by Prometheus.
//!
//! Metric names exposed in the format are derived from the value's name in the
//! struct or map that contains it.
//!
//! ## Basic Usage
//!
//! ```rust
//! # use std::collections::HashMap;
//! # use serde::Serialize;
//! # fn main() -> Result<(), serde_prometheus::Error> {
//! #[derive(Serialize)]
//! struct HitCount(u64);
//!
//! #[derive(Serialize)]
//! struct MetricRegistry {
//!     my_struct: MyStructMetrics
//! }
//!
//! #[derive(Serialize)]
//! struct MyStructMetrics {
//!     hit_count: HitCount
//! }
//!
//! let metrics = MetricRegistry {
//!     my_struct: MyStructMetrics {
//!         hit_count: HitCount(30)
//!     }
//! };
//!
//! assert_eq!(
//!    serde_prometheus::to_string(&metrics, None, HashMap::new())?,
//!    "hit_count{path = \"my_struct\"} 30\n"
//! );
//! # Ok(())
//! # }
//! ```
//!
//! ## Global Labels
//!
//! Global labels can be added to all metrics exported by `serde_prometheus` using
//! the `HashMap` (or any type resolving to `IntoIterator<Borrow<(&str, &str)>>`)
//! passed into `serde_prometheus::to_string` for example:
//!
//! ```rust
//! # use std::collections::HashMap;
//! # use serde::Serialize;
//! # fn main() -> Result<(), serde_prometheus::Error> {
//! # #[derive(Serialize)]
//! # struct HitCount(u64);
//! #
//! # #[derive(Serialize)]   
//! # struct MetricRegistry {
//! #     my_struct: MyStructMetrics
//! # }
//! #
//! # #[derive(Serialize)]    
//! # struct MyStructMetrics {
//! #     hit_count: HitCount
//! # }
//! #
//! # let metrics = MetricRegistry {  
//! #     my_struct: MyStructMetrics {
//! #         hit_count: HitCount(30)
//! #     }
//! # };
//! let mut labels = HashMap::new();
//! labels.insert("my_key", "my_value");
//!
//! let serialised = serde_prometheus::to_string(&metrics, None, &[("my_key", "my_value")])?;
//! # // deal with HashMap reordering vals
//! # if serialised.contains("{path") {
//! #     assert_eq!(serialised, "hit_count{path = \"my_struct\", my_key = \"my_value\"} 30\n");
//! # } else {
//! assert_eq!(serialised, "hit_count{my_key = \"my_value\", path = \"my_struct\"} 30\n");
//! # }
//! # Ok(())
//! # }
//! ```
//!
//! ## Global Prefix
//!
//! And a global prefix can be added to all metrics:
//!
//! ```rust
//! # use std::collections::HashMap;
//! # use serde::Serialize;
//! # fn main() -> Result<(), serde_prometheus::Error> {
//! # #[derive(Serialize)]
//! # struct HitCount(u64);
//! #
//! # #[derive(Serialize)]   
//! # struct MetricRegistry {
//! #     my_struct: MyStructMetrics
//! # }
//! #  
//! # #[derive(Serialize)]    
//! # struct MyStructMetrics {
//! #     hit_count: HitCount
//! # }
//! #  
//! # let metrics = MetricRegistry {  
//! #     my_struct: MyStructMetrics {
//! #         hit_count: HitCount(30)
//! #     }
//! # };
//! assert_eq!(
//!    serde_prometheus::to_string(&metrics, Some("my_prefix"), HashMap::new())?,
//!    "my_prefix_hit_count{path = \"my_struct\"} 30\n"
//! );
//! # Ok(())
//! # }
//! ```
//!
//! ## Metadata/key manipulation
//!
//! Serde's newtype implementation is (ab)used by `serde_prometheus` to add metadata
//! to serialised fields without breaking backwards compatibility with `serde_json`
//! and such.
//!
//! For example, [`serde_prometheus` support has been added to metered-rs][mrsimpl]'s
//! histograms whilst still keeping the same JSON schema, it does this by using
//! a call to `serialize_newtype_struct` in a struct's Serialize trait impl, the
//! format for the type names is as follows:
//!
//! ```txt
//! keymodifiers|key=value,key2=value2
//! ```
//!
//! Modifiers can also be used in labels using a `==` like so:
//!
//! ```txt
//! |key3==modifiers
//! ```
//!
//! The `path` stack is reset for each label value, however after applying the key
//! modifiers, it is retained when writing the `path` label itself.
//!
//! The modifiers that can be used are:
//!
//! | Modifier     | Description |
//! | ------------ | ----------- |
//! | <            | Pops a value off of the `path` stack and appends it to the name |
//! | !            | Pops the last value off of the `path` stack and drops it |
//! | -            | Moves the stack cursor back a position, when a value is popped off the stack, the position is reset back to the top of the stack |
//! | .            | The default behaviour of `serde_prometheus 0.1` is to append the collected stack to the next value in `path` (as if an extra `<` was added to your modifiers), to prevent this use this modifier. This has no effect in labels. This will be the default behaviour in `serde_prometheus 0.2` |
//!
//! These can be combined and are read from left to right, for example:
//!
//! ```rust
//! # use std::collections::HashMap;
//! # use serde::{Serializer, Serialize};
//! # fn main() -> Result<(), serde_prometheus::Error> {
//! # #[derive(Serialize)]
//! # struct MetricRegistry {
//! #     my_struct: MyStructMetrics
//! # }
//! #
//! # #[derive(Serialize)]    
//! # struct MyStructMetrics {
//! #     my_method: MyMethodMetrics
//! # }
//! #
//! # #[derive(Serialize)]
//! # struct MyMethodMetrics {
//! #     hit_count: HitCount
//! # }
//! #
//! struct HitCount(u64);
//! impl Serialize for HitCount {
//!     fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
//!         // metric name: include our key (hit_count), ignore the second (my_method), and include the third (my_struct)
//!         // metric meta: for the method_name, ignore our key (hit_count), include the second (my_method)
//!         serializer.serialize_newtype_struct("<!<|my_key=my_value,method_name==!<", &self.0)
//!     }
//! }
//!
//! let metrics = MetricRegistry {
//!     my_struct: MyStructMetrics {
//!         my_method: MyMethodMetrics {
//!             hit_count: HitCount(30)
//!         }
//!     }
//! };
//!
//! let serialised = serde_prometheus::to_string(&metrics, None, HashMap::new())?;
//! # // handle hashmap label reordering
//! # if serialised.contains("{my_key") {
//! assert_eq!(
//!    serialised,
//!    // would be `hit_count{path = "my_struct/my_method"}` without the Serialize impl
//!    "my_struct_hit_count{my_key = \"my_value\", method_name = \"my_method\"} 30\n"
//! );
//! # } else {
//! # assert_eq!(
//! #   serialised,
//! #   "my_struct_hit_count{method_name = \"my_method\", my_key = \"my_value\"} 30\n"
//! # );
//! # }
//! # Ok(())
//! # }
//! ```
//!
//! ## Label concatenation
//! By default, when added via a `serialize_newtype_struct` call, a new label added by a
//! "deeper" value will override a previously set one set further up the stack. This can
//! be overridden by the "deeper" value by appending `[::]` to the label name, this will
//! concatenate the previously set label and the new label with a `::`. The `::` can be
//! anything valid in a Prometheus label value except `=`.
//!
//! ```rust
//! # use std::collections::HashMap;
//! # use serde::{Serializer, Serialize};
//! # fn main() -> Result<(), serde_prometheus::Error> {
//! fn add_my_cool_key<S: serde::Serializer, T: serde::Serialize>(
//!     value: &T,
//!     serializer: S,
//! ) -> Result<S::Ok, S::Error> {
//!     serializer.serialize_newtype_struct("!|my_cool_key[::]==<", value)
//! }
//!
//! #[derive(Serialize)]
//! struct MetricRegistry {
//!     #[serde(serialize_with = "add_my_cool_key")]
//!     my_struct: MyStructMetrics
//! }
//!
//! #[derive(Serialize)]
//! struct MyStructMetrics {
//!     #[serde(serialize_with = "add_my_cool_key")]
//!     my_method: MyMethodMetrics
//! }
//!
//! #[derive(Serialize)]
//! struct MyMethodMetrics {
//!     hit_count: HitCount
//! }
//! #
//! # #[derive(Serialize)]
//! # struct HitCount(u64);
//!
//! let metrics = MetricRegistry {
//!     my_struct: MyStructMetrics {
//!         my_method: MyMethodMetrics {
//!             hit_count: HitCount(30)
//!         }
//!     }
//! };
//!
//! let serialised = serde_prometheus::to_string(&metrics, None, HashMap::new())?;
//! assert_eq!(
//!    serialised,
//!    "hit_count{my_cool_key = \"my_struct::my_method\"} 30\n"
//! );
//! # Ok(())
//! # }
//! ```
//!
//! [serde]: https://github.com/serde-rs/serde/
//! [mrsimpl]: https://github.com/magnet/metered-rs/commit/b6b61979a2727e3be58737015ba11eb63309ed6b

#![deny(clippy::all)]
#![deny(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

mod error;
mod key;
mod label;
mod value;

pub use crate::error::Error;
use crate::key::Serializer as KeySerializer;
use crate::label::Serializer as LabelSerializer;
use crate::value::Serializer as ValueSerializer;

use std::borrow::{Borrow, Cow};
use std::convert::TryFrom;
use std::fmt::Display;
use std::str::FromStr;

use indexmap::IndexMap;

use serde::{
    ser::{Impossible, SerializeMap, SerializeSeq, SerializeStruct},
    Serialize,
};
use snafu::ResultExt;

pub enum TypeHint {
    Counter = 1337,
    Guage = 1338,
    Histogram = 1339,
    Summary = 1340,
}
impl TryFrom<u32> for TypeHint {
    type Error = Error;

    fn try_from(x: u32) -> Result<Self, Self::Error> {
        match x {
            x if x == TypeHint::Counter as u32 => Ok(TypeHint::Counter),
            x if x == TypeHint::Guage as u32 => Ok(TypeHint::Guage),
            x if x == TypeHint::Histogram as u32 => Ok(TypeHint::Histogram),
            x if x == TypeHint::Summary as u32 => Ok(TypeHint::Summary),
            _ => Err(Error::UnsupportedValue {
                kind: "TypeHint".to_string(),
            }),
        }
    }
}
impl FromStr for TypeHint {
    type Err = Error;

    fn from_str(v: &str) -> std::result::Result<Self, Self::Err> {
        match v {
            "counter" => Ok(TypeHint::Counter),
            "guage" => Ok(TypeHint::Guage),
            "histogram" => Ok(TypeHint::Histogram),
            "summary" => Ok(TypeHint::Summary),
            _ => Err(Error::UnknownHint),
        }
    }
}
impl Display for TypeHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TypeHint::Counter => "counter",
            TypeHint::Guage => "guage",
            TypeHint::Histogram => "histogram",
            TypeHint::Summary => "summary",
        })
    }
}

struct Serializer<'a, T: std::io::Write> {
    namespace: Option<&'a str>,
    path: Vec<String>,
    global_labels: IndexMap<&'a str, &'a str>,
    current_labels: IndexMap<&'a str, Cow<'a, str>>,
    current_key_suffix: Vec<String>,
    // TODO: this should be replaced with a smarter check, for example - if we've
    //  got nested values after mutating the key, then - and only then - should the
    //  last `path` be appended. we shouldn't be modifying keys at all if the user
    //  has specified their intent for it.
    dont_mutate_keys: bool,
    output: T,
}

/// Outputs a `metered::MetricRegistry` in Prometheus' simple text-based exposition
/// format.
pub fn to_string<'a, T, L, LI>(
    value: &T,
    namespace: Option<&str>,
    global_labels: L,
) -> Result<String, Error>
where
    T: ?Sized + Serialize,
    LI: Borrow<(&'a str, &'a str)>,
    L: IntoIterator<Item = LI>,
{
    let mut serializer = Serializer {
        namespace,
        path: vec![],
        global_labels: global_labels
            .into_iter()
            .map(|v| (v.borrow().0, v.borrow().1))
            .collect(),
        current_labels: IndexMap::new(),
        current_key_suffix: Vec::new(),
        dont_mutate_keys: false,
        // sizeof(value) * 4 to get the size of the utf8-repr of the values then multiply by 12 to
        // get a decent estimate of the size of this output including keys.
        output: Vec::with_capacity(std::mem::size_of_val(value) * 8 * 12),
    };

    for (key, value) in &serializer.global_labels {
        serializer.current_labels.insert(key, Cow::Borrowed(value));
    }

    value.serialize(&mut serializer)?;
    Ok(String::from_utf8(serializer.output).unwrap())
}

impl<T: std::io::Write> Serializer<'_, T> {
    fn write_key(&mut self, hint: Option<TypeHint>) -> Result<(), Error> {
        let path = self.path.last();
        let key = self.current_key_suffix.join("_");

        let mut key = match path {
            Some(path) if key != "" && !self.dont_mutate_keys => format!("{}_{}", path, key),
            _ if key != "" => key,
            Some(path) => path.to_string(),
            None => return Err(error::Error::NoMetricName),
        };

        if let Some(namespace) = self.namespace {
            key = format!("{}_{}", namespace, key);
        }

        if let Some(typ) = hint {
            writeln!(self.output, "# TYPE {} {}", key, typ)?;
        }

        key.serialize(&mut KeySerializer {
            output: &mut self.output,
        })?;

        Ok(())
    }

    fn write_labels(&mut self) -> Result<(), Error> {
        let mut map = self.current_labels.clone();

        let path = if self.path.is_empty() {
            None
        } else {
            let path_len = if self.dont_mutate_keys {
                self.path.len()
            } else {
                self.path.len() - 1
            };
            Some(self.path[..path_len].join("/"))
        };
        if let Some(path) = path.as_ref() {
            if path != "" {
                map.insert("path", Cow::Borrowed(path));
            }
        }

        if !map.is_empty() {
            map.serialize(&mut LabelSerializer {
                output: &mut self.output,
                remaining: 0,
            })?;
        }

        Ok(())
    }

    fn write_value<V: Serialize>(&mut self, value: V) -> Result<(), Error> {
        self.output.write_all(b" ")?;
        value.serialize(&mut ValueSerializer {
            output: &mut self.output,
        })?;
        self.output.write_all(b"\n")?;

        Ok(())
    }

    fn map_modifiers(&mut self, modifiers: &str) -> Result<Option<Vec<String>>, Error> {
        if modifiers.is_empty() {
            return Ok(None);
        }

        let mut key = Vec::new(); // VecDeque for push_front?

        let mut to_skip = 0;

        for modifier in modifiers.chars() {
            match modifier {
                // include the last appended path, ignoring excluded ones, in the key instead
                // of the 'path' label
                '<' => {
                    key.insert(0, self.path.remove(self.path.len() - 1 - to_skip));
                    to_skip = 0;
                }
                // exclude a path from both the name and the 'path' label
                '!' => {
                    self.path.remove(self.path.len() - 1 - to_skip);
                    to_skip = 0;
                }
                '-' => {
                    to_skip += 1;
                }
                '.' => {
                    self.dont_mutate_keys = true;
                }
                _ => return Err(Error::InvalidModifier),
            }
        }

        if key.is_empty() {
            Ok(None)
        } else {
            Ok(Some(key))
        }
    }
}

impl<W: std::io::Write> serde::Serializer for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Self;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;

    ///////////////////////////////////////////////////////////
    // whole key/value serialisation
    ///////////////////////////////////////////////////////////

    // Unit struct means a named value containing no data.
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(0)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        type_name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        let (modifiers, labels) = if type_name.contains('|') {
            let mut split = type_name.splitn(2, '|');
            (
                split.next().filter(|v| !v.is_empty()),
                split.next().filter(|v| !v.is_empty()),
            )
        } else {
            (
                None,
                Some(type_name).filter(|v| !v.is_empty() && v.contains('=')),
            )
        };

        let original_path = self.path.clone();
        let original_key_suffix = self.current_key_suffix.clone();
        let original_labels = self.current_labels.clone();
        let original_dont_mutate_keys = self.dont_mutate_keys;

        // run the label manipulation first so key manipulation applies to the path
        if let Some(label) = labels {
            let pairs = label
                .split(',')
                .map(|pair| pair.splitn(2, '='))
                .map(|mut v| (v.next(), v.next()));

            for (key, value) in pairs {
                let mut key = key.ok_or(Error::InvalidLabel)?;
                let value = value.ok_or(Error::InvalidLabel)?;

                // a key ending with `[::]` means that we should concatenate all labels with the same
                // name we end up with when serializing the end value with `::` as the separator
                let mut separator = None;
                if key.ends_with(']') {
                    let mut split = key.splitn(2, '[');
                    key = split.next().ok_or(Error::InvalidLabel)?;
                    let separator_with_closing_bracket = split.next();

                    if let Some(separator_wcb) = separator_with_closing_bracket {
                        separator = Some(&separator_wcb[..separator_wcb.len() - 1]);
                    }
                }

                // `=` is a magic char to indicate that a label value uses modifiers to get its value
                let value = if value.starts_with('=') {
                    Cow::Owned(
                        self.map_modifiers(&value[1..])?
                            .map(|v| v.join("_"))
                            .unwrap_or_default(),
                    )
                } else {
                    Cow::Borrowed(value)
                };

                if let Some(separator) = separator {
                    self.current_labels
                        .entry(key)
                        .and_modify(|existing| {
                            *existing = Cow::Owned(format!("{}{}{}", existing, separator, value))
                        })
                        .or_insert(value);
                } else {
                    self.current_labels.insert(key, value);
                }

                // reset the path stack for the next set of modifiers
                self.path = original_path.clone();
                self.dont_mutate_keys = original_dont_mutate_keys;
            }
        }

        if let Some(keys) = modifiers.and_then(|v| self.map_modifiers(v).transpose()) {
            for key in keys? {
                self.current_key_suffix.push(key);
            }
        };

        value.serialize(&mut *self)?;

        self.path = original_path;
        self.current_key_suffix = original_key_suffix;
        self.current_labels = original_labels;
        self.dont_mutate_keys = original_dont_mutate_keys;

        Ok(())
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        value.serialize(&mut *self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(self)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(self)
    }

    fn serialize_i8(self, value: i8) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_i16(self, value: i16) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_i32(self, value: i32) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_i64(self, value: i64) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_u8(self, value: u8) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_u16(self, value: u16) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_u32(self, value: u32) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_u64(self, value: u64) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_f32(self, value: f32) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_f64(self, value: f64) -> Result<Self::Ok, Self::Error> {
        self.write_key(None)?;
        self.write_labels()?;
        self.write_value(value)?;
        Ok(())
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        // noop
        Ok(())
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        value.serialize(&mut *self)?;
        Ok(())
    }

    ///////////////////////////////////////////////////////////
    // Unsupported key/value serialisers
    ///////////////////////////////////////////////////////////

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: format!("Unit Variant ({}::{})", name, variant),
        })
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: "Tuple".to_string(),
        })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: format!("Tuple Struct ({})", name),
        })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: format!("Tuple Variant ({}::{})", name, variant),
        })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: format!("Struct Variant ({}::{})", name, variant),
        })
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::UnsupportedValue {
            kind: "collect_str".to_string(),
        })
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: "str".to_string(),
        })
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: "char".to_string(),
        })
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: "bytes".to_string(),
        })
    }

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: "bool".to_string(),
        })
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue {
            kind: "()".to_string(),
        })
    }
}

/// Maps are most of the time histograms so we handle them a little bit differently,
/// instead of using the key directly from the map, we modify them a little bit to
/// make them a little bit more Prometheus-like using the `MapKeySerializer`.
impl<W: std::io::Write> SerializeMap for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        let key_bytes = key.serialize(MapKeySerializer)?;
        self.path.push(
            std::str::from_utf8(key_bytes.as_bytes())
                .context(error::MetricNameMustBeUtf8)?
                .to_owned(),
        );

        Ok(())
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        value.serialize(&mut **self)?;
        self.path.pop();
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<W: std::io::Write> SerializeStruct for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.path.push(key.to_owned());
        value.serialize(&mut **self)?;
        self.path.pop();
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<W: std::io::Write> SerializeSeq for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        value.serialize(&mut **self)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

struct MapKeySerializer;
impl serde::Serializer for MapKeySerializer {
    type Ok = String;
    type Error = Error;

    type SerializeSeq = Impossible<String, Error>;
    type SerializeTuple = Impossible<String, Error>;
    type SerializeTupleStruct = Impossible<String, Error>;
    type SerializeTupleVariant = Impossible<String, Error>;
    type SerializeMap = Impossible<String, Error>;
    type SerializeStruct = Impossible<String, Error>;
    type SerializeStructVariant = Impossible<String, Error>;

    fn serialize_bool(self, _value: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_i8(self, value: i8) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_i16(self, value: i16) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_i32(self, value: i32) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_i64(self, value: i64) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_u8(self, value: u8) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_u16(self, value: u16) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_u32(self, value: u32) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_u64(self, value: u64) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_f32(self, _value: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_f64(self, _value: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_char(self, value: char) -> Result<Self::Ok, Self::Error> {
        // A char encoded as UTF-8 takes 4 bytes at most.
        let mut buf = [0; 4];
        self.serialize_str(value.encode_utf8(&mut buf))
    }

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        Ok(value.to_string())
    }

    fn serialize_bytes(self, _value: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_some<T>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(variant.to_string())
    }

    #[inline]
    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::MapKeyMustBeString)
    }

    fn collect_str<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Ok(value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use metered::{metered, HitCount, ResponseTime, Throughput};
    use std::collections::HashMap;

    #[derive(serde::Serialize)]
    pub struct ServiceMetricRegistry<'a> {
        biz: &'a BizMetrics,
        baz: &'a BazMetrics,
    }

    #[derive(Default)]
    pub struct Biz {
        metrics: BizMetrics,
    }
    #[metered(registry = BizMetrics)]
    impl Biz {
        #[measure([HitCount, Throughput, ResponseTime])]
        pub fn bizle(&self) {}
    }

    #[derive(Default)]
    pub struct Baz {
        metrics: BazMetrics,
    }
    #[metered(registry = BazMetrics)]
    impl Baz {
        #[measure([HitCount, Throughput, ResponseTime])]
        pub fn bazle(&self) {}
    }

    #[test]
    fn normal_registry() {
        let biz = Biz::default();
        let baz = Baz::default();

        let ret = crate::to_string(
            &ServiceMetricRegistry {
                biz: &biz.metrics,
                baz: &baz.metrics,
            },
            None,
            HashMap::new(),
        )
        .unwrap();
        let split: Vec<&str> = ret.split("\n").collect();

        assert_eq!(split[0], "hit_count{path = \"biz/bizle\"} 0");
        assert!(split.contains(&"throughput{quantile = \"0.95\", path = \"biz/bizle\"} 0"));
    }

    #[test]
    fn wrapped_registry() {
        #[derive(serde::Serialize)]
        pub struct MyWrapperRegistry<'a> {
            wrapper: ServiceMetricRegistry<'a>,
        }

        let biz = Biz::default();
        let baz = Baz::default();

        let mut labels = HashMap::new();
        labels.insert("service", "my_cool_service");
        let ret = crate::to_string(
            &MyWrapperRegistry {
                wrapper: ServiceMetricRegistry {
                    biz: &biz.metrics,
                    baz: &baz.metrics,
                },
            },
            Some("global"),
            labels,
        )
        .unwrap();
        let split: Vec<&str> = ret.split("\n").collect();

        assert_eq!(split[0], "global_hit_count{service = \"my_cool_service\", path = \"wrapper/biz/bizle\"} 0");
        assert!(split.contains(&"global_response_time_samples{service = \"my_cool_service\", path = \"wrapper/biz/bizle\"} 0"));
    }
}
