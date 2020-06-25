//! A [serde][] implementation for Prometheus' text-based exposition format.
//!
//! Currently this library only supports serialisation to Prometheus' format
//! for exporting metrics but this might be extended to deserialisation
//! later on down the line.
//!
//! serde_prometheus will work with most metric libraries' structs out of the
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
//! Global labels can be added to all metrics exported by serde_prometheus using
//! the `HashMap` passed into `serde_prometheus::to_string` for example:
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
//! let serialised = serde_prometheus::to_string(&metrics, None, labels)?;
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
//! Serde's newtype implementation is (ab)used by serde_prometheus to add metadata
//! to serialised fields without breaking backwards compatibility with serde_json
//! and such.
//!
//! For example, [serde_prometheus support has been added to metered-rs][mrsimpl]'s
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
//! | <            | Pops a value off of the `path` stack and prepends it to the name |
//! | !            | Pops the last value off of the `path` stack and drops it |
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
use crate::key::{Serializer as KeySerializer};
use crate::label::{Serializer as LabelSerializer};
use crate::value::{Serializer as ValueSerializer};

use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::FromStr;
use std::fmt::Display;
use std::borrow::Cow;

use serde::{Serialize, ser::{Impossible, SerializeMap, SerializeStruct, SerializeSeq}};
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
            _ => Err(Error::UnsupportedValue { kind: "TypeHint".to_string() }),
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

struct Serializer<'a, T: std::io::Write, S: std::hash::BuildHasher + Clone> {
    namespace: Option<&'a str>,
    path: Vec<String>,
    global_labels: HashMap<&'a str, &'a str, S>,
    current_labels: HashMap<&'a str, Cow<'a, str>>,
    current_key_prefix: Vec<String>,
    output: T,
}

/// Outputs a `metered::MetricRegistry` in Prometheus' simple text-based exposition
/// format.
pub fn to_string<T, S>(
    value: &T,
    namespace: Option<&str>,
    global_labels: HashMap<&str, &str, S>,
) -> Result<String, Error>
where
    T: ?Sized + Serialize,
    S: std::hash::BuildHasher + Clone,
{
    let mut serializer = Serializer {
        namespace,
        path: vec![],
        global_labels,
        current_labels: HashMap::new(),
        current_key_prefix: Vec::new(),
        // sizeof(value) * 4 to get the size of the utf8-repr of the values then multiply by 12 to
        // get a decent estimate of the size of this output including keys.
        output: Vec::with_capacity(std::mem::size_of_val(value) * 4 * 12),
    };
    serializer.reset_labels();
    value.serialize(&mut serializer)?;
    Ok(String::from_utf8(serializer.output).unwrap())
}

impl<T: std::io::Write, S: std::hash::BuildHasher + Clone> Serializer<'_, T, S> {
    fn write_key<'a>(&mut self, hint: Option<TypeHint>) -> Result<(), Error> {
        let path = self.path.last();
        let key = self.current_key_prefix.join("_");

        let mut key = match path {
            Some(path) if key != "" => format!("{}_{}", path, key),
            _          if key != "" => key,
            Some(path) => path.to_string(),
            _ => return Err(error::Error::NoMetricName),
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

    fn reset_labels(&mut self) {
        self.current_labels = HashMap::new();

        for (key, value) in &self.global_labels {
            self.current_labels.insert(key, Cow::Borrowed(value));
        }
    }

    fn write_labels<'a>(&mut self) -> Result<(), Error> {
        let mut map = self.current_labels.clone();

        let path = if self.path.is_empty() {
            None
        } else {
            Some(self.path[..self.path.len() - 1].join("/"))
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

        for modifier in modifiers.chars() {
            match modifier {
                // include the last appended path, ignoring excluded ones, in the key instead
                // of the 'path' label
                '<' => key.insert(0, self.path.pop().expect("no path to pop!!")),
                // exclude a path from both the name and the 'path' label
                '!' => { self.path.pop(); },
                _ => return Err(Error::InvalidModifier)
            }
        }

        if key.is_empty() {
            Ok(None)
        } else {
            Ok(Some(key))
        }
    }
}

impl<W: std::io::Write, S: std::hash::BuildHasher + Clone> serde::Serializer for &mut Serializer<'_, W, S> {
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
            (split.next().filter(|v| !v.is_empty()), split.next().filter(|v| !v.is_empty()))
        } else {
            (None, Some(type_name).filter(|v| !v.is_empty() && v.contains('=')))
        };

        let original_path = self.path.clone();
        let original_key_prefix = self.current_key_prefix.clone();
        let original_labels = self.current_labels.clone();

        // run the label manipulation first so key manipulation applies to the path
        if let Some(label) = labels {
            let pairs = label
                .split(',')
                .map(|pair| pair.splitn(2, '='))
                .map(|mut v| (v.next(), v.next()));

            for (key, value) in pairs {
                let value = value.ok_or(Error::InvalidLabel)?;

                // `=` is a magic char to indicate that a label value uses modifiers to get its value
                let value = if value.starts_with('=') {
                    Cow::Owned(self.map_modifiers(&value[1..])?.map(|v| v.join("_")).unwrap_or_default())
                } else {
                    Cow::Borrowed(value)
                };

                self.current_labels.insert(
                    key.ok_or(Error::InvalidLabel)?,
                    value,
                );

                // reset the path stack for the next set of modifiers
                self.path = original_path.clone();
            }
        }

        if let Some(keys) = modifiers.and_then(|v| self.map_modifiers(v).transpose()) {
            for key in keys? {
                self.current_key_prefix.push(key);
            }
        };

        value.serialize(&mut *self)?;

        self.path = original_path;
        self.current_key_prefix = original_key_prefix;
        self.current_labels = original_labels;

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
        Err(Error::UnsupportedValue { kind: format!("Unit Variant ({}::{})", name, variant) })
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::UnsupportedValue { kind: "Tuple".to_string() })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::UnsupportedValue { kind: format!("Tuple Struct ({})", name) })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::UnsupportedValue { kind: format!("Tuple Variant ({}::{})", name, variant) })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::UnsupportedValue { kind: format!("Struct Variant ({}::{})", name, variant) })
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::UnsupportedValue { kind: "collect_str".to_string() })
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue { kind: "str".to_string() })
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue { kind: "char".to_string() })
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue { kind: "bytes".to_string() })
    }

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue { kind: "bool".to_string() })
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue { kind: "()".to_string() })
    }
}

/// Maps are most of the time histograms so we handle them a little bit differently,
/// instead of using the key directly from the map, we modify them a little bit to
/// make them a little bit more Prometheus-like using the `MapKeySerializer`.
impl<W: std::io::Write, S: std::hash::BuildHasher + Clone> SerializeMap for &mut Serializer<'_, W, S> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        let key_bytes = key.serialize(MapKeySerializer)?;
        self.path.push(std::str::from_utf8(key_bytes.as_bytes()).context(error::MetricNameMustBeUtf8)?.to_owned());

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

impl<W: std::io::Write, S: std::hash::BuildHasher + Clone> SerializeStruct for &mut Serializer<'_, W, S> {
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

impl<W: std::io::Write, S: std::hash::BuildHasher + Clone> SerializeSeq for &mut Serializer<'_, W, S> {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(
        &mut self,
        value: &T
    ) -> Result<(), Self::Error> {
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

        if !split.contains(&"throughput{quantile = \"0.95\", path = \"biz/bizle\"} 0")
            && !split.contains(&"throughput{path = \"biz/bizle\", quantile = \"0.95\"} 0")
        {
            assert!(split.contains(&"throughput{quantile = \"0.95\", path = \"biz/bizle\"} 0"));
        }
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

        if split[0] != "global_hit_count{service = \"my_cool_service\", path = \"wrapper/biz/bizle\"} 0"
            && split[0] != "global_hit_count{path = \"wrapper/biz/bizle\", service = \"my_cool_service\"} 0"
        {
            assert_eq!(split[0], "global_hit_count{service = \"my_cool_service\", path = \"wrapper/biz/bizle\"} 0");
        }
        if !split.contains(&"global_response_time_samples{path = \"wrapper/baz/bazle\", service = \"my_cool_service\"} 0")
            && !split.contains(&"global_response_time_samples{service = \"my_cool_service\", path = \"wrapper/baz/bazle\"} 0")
        {
            assert!(split.contains(&"global_response_time_samples{service = \"my_cool_service\", path = \"wrapper/biz/bizle\"} 0"));
        }
    }
}
