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

use serde::{Serialize, ser::{Impossible, SerializeMap, SerializeStruct}};
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
            _ => Err(Error::UnsupportedValue),
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

struct Serializer<'a, T: std::io::Write, S: std::hash::BuildHasher> {
    namespace: Option<&'a str>,
    path: Vec<String>,
    global_labels: HashMap<&'a str, &'a str, S>,
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
    S: std::hash::BuildHasher,
{
    let mut serializer = Serializer {
        namespace,
        path: vec![],
        global_labels,
        // sizeof(value) * 4 to get the size of the utf8-repr of the values then multiply by 12 to
        // get a decent estimate of the size of this output including keys.
        output: Vec::with_capacity(std::mem::size_of_val(value) * 4 * 12),
    };
    value.serialize(&mut serializer)?;
    Ok(String::from_utf8(serializer.output).unwrap())
}

impl<T: std::io::Write, S: std::hash::BuildHasher> Serializer<'_, T, S> {
    fn write_key(&mut self, hint: Option<TypeHint>, key: Option<&str>) -> Result<usize, Error> {
        let path = self.path.last().filter(|x| !x.is_empty());
        let path_before = if self.path.len() > 1 {
            self.path.get(self.path.len() - 2)
        } else {
            None
        };

        let (paths_to_remove, key) = match (path_before, path, key) {
            (_,                 Some(path), Some(key)) => (1, format!("{}_{}", path, key)),
            (Some(path_before), None,       Some(key)) => (2, format!("{}_{}", path_before, key)),
            (_,                 _,          Some(key)) => (1, key.to_string()),
            (Some(path_before), None,       _) => (2, path_before.to_string()),
            //(Some(path_before), Some(path), _) => (2, format!("{}_{}", path_before, path)),
            (_,                 Some(path), _) => (1, path.to_string()),
            (_,                 _,          _) => panic!("that's not going to work"),
        };
        let key = match self.namespace {
            Some(namespace) => format!("{}_{}", namespace, key),
            None => key,
        };

        if let Some(typ) = hint {
            writeln!(self.output, "# TYPE {} {}", key, typ)?;
        }

        key.serialize(&mut KeySerializer {
            output: &mut self.output,
        })?;

        Ok(paths_to_remove)
    }

    fn write_labels(&mut self, paths_to_remove: usize, extras: Option<HashMap<&str, &str>>) -> Result<(), Error> {
        let mut map = extras.unwrap_or_default();

        let path = if self.path.is_empty() {
            None
        } else {
            Some(self.path[..self.path.len() - paths_to_remove].join("/"))
        };
        if let Some(path) = path.as_ref() {
            map.insert("path", path);
        }

        for (key, value) in &self.global_labels {
            map.insert(key, value);
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
}

impl<W: std::io::Write, S: std::hash::BuildHasher> serde::Serializer for &mut Serializer<'_, W, S> {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
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
    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        let paths_to_remove = self.write_key(None, Some(name))?;
        self.write_labels(paths_to_remove, None)?;
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
        let paths_to_remove = self.write_key(TypeHint::from_str(type_name).ok(), None)?;
        self.write_labels(paths_to_remove, None)?;
        self.write_value(value)?;

        Ok(())
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        let name = match name {
            "" => None,
            x => Some(x),
        };

        let hint = match variant_index {
            0 => None,
            x => Some(TypeHint::try_from(x)?),
        };

        let paths_to_remove = self.write_key(hint, name)?;

        self.write_labels(paths_to_remove, if variant.contains('=') {
            let mut labels = HashMap::new();
            let pairs = variant
                .split(',')
                .map(|pair| pair.splitn(2, '='))
                .map(|mut v| (v.next(), v.next()));

            for (key, value) in pairs {
                labels.insert(
                    key.ok_or(Error::InvalidLabel)?,
                    value.ok_or(Error::InvalidLabel)?,
                );
            }
            Some(labels)
        } else {
            None
        })?;

        self.write_value(value)?;

        Ok(())
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

    ///////////////////////////////////////////////////////////
    // Unsupported key/value serialisers
    ///////////////////////////////////////////////////////////

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::UnsupportedValue)
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_i8(self, _v: i8) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_i16(self, _v: i16) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_i32(self, _v: i32) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_i64(self, _v: i64) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_u8(self, _v: u8) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_u16(self, _v: u16) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_u32(self, _v: u32) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_u64(self, _v: u64) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_f32(self, _v: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_f64(self, _v: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }

    fn serialize_some<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::UnsupportedValue)
    }
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::UnsupportedValue)
    }
}

/// Maps are most of the time histograms so we handle them a little bit differently,
/// instead of using the key directly from the map, we modify them a little bit to
/// make them a little bit more Prometheus-like using the `MapKeySerializer`.
impl<W: std::io::Write, S: std::hash::BuildHasher> SerializeMap for &mut Serializer<'_, W, S> {
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

impl<W: std::io::Write, S: std::hash::BuildHasher> SerializeStruct for &mut Serializer<'_, W, S> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        if key != "" {
            self.path.push(key.to_owned());
        }
        value.serialize(&mut **self)?;
        if key != "" {
            self.path.pop();
        }
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

/// A basic serializer for map keys which ensures the key must be stringy and maps
/// them to a more Prometheus-like structure if they look like histograms (which
/// they should be if they're coming from a safe implementation of
/// `metered::MetricRegistry`).
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
            || !split.contains(&"throughput{path = \"biz/bizle\", quantile = \"0.95\"} 0")
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
        if !split.contains(&"global_response_time_count{path = \"wrapper/baz/bazle\", service = \"my_cool_service\"} 0")
            && !split.contains(&"global_response_time_count{service = \"my_cool_service\", path = \"wrapper/baz/bazle\"} 0")
        {
            assert!(split.contains(&"global_response_time_count{service = \"my_cool_service\", path = \"wrapper/biz/bizle\"} 0"));
        }
    }
}
