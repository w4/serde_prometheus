use crate::error::{Error, MetricNameMustBeUtf8};
use lazy_static::lazy_static;
use regex::Regex;
use serde::ser::{Impossible, Serialize};
use snafu::ResultExt;
use std::fmt::Display;

pub struct Serializer<T: std::io::Write> {
    pub output: T,
}
impl<W: std::io::Write> serde::Serializer for &mut Serializer<W> {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Impossible<Self::Ok, Self::Error>;
    type SerializeStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        lazy_static! {
            static ref ALLOWED_METRIC_NAME_RE: Regex =
                Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
        }

        if !ALLOWED_METRIC_NAME_RE.is_match(v) {
            return Err(Error::MetricNameNotInFormat { kind: format!("regex {}", v) });
        }

        self.output.write_all(v.as_bytes())?;
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        // A char encoded as UTF-8 takes 4 bytes at most.
        let mut buf = [0; 4];
        self.serialize_str(v.encode_utf8(&mut buf))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        // convert to utf8 so we can validate the contents of the string
        self.serialize_str(std::str::from_utf8(v).context(MetricNameMustBeUtf8)?)
    }

    ///////////////////////////////////////////////////////////
    // Every other serialiser is a no-op for keys
    ///////////////////////////////////////////////////////////

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "bool".to_string() })
    }

    fn serialize_i8(self, _v: i8) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "i8".to_string() })
    }

    fn serialize_i16(self, _v: i16) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "i16".to_string() })
    }

    fn serialize_i32(self, _v: i32) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "i32".to_string() })
    }

    fn serialize_i64(self, _v: i64) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "i64".to_string() })
    }

    fn serialize_u8(self, _v: u8) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "u8".to_string() })
    }

    fn serialize_u16(self, _v: u16) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "u16".to_string() })
    }

    fn serialize_u32(self, _v: u32) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "u32".to_string() })
    }

    fn serialize_u64(self, _v: u64) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "u64".to_string() })
    }

    fn serialize_f32(self, _v: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "f32".to_string() })
    }

    fn serialize_f64(self, _v: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "f64".to_string() })
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "None".to_string() })
    }

    fn serialize_some<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::MetricNameNotInFormat { kind: "Some".to_string() })
    }
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "()".to_string() })
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: format!("Unit Struct: {}", name) })
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: format!("Unit Variant ({}::{})", name, variant) })
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::MetricNameNotInFormat { kind: format!("Newtype Struct ({})", name) })
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        Err(Error::MetricNameNotInFormat { kind: format!("Newtype Variant ({}::{})", name, variant) })
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "Seq".to_string() })
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "Tuple".to_string() })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: format!("Tuple Struct ({})", name) })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: format!("Tuple Variant ({}::{})", name, variant) })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: "Map".to_string() })
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: format!("Struct ({})", name) })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::MetricNameNotInFormat { kind: format!("Struct Variant ({}::{})", name, variant) })
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::MetricNameNotInFormat { kind: "collect str".to_string() })
    }
}
