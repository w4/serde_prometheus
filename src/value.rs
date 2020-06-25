use crate::error::Error;
use serde::ser::{Impossible, Serialize};
use std::fmt::Display;

pub struct Serializer<T: std::io::Write> {
    pub output: T,
}
impl<T: std::io::Write> Serializer<T> {
    fn write_int<N: itoa::Integer>(&mut self, num: N) -> Result<(), Error> {
        itoa::write(&mut self.output, num)?;
        Ok(())
    }

    fn write_float<N: ftoa::Floating>(&mut self, num: N) -> Result<(), Error> {
        ftoa::write(&mut self.output, num)?;
        Ok(())
    }
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

    ///////////////////////////////////////////////////////////
    // value serialisation
    ///////////////////////////////////////////////////////////

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.write_int(if v { 1 } else { 0 })
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.write_int(v)
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.write_float(v)
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.write_float(v)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        0.serialize(self)
    }

    fn serialize_some<T: ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        value.serialize(self)
    }
    // In Serde, unit means an anonymous value containing no data. Map this as 0
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        0.serialize(self)
    }

    // forward values inside newtypes to ourselves
    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize,
    {
        value.serialize(self)
    }

    ///////////////////////////////////////////////////////////
    // No-ops for value serializers
    ///////////////////////////////////////////////////////////

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("Unit Struct ({})", name) })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("map (len: {:?})", len) })
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("Struct ({})", name) })
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: "str".to_string() })
    }

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: "char".to_string() })
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: "bytes".to_string() })
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("Unit Variant ({}::{})", name, variant) })
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
        Err(Error::MetricValueMustBeNumeric { kind: format!("Newtype Variant ({}::{})", name, variant) })
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: "seq".to_string() })
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: "tuple".to_string() })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("Tuple Struct ({})", name) })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("Tuple Variant ({}::{})", name, variant) })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::MetricValueMustBeNumeric { kind: format!("Struct Variant ({}::{})", name, variant) })
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::MetricValueMustBeNumeric { kind: "collect str".to_string() })
    }
}
