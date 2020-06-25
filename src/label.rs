use crate::error::Error;
use crate::key::Serializer as KeySerializer;
use serde::ser::{Impossible, Serialize};
use serde::ser::{SerializeMap, SerializeStruct};
use std::fmt::Display;

pub struct Serializer<W: std::io::Write> {
    pub output: W,
    pub remaining: usize,
}
impl<W: std::io::Write> Serializer<W> {
    fn write_kv_pair<K: ?Sized + Serialize, V: ?Sized + Serialize>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Error> {
        if let Some(value) = value.serialize(LabelValueSerializer {})? {
            key.serialize(&mut KeySerializer {
                output: &mut self.output,
            })
            .map_err(|_| Error::LabelKeyNotInFormat)?;

            self.output.write_all(b" = ")?;

            self.output.write_all(value.as_bytes())?;

            if self.remaining > 0 {
                self.output.write_all(b", ")?;
                self.remaining -= 1;
            }
        }

        Ok(())
    }
}
impl<W: std::io::Write> serde::Serializer for &mut Serializer<W> {
    type Ok = ();
    type Error = Error;

    type SerializeSeq = Impossible<(), Error>;
    type SerializeTuple = Impossible<(), Error>;
    type SerializeTupleStruct = Impossible<(), Error>;
    type SerializeTupleVariant = Impossible<(), Error>;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Impossible<(), Error>;

    fn serialize_char(self, _value: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.output.write_all(value.as_bytes())?;
        Ok(())
    }

    fn serialize_bytes(self, _value: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_bool(self, _value: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_i8(self, _value: i8) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_i16(self, _value: i16) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_i32(self, _value: i32) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_i64(self, _value: i64) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_u8(self, _value: u8) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_u16(self, _value: u16) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_u32(self, _value: u32) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_u64(self, _value: u64) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_f32(self, _value: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_f64(self, _value: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_some<T>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    #[inline]
    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        Err(Error::LabelsMustBeMap)
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
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.remaining = len.expect("only supports sized maps") - 1;
        self.output.write_all(b"{")?;
        Ok(self)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.remaining = len - 1;
        self.output.write_all(b"{")?;
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::LabelsMustBeMap)
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::LabelsMustBeMap)
    }
}
impl<W: std::io::Write> SerializeStruct for &mut Serializer<W> {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.write_kv_pair(key, value)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.output.write_all(b"}")?;
        Ok(())
    }
}
impl<W: std::io::Write> SerializeMap for &mut Serializer<W> {
    type Ok = ();
    type Error = Error;

    fn serialize_entry<K: ?Sized, V: ?Sized>(
        &mut self,
        key: &K,
        value: &V,
    ) -> Result<(), Self::Error>
    where
        K: Serialize,
        V: Serialize,
    {
        self.write_kv_pair(key, value)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.output.write_all(b"}")?;
        Ok(())
    }

    fn serialize_key<T: ?Sized>(&mut self, _key: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        unreachable!()
    }

    fn serialize_value<T: ?Sized>(&mut self, _value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        unreachable!()
    }
}

struct LabelValueSerializer {}
impl serde::Serializer for LabelValueSerializer {
    type Ok = Option<String>;
    type Error = Error;

    type SerializeSeq = Impossible<Self::Ok, Self::Error>;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Impossible<Self::Ok, Self::Error>;
    type SerializeStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        Ok(Some(format!("\"{}\"", value.replace("\"", "\\\""))))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(None)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_char(self, _value: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_bytes(self, _value: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_bool(self, _value: bool) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_i8(self, _value: i8) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_i16(self, _value: i16) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_i32(self, _value: i32) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_i64(self, _value: i64) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_u8(self, _value: u8) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_u16(self, _value: u16) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_u32(self, _value: u32) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_u64(self, _value: u64) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_f32(self, _value: f32) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_f64(self, _value: f64) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    #[inline]
    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        Err(Error::LabelValueMustBeString)
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
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::LabelValueMustBeString)
    }

    fn collect_str<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Display,
    {
        Err(Error::LabelValueMustBeString)
    }
}

#[cfg(test)]
mod tests {
    use crate::label::Serializer;
    use std::collections::HashMap;

    pub fn serialize_label<T: serde::Serialize>(value: T) -> Result<String, crate::error::Error> {
        let mut serializer = Serializer {
            remaining: 0,
            output: Vec::with_capacity(128),
        };
        value.serialize(&mut serializer)?;
        Ok(String::from_utf8(serializer.output).unwrap())
    }

    #[test]
    pub fn single_struct() {
        #[derive(serde::Serialize)]
        struct Single {
            my_key: &'static str,
        }

        let a = Single { my_key: "abcdef" };

        let val = serialize_label(a).unwrap();
        assert_eq!(val, "{my_key = \"abcdef\"}");
    }

    #[test]
    pub fn multi_struct() {
        #[derive(serde::Serialize)]
        struct Multi {
            my_key: &'static str,
            my_other_key: &'static str,
        }

        let a = Multi {
            my_key: "abcdef",
            my_other_key: "1234",
        };

        let val = serialize_label(a).unwrap();
        assert_eq!(val, "{my_key = \"abcdef\", my_other_key = \"1234\"}");
    }

    #[test]
    pub fn single_map() {
        let mut a = HashMap::new();
        a.insert("my_key", "abcdef");

        let val = serialize_label(a).unwrap();
        assert_eq!(val, "{my_key = \"abcdef\"}");
    }

    #[test]
    pub fn multi_map() {
        let mut a = std::collections::BTreeMap::new();
        a.insert("my_key", "abcdef");
        a.insert("my_other_key", "1234");

        let val = serialize_label(a).unwrap();
        assert_eq!(val, "{my_key = \"abcdef\", my_other_key = \"1234\"}");
    }

    #[test]
    pub fn quoted_value() {
        let mut a = HashMap::new();
        a.insert(
            "my_key",
            "this is a story all about how my \"life got flip turned upside down\"",
        );

        let val = serialize_label(a).unwrap();
        assert_eq!(val, "{my_key = \"this is a story all about how my \\\"life got flip turned upside down\\\"\"}");
    }

    #[test]
    pub fn invalid_key() {
        let mut a = HashMap::new();
        a.insert(
            "my_key!",
            "this is a story all about how my \"life got flip turned upside down\"",
        );

        let actual = serialize_label(a);
        assert!(
            actual.is_err(),
            "invalid label key should've returned an Err"
        );
    }
}
