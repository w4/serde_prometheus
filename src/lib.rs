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
//! | .            | The default behaviour of `serde_prometheus 0.1` is to append the collected stack to the next value in `path` (as if an extra `<` was added to your modifiers), to prevent this use this modifier. This has no effect in labels. |
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
//! ## Internal overrides
//!
//! `serialize_newtype_struct` supports an extra section to allow overriding of internal operations,
//! as follows:
//!
//! ```txt
//! keymodifiers|key=value,key2=value2|:internal=abc
//! ```
//!
//! Internal overrides are always prefixed with a `:` to make it plainly obvious they're not labels,
//! a list of internal overrides available for use are as follows:
//!
//! | Override   | Description |
//! | ---------- | ----------- |
//! | :namespace | Overrides the global namespace with a new value for that struct and any leaves under it |
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

#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::missing_errors_doc)]

mod error;
pub(crate) mod modifiers;
pub(crate) mod util;

use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
    io::Write,
};

use serde::{
    ser::{Impossible, SerializeMap, SerializeSeq, SerializeStruct},
    Serialize,
};

use crate::{
    modifiers::{
        apply_path_modifications, BuiltLabel, LabelBehaviour, LabelStack, LabelStackWithPath,
        ParsedModifiersList, PathStack, MAX_ALLOWED_SUFFIX_PARTS, MAX_DEPTH,
    },
    util::CowArcStr,
};

pub use crate::error::Error;

/// Builds a string for a `metered::MetricRegistry` in Prometheus' simple text-based exposition
/// format.
#[inline]
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
    let mut output = Vec::with_capacity(std::mem::size_of_val(value) * 8 * 12);

    write(&mut output, value, namespace, global_labels)?;

    Ok(String::from_utf8(output)?)
}

/// Outputs a `metered::MetricRegistry` in Prometheus' simple text-based exposition
/// format.
#[inline]
pub fn write<'a, T, L, LI, W: Write>(
    output: &mut W,
    value: &T,
    namespace: Option<&str>,
    global_labels: L,
) -> Result<(), Error>
where
    T: ?Sized + Serialize,
    LI: Borrow<(&'a str, &'a str)>,
    L: IntoIterator<Item = LI>,
{
    let mut serializer = Serializer::new(output, namespace);

    // consume the first label position and fill it with our global labels, these can
    // be overridden or appended to as `serialize_newtype_struct` likes
    serializer.labels.push(global_labels.into_iter().map(|v| {
        let (key, value) = v.borrow();

        Ok(BuiltLabel {
            behaviour: LabelBehaviour::Replace,
            key,
            value: CowArcStr::Borrowed(value),
        })
    }))?;

    value.serialize(&mut serializer)?;

    Ok(())
}

struct Serializer<'a, W: Write> {
    path: PathStack<'a>,
    labels: LabelStack<'a>,
    output: W,
    modifier_cache: HashMap<&'a str, ParsedModifiersList<'a>>,
    namespace: Option<CowArcStr<'a>>,
    key: heapless::Vec<heapless::Vec<CowArcStr<'a>, { MAX_ALLOWED_SUFFIX_PARTS }>, 12>,
    prevent_key_modification: bool,
}

impl<'a, W: Write> Serializer<'a, W> {
    #[inline]
    pub(crate) fn new(output: W, namespace: Option<&'a str>) -> Self {
        Self {
            path: PathStack::default(),
            labels: LabelStack::default(),
            output,
            namespace: namespace.map(CowArcStr::Borrowed),
            modifier_cache: HashMap::default(),
            key: heapless::Vec::<_, MAX_ALLOWED_SUFFIX_PARTS>::default(),
            prevent_key_modification: false,
        }
    }

    /// Writes a value (`v`) to the output buffer.
    ///
    /// This will write the full:
    ///
    /// ```text
    /// namespace_abc{def = "123"} 3
    /// ```
    ///
    /// format, along with a new line.
    #[inline]
    pub fn write_value<S: Display>(&mut self, v: S) -> Result<(), Error> {
        if let Some(namespace) = &self.namespace {
            write!(self.output, "{namespace}_").map_err(Error::Write)?;
        }

        let mut prefix = "";

        // a bit of carry over from serde_prometheus-0.1 for compatibility, if the
        // `prevent_key_modification` modifier wasn't given for the current stack
        // then serde_prometheus will attempt to temporarily pop off another value
        // and prepends it to the name before adding it back to the stack again
        let popped_path = if self.prevent_key_modification {
            None
        } else if let Some(last_set_index) = self.path.last_set_index() {
            self.path
                .get_mut(last_set_index)
                .and_then(Option::take)
                .map(|v| (last_set_index, v))
        } else {
            None
        };

        // if a path was just popped off, then prefix it to the rest of the
        // metric name
        if let Some((_, path)) = &popped_path {
            write!(self.output, "{prefix}{path}").map_err(Error::Write)?;
            prefix = "_";
        }

        // loop over each stack entry...
        for key_stack in &self.key {
            // and write any keys they added to the metric name
            for key in key_stack {
                write!(self.output, "{prefix}{key}").map_err(Error::Write)?;

                if prefix.is_empty() {
                    prefix = "_";
                }
            }
        }

        // combines `self.path` and `self.labels` without the temporary allocation,
        // this will also deal either `self.path`, `self.labels` or both not having
        // any readable values in them
        let labels = LabelStackWithPath {
            path_stack: &self.path,
            label_stack: &self.labels,
        };

        // write the labels and value
        writeln!(self.output, "{labels} {v}").map_err(Error::Write)?;

        // add the path we've just removed for the metric name back into the path
        if let Some((popped_index, popped_path)) = popped_path {
            self.path[popped_index] = Some(popped_path);
        }

        Ok(())
    }
}

impl<W: Write> serde::Serializer for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Self;
    type SerializeTuple = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleStruct = Impossible<Self::Ok, Self::Error>;
    type SerializeTupleVariant = Impossible<Self::Ok, Self::Error>;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;

    #[inline]
    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.write_value(u8::from(v))
    }

    #[inline]
    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.write_value(v)
    }

    #[inline]
    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(Error::NotSupported("char"))
    }

    #[inline]
    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(Error::NotSupported("&str"))
    }

    #[inline]
    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(Error::NotSupported("&[u8]"))
    }

    #[inline]
    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }

    #[inline]
    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(Error::NotSupported("()"))
    }

    #[inline]
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(Error::NotSupported("unit struct"))
    }

    #[inline]
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(Error::NotSupported("unit variant"))
    }

    #[inline]
    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        // fetch the modifiers from the cache if the `name` has already been parsed, otherwise parse
        // and cache it
        let modifiers = match self.modifier_cache.entry(name) {
            Entry::Occupied(inner) => inner.into_mut(),
            Entry::Vacant(inner) => inner.insert(
                ParsedModifiersList::parse(name)
                    .map_err(|e| Error::ParseModifiers(name, e.to_string()))?
                    .1,
            ),
        };

        let old_namespace = if let Some(new_namespace) = modifiers.internal.namespace.as_ref() {
            let old_namespace = self.namespace.take();
            self.namespace = Some(new_namespace.clone());
            Some(old_namespace)
        } else {
            None
        };

        // push to the label stack if the modifiers contained new labels
        let has_label_modifiers = !modifiers.labels.is_empty();
        if has_label_modifiers {
            let labels = modifiers
                .labels
                .iter()
                .filter_map(|definition| definition.build_label(&self.path).transpose());
            self.labels.push(labels)?;
        }

        // replace self.path if the modifiers had key modifiers
        let (old_path, old_prevent_key_mod) = if modifiers.key_modifiers.is_empty() {
            (None, None)
        } else {
            // take a copy of the path stack, so we can restore it later. this only contains
            // static references or Arcs so it's relatively inexpensive
            let mut new_path = self.path.clone();

            // mutate the new path
            let suffix = apply_path_modifications(&mut new_path, &modifiers.key_modifiers)?;

            // if the caller prepended some keys, write them out to the key
            self.key.push(suffix.names).map_err(|val| {
                Error::TooManyKeyPushers(MAX_ALLOWED_SUFFIX_PARTS, format!("{val:?}"))
            })?;

            // swap out the current path with the one we've just mutated and return the
            // old value so we can reset back to it later
            (
                Some(std::mem::replace(&mut self.path, new_path)),
                suffix
                    .prevent_key_modification
                    .then(|| std::mem::replace(&mut self.prevent_key_modification, true)),
            )
        };

        value.serialize(&mut *self)?;

        // we're all done with these modifiers so lets clean up after ourselves
        if has_label_modifiers {
            self.labels.pop();
        }

        // if there was no path stack, there's no reason for us to reset it back
        if let Some(old_path) = old_path {
            self.path = old_path;
            self.key.pop();
        }

        if let Some(old_prevent_key_mod) = old_prevent_key_mod {
            self.prevent_key_modification = old_prevent_key_mod;
        }

        if let Some(old_namespace) = old_namespace {
            self.namespace = old_namespace;
        }

        Ok(())
    }

    #[inline]
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
        Err(Error::NotSupported("newtype variant"))
    }

    #[inline]
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(self)
    }

    #[inline]
    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(Error::NotSupported("tuple"))
    }

    #[inline]
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(Error::NotSupported("tuple struct"))
    }

    #[inline]
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(Error::NotSupported("tuple variant"))
    }

    #[inline]
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(self)
    }

    #[inline]
    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(self)
    }

    #[inline]
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(Error::NotSupported("struct variant"))
    }
}

impl<W: Write> SerializeStruct for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.path
            .push(key.into())
            .map_err(|_| Error::TooManyNestedMetrics(MAX_DEPTH, key, self.path.to_string()))?;
        value.serialize(&mut **self)?;
        self.path.pop();
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<W: Write> SerializeSeq for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;

    #[inline]
    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

impl<W: Write> SerializeMap for &mut Serializer<'_, W> {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        self.path
            .push(
                key.serialize(serde_plain::Serializer)
                    .map_err(Error::SerializeMapKey)?
                    .into(),
            )
            .map_err(|_| Error::TooManyNestedMetrics(MAX_DEPTH, "map", self.path.to_string()))?;
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)?;
        self.path.pop();
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    mod serializer {
        use metered::HitCount;
        use serde::Serialize;
        use std::collections::BTreeMap;

        #[test]
        fn basic() {
            #[derive(Serialize)]
            pub struct Root {
                something: Test,
            }

            #[derive(Serialize)]
            pub struct Test {
                hello_world: Wrapper,
            }

            pub struct Wrapper(bool);

            impl Serialize for Wrapper {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_newtype_struct("<|hello=world", &self.0)
                }
            }

            let input = Root {
                something: Test {
                    hello_world: Wrapper(true),
                },
            };

            #[allow(clippy::needless_borrow)]
            let actual = crate::to_string(&input, None, &[]).unwrap();

            assert_eq!(actual, "something_hello_world{hello = \"world\"} 1\n");
        }

        #[test]
        fn overflows_labels() {
            #[derive(Serialize)]
            pub struct Root {
                something: Test,
            }

            pub struct Test {
                hello_world: Wrapper,
            }

            impl Serialize for Test {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_newtype_struct("|a=base", &self.hello_world)
                }
            }

            pub struct Wrapper(bool);

            impl Serialize for Wrapper {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_newtype_struct(
                        "<|a[::]=1,b=2,c=3,d=4,e=5,f=6,g=7,h=8,i=9,j=10,k=12,l=12,m=13,n=14,o=15,p=16,q=17,r=18,s=19,t=20,u=21,v=22,w=23,x=24,y=25,z=26,aa=27,ab=28,ac=29,ad=30,ae=31,af=32,ag=33",
                        &self.0,
                    )
                }
            }

            let input = Root {
                something: Test {
                    hello_world: Wrapper(true),
                },
            };

            #[allow(clippy::needless_borrow)]
            let actual = crate::to_string(&input, None, &[]);
            insta::assert_snapshot!(actual.unwrap_err());
        }

        #[test]
        fn skip_key_modification() {
            #[derive(Serialize)]
            pub struct Root {
                something: Test,
            }

            #[derive(Serialize)]
            pub struct Test {
                hello_world: Wrapper,
            }

            pub struct Wrapper(bool);

            impl Serialize for Wrapper {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_newtype_struct("<.|hello=world", &self.0)
                }
            }

            let input = Root {
                something: Test {
                    hello_world: Wrapper(true),
                },
            };

            #[allow(clippy::needless_borrow)]
            let actual = crate::to_string(&input, None, &[]).unwrap();

            assert_eq!(
                actual,
                "hello_world{hello = \"world\", path = \"something\"} 1\n"
            );
        }

        #[test]
        fn geoip() {
            #[derive(Debug, Default)]
            pub struct CountryCodeCount(HitCount);

            impl Serialize for CountryCodeCount {
                fn serialize<S: serde::ser::Serializer>(
                    &self,
                    serializer: S,
                ) -> Result<S::Ok, S::Error> {
                    serializer.serialize_newtype_struct("!!<.|country_code==<", &self.0)
                }
            }

            #[derive(Default, Serialize)]
            struct GeoIpMetrics {
                country_code: BTreeMap<String, CountryCodeCount>,
            }

            #[derive(Default, Serialize)]
            struct Metrics {
                geoip: GeoIpMetrics,
            }

            let mut metrics = Metrics::default();

            for ch in 'A'..='Z' {
                metrics
                    .geoip
                    .country_code
                    .insert(format!("A{ch}"), CountryCodeCount::default());
            }

            let actual = crate::to_string(&metrics, Some("example"), [("foo", "bar")]).unwrap();
            insta::assert_snapshot!(actual);
        }
    }

    mod namespace {
        use serde::Serialize;

        #[test]
        fn override_works() {
            #[derive(Serialize)]
            pub struct Root {
                something: Test,
            }

            #[derive(Serialize)]
            pub struct Test {
                hello_world: Wrapper,
                something_else: bool,
            }

            pub struct Wrapper(bool);

            impl Serialize for Wrapper {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_newtype_struct(
                        "<|hello=\"wor\\\"ld\"|:namespace=override",
                        &self.0,
                    )
                }
            }

            let input = Root {
                something: Test {
                    hello_world: Wrapper(true),
                    something_else: true,
                },
            };

            #[allow(clippy::needless_borrow)]
            let actual = crate::to_string(&input, Some("hello_world"), &[]).unwrap();

            insta::assert_snapshot!(actual);
        }
    }

    mod metered_integ {
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
        #[allow(clippy::pedantic)]
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

            insta::assert_snapshot!(ret);
        }

        #[test]
        #[allow(clippy::pedantic)]
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

            insta::assert_snapshot!(ret);
        }
    }

    mod compat {
        use serde::{Serialize, Serializer};

        #[test]
        fn ensure_skips_lazy_evaluated_for_01_compat() {
            #[derive(Serialize)]
            pub struct A {
                b: B,
            }

            pub struct B;

            impl Serialize for B {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: Serializer,
                {
                    serializer.serialize_newtype_struct("-----|", &2)
                }
            }

            #[allow(clippy::needless_borrow)]
            let ret = crate::to_string(&A { b: B }, Some("global"), &[]).unwrap();

            assert_eq!(ret, "global_b 2\n");
        }
    }
}
