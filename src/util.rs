use std::fmt::Write;
use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

/// A slight optimisation to `Cow`, which stores the "owned" version of a string as an
/// `Arc<str>` so cloning is less expensive.
#[derive(Debug, Clone, Eq)]
pub enum CowArcStr<'a> {
    Borrowed(&'a str),
    Owned(Rc<str>),
    SignedNumber(i64),
    UnsignedNumber(u64),
    Char(char),
}

impl Default for CowArcStr<'_> {
    fn default() -> Self {
        Self::Borrowed("")
    }
}

impl PartialEq for CowArcStr<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CowArcStr::Borrowed(v), _) => <Self as PartialEq<&str>>::eq(other, v),
            (CowArcStr::Owned(v), _) => <Self as PartialEq<&str>>::eq(other, &v.as_ref()),
            (CowArcStr::SignedNumber(v), CowArcStr::SignedNumber(o)) => v == o,
            (CowArcStr::UnsignedNumber(v), CowArcStr::UnsignedNumber(o)) => v == o,
            (CowArcStr::SignedNumber(v), _) => {
                let mut buf = itoa::Buffer::new();
                <Self as PartialEq<&str>>::eq(other, &buf.format(*v))
            }
            (CowArcStr::UnsignedNumber(v), _) => {
                let mut buf = itoa::Buffer::new();
                <Self as PartialEq<&str>>::eq(other, &buf.format(*v))
            }
            (CowArcStr::Char(c), CowArcStr::Char(o)) => c == o,
            (CowArcStr::Char(c), _) => {
                let mut buf = [0_u8; 4];
                <Self as PartialEq<&str>>::eq(other, &&*c.encode_utf8(&mut buf))
            }
        }
    }
}

impl PartialEq<&str> for CowArcStr<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        match self {
            CowArcStr::Borrowed(v) => *v == *other,
            CowArcStr::Owned(v) => &**v == *other,
            CowArcStr::SignedNumber(v) => {
                let mut buf = itoa::Buffer::new();
                buf.format(*v) == *other
            }
            CowArcStr::UnsignedNumber(v) => {
                let mut buf = itoa::Buffer::new();
                buf.format(*v) == *other
            }
            CowArcStr::Char(c) => {
                let mut buf = [0_u8; 4];
                c.encode_utf8(&mut buf) == *other
            }
        }
    }
}

impl Display for CowArcStr<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CowArcStr::Borrowed(inner) => f.write_str(inner),
            CowArcStr::Owned(inner) => f.write_str(inner),
            CowArcStr::SignedNumber(inner) => inner.fmt(f),
            CowArcStr::UnsignedNumber(inner) => inner.fmt(f),
            CowArcStr::Char(inner) => f.write_char(*inner),
        }
    }
}

impl<'a> From<&'a str> for CowArcStr<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self::Borrowed(value)
    }
}

impl From<String> for CowArcStr<'_> {
    #[inline]
    fn from(value: String) -> Self {
        Self::Owned(Rc::from(value))
    }
}

impl From<u64> for CowArcStr<'_> {
    #[inline]
    fn from(value: u64) -> Self {
        Self::UnsignedNumber(value)
    }
}

impl From<i64> for CowArcStr<'_> {
    #[inline]
    fn from(value: i64) -> Self {
        Self::SignedNumber(value)
    }
}

impl From<char> for CowArcStr<'_> {
    #[inline]
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}
