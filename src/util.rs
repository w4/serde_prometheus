use std::ops::Deref;
use std::{
    fmt::{Display, Formatter},
    sync::Arc,
};

/// A slight optimisation to `Cow`, which stores the "owned" version of a string as an
/// `Arc<str>` so cloning is less expensive.
#[derive(Debug, Clone, Eq)]
pub enum CowArcStr<'a> {
    Borrowed(&'a str),
    Owned(Arc<str>),
}

impl Deref for CowArcStr<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self {
            CowArcStr::Borrowed(v) => v,
            CowArcStr::Owned(v) => v.as_ref(),
        }
    }
}

impl PartialEq for CowArcStr<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl PartialEq<&str> for CowArcStr<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        match self {
            CowArcStr::Borrowed(v) => *v == *other,
            CowArcStr::Owned(v) => &**v == *other,
        }
    }
}

impl<'a> Display for CowArcStr<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CowArcStr::Borrowed(inner) => inner.fmt(f),
            CowArcStr::Owned(inner) => inner.fmt(f),
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
        Self::Owned(Arc::from(value))
    }
}
