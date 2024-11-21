use std::{
    fmt::{Debug, Display, Formatter, Write},
    ops::{Deref, DerefMut},
    str::FromStr,
};

use heapless::{Deque, FnvIndexMap, Vec as ArrayVec};
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_till, take_until, take_while},
    character::complete::{anychar, none_of},
    combinator::{cut, map, map_parser, map_res, opt, peek},
    error::ErrorKind,
    multi::{fold_many0, separated_list0},
    sequence::{delimited, preceded, separated_pair, terminated},
    IResult,
};

use crate::{error::Error, util::CowArcStr};

pub const MAX_ALLOWED_MODIFIERS: usize = 16;
pub const MAX_ALLOWED_LABELS: usize = 32;
pub const MAX_DEPTH: usize = 128;
pub const MAX_ALLOWED_SUFFIX_PARTS: usize = 12;
pub const MAX_ALLOWED_LABEL_OVERRIDES: usize = 32;

/// Execute the given `Modifier`s against the `PathStack` in order to retrieve the requested
/// value. This is used for fetching a single value for labels, and won't modify state.
#[inline]
pub fn get_value_from_path_for_modifiers<'a>(
    path: &PathStack<'a>,
    modifiers: &[Modifier],
) -> Result<Option<CowArcStr<'a>>, Error> {
    let mut index_rev = path.last_set_index();

    for modifier in modifiers {
        match modifier {
            Modifier::Prepend => {
                return index_rev
                    .and_then(|v| path.get(v))
                    .cloned()
                    .flatten()
                    .ok_or_else(|| Error::ExhaustedPathStack(path.to_string(), modifiers.to_vec()))
                    .map(Some);
            }
            Modifier::Exclude | Modifier::Skip => {
                index_rev = index_rev.and_then(|v| v.checked_sub(1));
            }
            Modifier::PreventKeyModification => {}
        }
    }

    Ok(None)
}

/// The requested modifiers from the key.
#[derive(Debug)]
pub struct PathModifications<'a> {
    /// The stack items to append to the metric name.
    pub names: ArrayVec<CowArcStr<'a>, MAX_ALLOWED_SUFFIX_PARTS>,
    /// Prevents `serde_prometheus` from popping another value off the stack to append to
    /// the name after our appends.
    pub prevent_key_modification: bool,
}

/// Applies multiple modifiers and modifies the path stack to continue
/// passing down the stack.
#[inline]
pub fn apply_path_modifications<'a>(
    path: &mut PathStack<'a>,
    modifiers: &[Modifier],
) -> Result<PathModifications<'a>, Error> {
    // initiate the position of the cursor in the `path` at the position of the last
    // `Some` value
    let mut index_rev = path.last_set_index();

    // initiate the state that the modifiers will fill
    let mut names = Deque::<_, 12>::new();
    let mut prevent_key_modification = false;

    // apply each modifier
    for modifier in modifiers {
        match modifier {
            Modifier::Prepend => {
                let value = index_rev
                    .and_then(|v| path.get_mut(v).map(Option::take))
                    .flatten()
                    .ok_or_else(|| {
                        Error::ExhaustedPathStack(path.to_string(), modifiers.to_vec())
                    })?;
                names
                    .push_front(value)
                    .map_err(|_| Error::ExceededPathSize)?;
                index_rev = path.last_set_index();
            }
            Modifier::Exclude => {
                if let Some(path) = index_rev.and_then(|v| path.get_mut(v)) {
                    *path = None;
                } else {
                    return Err(Error::ExhaustedPathStack(
                        path.to_string(),
                        modifiers.to_vec(),
                    ));
                }

                index_rev = path.last_set_index();
            }
            Modifier::Skip => {
                index_rev = index_rev.and_then(|v| v.checked_sub(1));
            }
            Modifier::PreventKeyModification => {
                prevent_key_modification = true;
            }
        }
    }

    Ok(PathModifications {
        names: names.into_iter().collect(),
        prevent_key_modification,
    })
}

/// A very basic stack for keeping track of the current `path` (which is the name of each
/// field down to the current value).
#[derive(Default, Debug, Clone)]
pub struct PathStack<'a>(ArrayVec<Option<CowArcStr<'a>>, MAX_DEPTH>);

impl<'a> PathStack<'a> {
    /// Pushes a new value to the stack
    #[inline]
    pub fn push(&mut self, value: CowArcStr<'a>) -> Result<(), Option<CowArcStr<'a>>> {
        self.0.push(Some(value))
    }

    /// Checks if the current path stack is empty
    #[inline]
    fn is_empty(&self) -> bool {
        self.0.is_empty() || self.0.iter().all(Option::is_none)
    }

    #[inline]
    pub fn last_set_index(&self) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .rfind(|(_i, v)| v.is_some())
            .map(|(i, _)| i)
    }
}

impl<'a> Deref for PathStack<'a> {
    type Target = ArrayVec<Option<CowArcStr<'a>>, MAX_DEPTH>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PathStack<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for PathStack<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, part) in self.0.iter().filter_map(Option::as_ref).enumerate() {
            if i > 0 {
                f.write_char('/')?;
            }

            Display::fmt(&part, f)?;
        }

        Ok(())
    }
}

/// A helper struct for adding a `PathStack` to a `LabelStack` without `to_string`ing them
/// separately.
///
/// The `Display` implementation for this struct will output a set of labels in prometheus format,
/// or nothing if both the `PathStack` and `LabelStack` are empty.
pub struct LabelStackWithPath<'a> {
    pub path_stack: &'a PathStack<'a>,
    pub label_stack: &'a LabelStack<'a>,
}

impl Display for LabelStackWithPath<'_> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let path_stack_is_empty = self.path_stack.is_empty();
        let label_stack_is_empty = self.label_stack.is_empty();

        if path_stack_is_empty && label_stack_is_empty {
            return Ok(());
        }

        let label_stack_suffix = if label_stack_is_empty || path_stack_is_empty {
            ""
        } else {
            ", "
        };

        let (path_stack_prefix, path_stack_suffix) = if path_stack_is_empty {
            ("", "")
        } else {
            ("path = \"", "\"")
        };

        write!(
            f,
            "{{{}{label_stack_suffix}{path_stack_prefix}{}{path_stack_suffix}}}",
            self.label_stack, self.path_stack,
        )?;

        Ok(())
    }
}

pub type LabelName<'a> = &'a str;
pub type HighestSetValue = Option<usize>;
pub type LabelValuesByOffset<'a> = [Option<LabelKind<'a>>; MAX_ALLOWED_LABEL_OVERRIDES];

#[derive(Clone, Debug)]
pub enum LabelKind<'a> {
    Single(CowArcStr<'a>),
    Concatenated(rpds::Queue<(&'a str, CowArcStr<'a>)>),
}

impl<'a> LabelKind<'a> {
    pub fn push(&mut self, glue: &'a str, value: CowArcStr<'a>) {
        // if we have a single value, we need to upgrade it to a concatenated value
        match self {
            LabelKind::Single(previous) => {
                let mut inner = rpds::Queue::new();
                inner.enqueue_mut(("", std::mem::take(previous)));
                inner.enqueue_mut((glue, value));
                *self = LabelKind::Concatenated(inner);
            }
            LabelKind::Concatenated(inner) => inner.enqueue_mut((glue, value)),
        }
    }
}

impl Display for LabelKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelKind::Single(v) => write_sanitized_string(f, v),
            LabelKind::Concatenated(v) => {
                for (idx, (glue, value)) in v.iter().enumerate() {
                    if idx != 0 {
                        write!(f, "{glue}")?;
                    }

                    write_sanitized_string(f, value)?;
                }

                Ok(())
            }
        }
    }
}

fn write_sanitized_string(f: &mut Formatter<'_>, s: &str) -> std::fmt::Result {
    for c in s.chars() {
        match c {
            '\\' => f.write_str("\\\\")?,
            '"' => f.write_str("\\\"")?,
            '\n' => f.write_str("\\n")?,
            _ => f.write_char(c)?,
        }
    }

    Ok(())
}

/// Labels that have been added whilst traversing each field until the current value.
///
/// This allows for overriding of and concatenation to labels that came before the current
/// value.
///
/// This is implemented using a `current_offset` field to determine which value should be popped
/// off the stack next, the `stack` itself is a fixed-length array which is referred to by the
/// `current_offset` - this allows us to know exactly which set of labels were set by a single
/// call to `push` and allows us to `pop` them all off the stack in `O(n)` time.
///
/// Each label also contains a `HighestSetValue`, which contains the last-set offset in the array,
/// so each "latest" label value can be looked up in `O(1)` time.
#[derive(Default)]
pub struct LabelStack<'a> {
    stack:
        FnvIndexMap<LabelName<'a>, (HighestSetValue, LabelValuesByOffset<'a>), MAX_ALLOWED_LABELS>,
    current_offset: usize,
}

impl<'a> LabelStack<'a> {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty() || self.stack.values().all(|(idx, _)| idx.is_none())
    }

    /// Push newly built labels to the stack.
    #[inline]
    pub fn push(
        &mut self,
        new: impl Iterator<Item = Result<BuiltLabel<'a>, Error>>,
    ) -> Result<(), Error> {
        for label in new {
            let label = label?;

            // avoid using entry API until https://github.com/japaric/heapless/issues/360 is resolved
            if !self.stack.contains_key(label.key) {
                self.stack
                    .insert(label.key, Default::default())
                    .map_err(|_| {
                        Error::TooManyKeys(
                            MAX_ALLOWED_LABELS,
                            format!(
                                "{:?}",
                                self.stack
                                    .iter()
                                    .map(|v| (v.0, v.1 .1.iter().flatten().collect::<Vec<_>>()))
                                    .collect::<Vec<_>>()
                            ),
                            label.key.to_string(),
                        )
                    })?;
            }

            // SAFETY: we insert immediately before if the key was not present
            let entry = self.stack.get_mut(label.key).unwrap();

            let new_label = match label.behaviour {
                LabelBehaviour::Replace => LabelKind::Single(label.value),
                LabelBehaviour::Append(glue) => {
                    if let Some(previous) = entry.0.and_then(|offset| entry.1[offset].as_ref()) {
                        // grab the last set value and prepend it to our string using the glue given
                        // by the caller
                        let mut new = previous.clone();
                        new.push(glue, label.value);
                        new
                    } else {
                        LabelKind::Single(label.value)
                    }
                }
            };

            // update the highest offset for this label to ours, and insert our value
            entry.0 = Some(self.current_offset);
            entry.1[self.current_offset] = Some(new_label);
        }

        self.current_offset += 1;

        Ok(())
    }

    /// Pop the last set of labels from the stack.
    #[inline]
    pub fn pop(&mut self) {
        self.current_offset = self.current_offset.saturating_sub(1);

        for (highest, v) in self.stack.values_mut() {
            let cell = v[self.current_offset].take();

            // there was no value for the current label on our offset, so there's no need
            // for us to reset the latest-highest-offset
            if cell.is_none() {
                continue;
            }

            *highest = Self::next_back(v).map(|(offset, _)| offset);
        }

        self.stack.retain(|_, (highest, _)| highest.is_some());
    }

    /// Grabs the next available string in the array that is set, starting from the end of the
    /// array and working its way back to the beginning.
    #[inline]
    fn next_back<'b>(
        array: &'b [Option<LabelKind<'b>>],
    ) -> Option<(usize, &'b Option<LabelKind<'b>>)> {
        array.iter().enumerate().rfind(|(_, val)| val.is_some())
    }
}

impl<'a> Display for LabelStack<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut prefix = "";

        for (k, (offset, v)) in &self.stack {
            // fetch the latest value for the label
            let Some(v) = offset.and_then(|offset| v[offset].as_ref()) else {
                continue;
            };

            write!(f, r#"{prefix}{k} = "{v}""#)?;

            if prefix.is_empty() {
                prefix = ", ";
            }
        }

        Ok(())
    }
}

/// A list of parsed modifiers that have not yet been built into metric names & label values. This
/// is useful as an intermediary state for caching the built IR.
#[derive(Eq, PartialEq, Debug, Default)]
pub struct ParsedModifiersList<'a> {
    pub key_modifiers: ArrayVec<Modifier, MAX_ALLOWED_MODIFIERS>,
    pub labels: Vec<LabelDefinition<'a>>,
    pub internal: InternalModifiers<'a>,
}

impl<'a> ParsedModifiersList<'a> {
    /// Attempts to parse a "modifier list", which is passed to `serde_prometheus` in the following
    /// format via `serialize_newtype_struct`:
    ///
    /// ```text
    /// !<|my_label==!<,my_other_label=test|:namespace=abc
    /// ```
    #[inline]
    pub fn parse(input: &'a str) -> IResult<&str, ParsedModifiersList<'a>> {
        if peek(ensure_char_is_parsable)(input).is_err() {
            return Ok((input, ParsedModifiersList::default()));
        }

        let (input, key_modifiers) =
            terminated(map_parser(take_until("|"), take_many_modifier), tag("|"))(input)?;
        let (input, labels) = separated_list0(
            tag(","),
            map(
                separated_pair(parse_label_key, tag("="), ParsedLabel::parse),
                |((behaviour, key), value)| LabelDefinition {
                    behaviour,
                    key,
                    value,
                },
            ),
        )(input)?;

        let (input, internal) = opt(preceded(tag("|"), cut(InternalModifiers::parse)))(input)?;

        Ok((
            input,
            ParsedModifiersList {
                key_modifiers,
                labels,
                internal: internal.unwrap_or_default(),
            },
        ))
    }
}

/// Provides options to allow consumers to override internal state.
#[derive(Eq, PartialEq, Debug, Default)]
pub struct InternalModifiers<'a> {
    /// Overrides the global namespace with a new value
    pub namespace: Option<CowArcStr<'a>>,
}

impl<'a> InternalModifiers<'a> {
    /// Parses a list of internal modifiers, e.g.
    ///
    /// ```text
    /// :namespace=abc,:something_else="def"
    /// ```
    #[inline]
    pub fn parse(input: &'a str) -> IResult<&str, Self> {
        fold_many0(
            separated_pair(
                map_res(
                    preceded(tag(":"), take_while(char::is_alphanumeric)),
                    InternalModifierKind::from_str,
                ),
                tag("="),
                parse_string,
            ),
            Self::default,
            |mut acc, (k, v)| {
                match k {
                    InternalModifierKind::Namespace => acc.namespace = Some(v),
                }

                acc
            },
        )(input)
    }
}

/// Internal parsing state for each `InternalModifiers` value to allow for returning
/// errors early in the parsing pipeline when an invalid value is seen, since
/// `fold_many0` is infallible.
///
/// See [`InternalModifiers`] for an explanation of what each modifier does.
enum InternalModifierKind {
    Namespace,
}

impl FromStr for InternalModifierKind {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "namespace" => Ok(Self::Namespace),
            _ => Err(Error::UnknownInternalModifier(s.to_string())),
        }
    }
}

/// Parses a label name, this is typically an alphanumeric (and underscore) string, but can also
/// be prefixed with `[abc]` to trigger the `LabelBehaviour::Replace("abc")` behaviour.
fn parse_label_key(input: &str) -> IResult<&str, (LabelBehaviour<'_>, &str)> {
    let (input, key) = take_while(|v: char| v.is_alphanumeric() || v == '_')(input)?;
    let (input, glue) = opt(delimited(tag("["), take_until("]"), tag("]")))(input)?;

    Ok((
        input,
        (
            glue.map_or_else(|| LabelBehaviour::Replace, LabelBehaviour::Append),
            key,
        ),
    ))
}

/// Modifiers are executed left to right and act upon the "stack" of field names that came before
/// it in order to make up a label value/metric name.
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Modifier {
    /// Take the current value from the stack and reset the stack pointer back to the top
    Prepend,
    /// Exclude the current value from the stack whilst serialising the current value and moves the
    /// stack pointer down one value, this is typically used for removing the value from the `path`
    /// label but, when used at the top of the stack, can be used to exclude it from both the metric
    /// name and `path` label.
    ///
    /// This operator does nothing when used in labels, and simply sets the stack pointer back to
    /// the top.
    Exclude,
    /// Moves the stack pointer down one value.
    Skip,
    /// Prevents `serde_prometheus` from modifying the metric key after the caller has written to it
    /// (default behaviour is to act as if another `<` was added to the modifiers)
    PreventKeyModification,
}

impl Modifier {
    /// Parses a modifier used in `serialize_newtype_struct` for label/metric name serialization.
    #[inline]
    pub fn parse(v: char) -> Result<Self, std::io::Error> {
        match v {
            '<' => Ok(Self::Prepend),
            '!' => Ok(Self::Exclude),
            '-' => Ok(Self::Skip),
            '.' => Ok(Self::PreventKeyModification),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "unknown modifier",
            )),
        }
    }
}

/// The behaviour that the new label should abide by if another label of the same name exists
/// further up the `LabelStack`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LabelBehaviour<'a> {
    /// Replace any previous labels of the same name.
    Replace,
    /// Append to the previous label of the same name (`[::]` after a label name will expand to
    /// `LabelBehaviour::Append("::")`.
    Append(&'a str),
}

/// A label which has been expanded from its IR based on a field's path stack, ready to
/// be written out as a label to prometheus
pub struct BuiltLabel<'a> {
    pub behaviour: LabelBehaviour<'a>,
    pub key: &'a str,
    pub value: CowArcStr<'a>,
}

/// A label, as defined in code, that has not yet been built (ie. modifiers expanded).
///
/// This is useful as an intermediary state for caching the parsed IR.
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct LabelDefinition<'a> {
    pub behaviour: LabelBehaviour<'a>,
    pub key: &'a str,
    pub value: ParsedLabel<'a>,
}

impl<'a> LabelDefinition<'a> {
    #[inline]
    pub fn build_label(&self, path: &PathStack<'a>) -> Result<Option<BuiltLabel<'a>>, Error> {
        match &self.value {
            ParsedLabel::Modifiers(modifiers) => {
                let Some(value) = get_value_from_path_for_modifiers(path, modifiers)? else {
                    return Ok(None);
                };

                Ok(Some(BuiltLabel {
                    behaviour: self.behaviour,
                    key: self.key,
                    value,
                }))
            }
            ParsedLabel::Fixed(value) => Ok(Some(BuiltLabel {
                behaviour: self.behaviour,
                key: self.key,
                value: value.clone(),
            })),
        }
    }
}

/// The parsed value of a prometheus metric label (`{abc="def"}`).
#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ParsedLabel<'a> {
    /// Defers to `Modifier`s in order to build the label.
    Modifiers(ArrayVec<Modifier, MAX_ALLOWED_MODIFIERS>),
    /// A fixed-text string that should be set as the label.
    Fixed(CowArcStr<'a>),
}

impl<'a> ParsedLabel<'a> {
    /// Attempts to parse a label from `serialize_newtype_struct` using [`Self::parse_modifiers`]
    /// and then [`Self::parse_fixed`] if that fails.
    #[inline]
    pub fn parse(input: &'a str) -> IResult<&str, Self> {
        alt((Self::parse_modifiers, Self::parse_fixed))(input)
    }

    /// Attempts to parse a list of modifiers that should be used to generate the metrics label.
    ///
    /// Modifiers are disambiguated from fixed inputs by an extra `=` at the start of the string.
    #[inline]
    fn parse_modifiers(input: &'a str) -> IResult<&str, Self> {
        map(
            preceded(
                tag("="),
                map_parser(take_till(|v| matches!(v, ',' | '|')), take_many_modifier),
            ),
            Self::Modifiers,
        )(input)
    }

    /// Parses a fixed-string label that should be used to generate the metrics label.
    #[inline]
    fn parse_fixed(input: &'a str) -> IResult<&str, Self> {
        map(parse_string, ParsedLabel::Fixed)(input)
    }
}

/// Parses a string that is terminated by a comma, or a quote delimited string.
#[inline]
fn parse_string(input: &str) -> IResult<&str, CowArcStr<'_>> {
    let quoted = map(
        delimited(
            tag(r#"""#),
            alt((escaped(none_of(r#"\""#), '\\', tag(r#"""#)), tag(""))),
            tag(r#"""#),
        ),
        |matched: &str| {
            if matched.contains('"') {
                // unescape quotes, we're only going to evaluate this fixed tag once so the alloc
                // is a small price to pay as opposed to pulling in the full regex lib
                CowArcStr::Owned(matched.replace(r#"\""#, "\"").into())
            } else {
                CowArcStr::Borrowed(matched)
            }
        },
    );

    let comma_separated = map(escaped(none_of(r"\,"), '\\', tag(",")), |matched: &str| {
        if matched.contains(',') {
            // unescape commas, we're only going to evaluate this fixed tag once so the alloc
            // is a small price to pay as opposed to pulling in the full regex lib
            CowArcStr::Owned(matched.replace(r"\,", ",").into())
        } else {
            CowArcStr::Borrowed(matched)
        }
    });

    alt((quoted, comma_separated))(input)
}

/// Attempts to consume the entire input string, converting each character to a `Modifier`,
/// allowing for a total of `MAX_ALLOWED_MODIFIERS` to be parsed.
#[inline]
fn take_many_modifier(input: &str) -> IResult<&str, ArrayVec<Modifier, MAX_ALLOWED_MODIFIERS>> {
    fold_many0(take_single_modifier, ArrayVec::new, |mut acc, item| {
        acc.push(item).expect("too many modifiers (max 16)");
        acc
    })(input)
}

/// Attempts to consume the next character in the input string and convert it to a `Modifier`,
/// returns a fatal error if `Modifier::parse` fails, or a normal error if the input string is
/// empty.
#[inline]
fn take_single_modifier(input: &str) -> IResult<&str, Modifier> {
    match map_res(anychar, Modifier::parse)(input) {
        Ok(v) => Ok(v),
        // convert any errors from `Modifier::parse` to failures, so they're bubbled up to the
        // caller
        Err(nom::Err::Error(
            err @ nom::error::Error {
                code: ErrorKind::MapRes,
                ..
            },
        )) => Err(nom::Err::Failure(err)),
        Err(e) => Err(e),
    }
}

/// Ensures that the first character of the input is a "special char" meaning it's parsable using
/// `ParsedModifiersList::parse`, otherwise we'll assume it's the actual name of a newtype struct
/// and skip parsing it.
#[inline]
fn ensure_char_is_parsable(input: &str) -> IResult<&str, ()> {
    alt((map(tag("|"), |_| ()), map(take_single_modifier, |_| ())))(input)
}

#[cfg(test)]
mod test {
    macro_rules! array_vec {
        () => {
            heapless::Vec::from_slice(&[]).unwrap()
        };
        ($($x:expr),+ $(,)?) => {
            heapless::Vec::from_slice(&[$($x,)*]).unwrap()
        }
    }

    mod label_stack_with_path {
        use crate::modifiers::{
            BuiltLabel, LabelBehaviour, LabelStack, LabelStackWithPath, PathStack,
        };

        #[test]
        fn empty_path() {
            let path_stack = PathStack::default();
            let mut label_stack = LabelStack::default();

            label_stack
                .push(std::iter::once(Ok(BuiltLabel {
                    behaviour: LabelBehaviour::Replace,
                    key: "testLabel",
                    value: "test-value".into(),
                })))
                .unwrap();

            let label_stack_with_path = LabelStackWithPath {
                path_stack: &path_stack,
                label_stack: &label_stack,
            };

            assert_eq!(
                label_stack_with_path.to_string(),
                r#"{testLabel = "test-value"}"#,
            );
        }

        #[test]
        fn empty_label() {
            let mut path_stack = PathStack::default();
            let label_stack = LabelStack::default();

            path_stack.push("test".into()).unwrap();
            path_stack.push("hello".into()).unwrap();

            let label_stack_with_path = LabelStackWithPath {
                path_stack: &path_stack,
                label_stack: &label_stack,
            };

            assert_eq!(
                label_stack_with_path.to_string(),
                r#"{path = "test/hello"}"#,
            );
        }

        #[test]
        fn both_empty() {
            let path_stack = PathStack::default();
            let label_stack = LabelStack::default();

            let label_stack_with_path = LabelStackWithPath {
                path_stack: &path_stack,
                label_stack: &label_stack,
            };

            assert_eq!(label_stack_with_path.to_string(), "");
        }

        #[test]
        fn both_set() {
            let mut path_stack = PathStack::default();
            let mut label_stack = LabelStack::default();

            label_stack
                .push(std::iter::once(Ok(BuiltLabel {
                    behaviour: LabelBehaviour::Replace,
                    key: "testLabel",
                    value: "test-value".into(),
                })))
                .unwrap();

            path_stack.push("test".into()).unwrap();
            path_stack.push("hello".into()).unwrap();

            let label_stack_with_path = LabelStackWithPath {
                path_stack: &path_stack,
                label_stack: &label_stack,
            };

            assert_eq!(
                label_stack_with_path.to_string(),
                r#"{testLabel = "test-value", path = "test/hello"}"#,
            );
        }
    }

    mod get_value_from_path_for_modifiers {
        use crate::modifiers::{get_value_from_path_for_modifiers, Modifier, PathStack};

        #[test]
        fn skip() {
            let path = {
                let mut p = PathStack::default();
                p.push("hello".into()).unwrap();
                p.push("pickme".into()).unwrap();
                p.push("nome".into()).unwrap();
                p
            };

            let modifiers = vec![Modifier::Skip, Modifier::Skip, Modifier::Prepend];

            let actual = get_value_from_path_for_modifiers(&path, &modifiers);
            let expected = "hello";

            assert_eq!(actual.unwrap().unwrap(), expected);
        }
    }

    mod path_stack {
        use crate::modifiers::PathStack;

        #[test]
        fn display() {
            let mut stack = PathStack::default();
            stack.push("hello".into()).unwrap();
            stack.push("world".into()).unwrap();
            assert_eq!(stack.to_string(), "hello/world");
        }
    }

    mod label_stack {
        use crate::modifiers::{BuiltLabel, LabelBehaviour, LabelStack};

        #[test]
        fn display() {
            let mut stack = LabelStack::default();

            stack
                .push(
                    [
                        Ok(BuiltLabel {
                            behaviour: LabelBehaviour::Replace,
                            key: "hello",
                            value: "world".into(),
                        }),
                        Ok(BuiltLabel {
                            behaviour: LabelBehaviour::Replace,
                            key: "something",
                            value: "cool".into(),
                        }),
                    ]
                    .into_iter(),
                )
                .unwrap();
            assert_eq!(stack.to_string(), r#"hello = "world", something = "cool""#);

            stack
                .push(
                    [Ok(BuiltLabel {
                        behaviour: LabelBehaviour::Replace,
                        key: "something",
                        value: "override".into(),
                    })]
                    .into_iter(),
                )
                .unwrap();
            assert_eq!(
                stack.to_string(),
                r#"hello = "world", something = "override""#
            );

            stack.pop();
            assert_eq!(stack.to_string(), r#"hello = "world", something = "cool""#);

            stack
                .push(
                    [Ok(BuiltLabel {
                        behaviour: LabelBehaviour::Append("::"),
                        key: "something",
                        value: "appendsomething".into(),
                    })]
                    .into_iter(),
                )
                .unwrap();
            assert_eq!(
                stack.to_string(),
                r#"hello = "world", something = "cool::appendsomething""#
            );
        }

        #[test]
        fn escape() {
            let mut stack = LabelStack::default();

            stack
                .push(std::iter::once(Ok(BuiltLabel {
                    behaviour: LabelBehaviour::Replace,
                    key: "hello",
                    value: "world\"!!\"".into(),
                })))
                .unwrap();
            assert_eq!(stack.to_string(), r#"hello = "world\"!!\"""#);

            stack
                .push(std::iter::once(Ok(BuiltLabel {
                    behaviour: LabelBehaviour::Replace,
                    key: "hello",
                    value: "world\n".into(),
                })))
                .unwrap();
            assert_eq!(stack.to_string(), r#"hello = "world\n""#);

            stack
                .push(std::iter::once(Ok(BuiltLabel {
                    behaviour: LabelBehaviour::Replace,
                    key: "hello",
                    value: "wor\\ld".into(),
                })))
                .unwrap();
            assert_eq!(stack.to_string(), r#"hello = "wor\\ld""#);
        }
    }

    mod parse_modifiers_string {
        use crate::modifiers::{
            InternalModifiers, LabelBehaviour, LabelDefinition, Modifier, ParsedLabel,
            ParsedModifiersList,
        };

        #[test]
        fn test_invalid_modifier_key() {
            let err = ParsedModifiersList::parse("!!<*|hello=world,other==!<").unwrap_err();

            assert_eq!(
                format!("{err:?}"),
                "Failure(Error { input: \"*\", code: MapRes })"
            );
        }

        #[test]
        fn test_invalid_modifier_label() {
            let err = ParsedModifiersList::parse("!!<|hello=world,other==!*<").unwrap_err();

            assert_eq!(
                format!("{err:?}"),
                "Failure(Error { input: \"*<\", code: MapRes })"
            );
        }

        #[test]
        fn key_and_labels() {
            let (rest, actual) =
                ParsedModifiersList::parse("!!<|other==!<,hello=world,hello[::]=test").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: array_vec![Modifier::Exclude, Modifier::Exclude, Modifier::Prepend],
                labels: vec![
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "other",
                        value: ParsedLabel::Modifiers(array_vec![
                            Modifier::Exclude,
                            Modifier::Prepend
                        ]),
                    },
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "hello",
                        value: ParsedLabel::Fixed("world".into()),
                    },
                    LabelDefinition {
                        behaviour: LabelBehaviour::Append("::"),
                        key: "hello",
                        value: ParsedLabel::Fixed("test".into()),
                    },
                ],
                internal: InternalModifiers::default(),
            };

            // ensure the whole input was consumed
            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn empty() {
            let (rest, actual) = ParsedModifiersList::parse("").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: array_vec![],
                labels: vec![],
                internal: InternalModifiers::default(),
            };

            // ensure the whole input was consumed
            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn only_keys() {
            let (rest, actual) = ParsedModifiersList::parse("!!<|").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: array_vec![Modifier::Exclude, Modifier::Exclude, Modifier::Prepend],
                labels: vec![],
                internal: InternalModifiers::default(),
            };

            // ensure the whole input was consumed
            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        // No longer supported since v0.2.0, the separator is always required
        #[test]
        #[should_panic]
        fn only_keys_no_separator() {
            ParsedModifiersList::parse("!!<").unwrap();
        }

        #[test]
        fn only_labels() {
            let (rest, actual) = ParsedModifiersList::parse("|hello=world,other==!<").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: array_vec![],
                labels: vec![
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "hello",
                        value: ParsedLabel::Fixed("world".into()),
                    },
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "other",
                        value: ParsedLabel::Modifiers(array_vec![
                            Modifier::Exclude,
                            Modifier::Prepend
                        ]),
                    },
                ],
                internal: InternalModifiers::default(),
            };

            // ensure the whole input was consumed
            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn quoted_labels() {
            let (rest, actual) = ParsedModifiersList::parse(
                r#"|version="1.2.3(\"crusty, crustacean\")",version2=1.2.3("crusty\, crustacean"),build=123"#,
            )
            .unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: array_vec![],
                labels: vec![
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "version",
                        value: ParsedLabel::Fixed(r#"1.2.3("crusty, crustacean")"#.into()),
                    },
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "version2",
                        value: ParsedLabel::Fixed(r#"1.2.3("crusty, crustacean")"#.into()),
                    },
                    LabelDefinition {
                        behaviour: LabelBehaviour::Replace,
                        key: "build",
                        value: ParsedLabel::Fixed(r#"123"#.into()),
                    },
                ],
                internal: InternalModifiers::default(),
            };

            // ensure the whole input was consumed
            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn struct_name() {
            let (rest, actual) = ParsedModifiersList::parse("HelloWorld").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: array_vec![],
                labels: vec![],
                internal: InternalModifiers::default(),
            };

            // ensure the whole input remained unparsed
            assert_eq!(rest, "HelloWorld");
            assert_eq!(actual, expected);
        }

        #[test]
        fn internal_modifiers() {
            let (rest, actual) = ParsedModifiersList::parse("||:namespace=abc").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: heapless::Vec::default(),
                labels: vec![],
                internal: InternalModifiers {
                    namespace: Some("abc".into()),
                },
            };

            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn empty_modifiers_with_internal() {
            let (rest, actual) = ParsedModifiersList::parse("|hello==|:namespace=abc").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: heapless::Vec::default(),
                labels: vec![LabelDefinition {
                    behaviour: LabelBehaviour::Replace,
                    key: "hello",
                    value: ParsedLabel::Modifiers(array_vec![]),
                }],
                internal: InternalModifiers {
                    namespace: Some("abc".into()),
                },
            };

            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn modifiers_with_internal() {
            let (rest, actual) = ParsedModifiersList::parse("|hello==<|:namespace=abc").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: heapless::Vec::default(),
                labels: vec![LabelDefinition {
                    behaviour: LabelBehaviour::Replace,
                    key: "hello",
                    value: ParsedLabel::Modifiers(array_vec![Modifier::Prepend]),
                }],
                internal: InternalModifiers {
                    namespace: Some("abc".into()),
                },
            };

            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn fixed_modifiers_with_internal() {
            let (rest, actual) =
                ParsedModifiersList::parse("|hello=\"test\"|:namespace=abc").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: heapless::Vec::default(),
                labels: vec![LabelDefinition {
                    behaviour: LabelBehaviour::Replace,
                    key: "hello",
                    value: ParsedLabel::Fixed("test".into()),
                }],
                internal: InternalModifiers {
                    namespace: Some("abc".into()),
                },
            };

            assert_eq!(rest, "");
            assert_eq!(actual, expected);
        }

        #[test]
        fn unknown_internal_modifiers() {
            let (rest, actual) = ParsedModifiersList::parse("||:random=abc").unwrap();

            let expected = ParsedModifiersList {
                key_modifiers: heapless::Vec::default(),
                labels: vec![],
                internal: InternalModifiers { namespace: None },
            };

            assert_eq!(rest, ":random=abc");
            assert_eq!(actual, expected);
        }
    }
}
