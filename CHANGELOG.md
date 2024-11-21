# Unreleased

# v0.2.7

Remove empty label stack elements to avoid imposing global label limit.

# v0.2.6

Add additional context when overflowing label stack.

# v0.2.5

Heapless has been upgraded to 0.8 (thanks @rursprung in #9).

Fixes a bug causing internal modifiers not to be parsed after following
label modifiers (ie. `|x==<|:namespace=xyz` would fail to parse).

# v0.2.4

This version fixes a bug that caused labels gathered via labels not
properly be escaped that could cause potential metric ingestion
issues.

# v0.2.3

This version includes the ability for consumers to override the global
namespace by using "internal overrides", this is an extra "section" in
the usual `serialize_newtype_struct` call:

```
x|y|:namespace=my_new_namespace
```

A small internal change to prevent unnecessary early formatting of labels
during concatenation is also included.

# v0.2.2

This includes a workaround for a bug in `heapless` which causes an
incorrect entry to be returned when using the entry API which under
some circumstances causes label values to be written to the incorrect
key (#6). Thanks @shaunbennett for the thorough investigation and
contribution.

# v0.2.1

This includes a fix for a small regression in v0.2.0 causing quotes not
to be escaped inside fixed labels, significantly improving the ergonomics
of labels in the process.

There's now two types of fixed labels, "quoted" and "unquoted" - the
"quoted" variant works as you'd expect, allowing any characters within
the boundaries of the quotes, and allows escaping quotes using the `\`
control character.

"Unquoted" labels will continue working as-is, but also allow escaping
the `,` delimiter used within labels.

```
|version="1.2.3(\"crusty, crustacean\")",build=123 -> {version = "1.2.3(\"crusty, crustacean\"), build = "123"}
|version=1.2.3("crusty\, crustacean"),build=123    -> {version = "1.2.3(\"crusty, crustacean\"), build = "123"}
```

# v0.2.0

This change brings a major revamp in the internal parsing and state
engine in `serde_prometheus`, significantly improving performance
for applications exposing a large amount of metrics.

The exposed API remains the same, and whilst striven for complete
compatibility with 0.1, there are a few breaking changes to keep in
mind when upgrading:

## Breaking changes

- `serialize_newtype_struct` users adding only labels now need to
  include the `|` separator, even if no path modifications are given.
  This separator was previously optional in `0.1`.

  ##### Before:

  ```
  my_label=my_value,other_label=other_value
  ```

  ##### After:

  ```
  |my_label=my_value,other_label=other_value
  ```
- Labels, stack depth and modifiers are now no longer unbounded.
  - A maximum of 12 items within a metric name (`<` operator)
  - A maximum of 16 modifiers are allowed within a single `serialize_newtype_struct` call
  - A maximum of 32 labels are allowed on a single metric
  - A maximum "label override" (multiple members within the same stack setting a label value) of 32.
  - A maximum nested metric depth of 128

## New features

- A `serde_prometheus::write` function is now exposed publicly that allows
  for buffer reuse.
