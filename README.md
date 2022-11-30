# Rust Struct Notation
Deserialization library inspired by [ron](https://github.com/ron-rs/ron). There are some key differences though. 

 - Rsn does not derive on serde, and isn't compatible with it yet. 
 - Everything needs to be explicitly typed out unless marked untagged. For example a tuple struct Would be written `StructName(field1, field2)` instead of `(field1, field2)`. And an enum would be `Enum::Variant(field1, field2)` instead of `Variant(field1, field2)`.
## Values

### Integer
A signed integer, either in decimal `123`, binary `0b1111011`, octal `0o173` or hexadecimal `0x7B`. Can be signed by prefixing with `-`.

### Float
A floating point number, expressed either in normal form `123.321`, or in scientific form `5e10`. Can be signed by prefixing with `-`.

### String
A text string. A series of characters in double qoutes i.e `"Hello World!"`. `\` is used as an escape character to enter special characters.

Escape codes:
 - `\n` => newline
 - `\r` => carrage return
 - `\t` => tab
 - `\\` => \
 - `\'` => '
 - `\"` => "
 - `\0` => Null character

### Char
A single character in single qoutes i.e `'A'`. It uses the same escape codes as String.

### Path
A list of identifiers seperated by double colon i.e `foo::bar::baz`. Can also be just `::`, where the two identifiers become empty strings.

Example values:
 - `Foo`, single identifier.
 - `some_crate::Foo`, path.
 - `::some_crate::Foo`, path with leading
 - `::`, just a double colon.

### Array
A list of values in brackets, seperated by commas. With an optional trailing comma.

Example:
```
[
    "Hello",
    ", ",
    "World",
    "!",
]
```

Can be empty: `[]`.

Values can have different types but this is not supported by most rust types using Array as a middle step.

### Map
A list of paired values in curly brackets, where pairs are internally seperated by fat arrow `=>` and externally by comma.

Example:
```
{
    1 => "cat",
    5 => "dog",
    4 => "bird",
    100 => "car",
}
```

Can be empty: `{}`

Values can have different types but this is not supported by most rust types using Array as a middle step.

### Range
A range between two values.

Valid ranges:
 - full range, `..`
 - range to, `..<value>`
 - range to inclusive, `..=<value>`
 - range from, `<value>..`
 - range, `<value>..<value>`
 - range inclusive, `<value>..=<value>`


### Tuple
Much like a sequence this is a list of elements seperated by commas. But is enclosed in parenthesis.

Example: `(1, 2..=5, "Hi")`

Can be empty: `()` or `(,)`.

Values can be different types and is often supported by rust representations.

### Named Tuple
A tuple that is prefixed with an identifier.

Example: `Foo(1, 'B')`

### Struct
A sequence of named values. Enclosed in curly brackets and seperated by commas. Identifiers and vlaues are seperated by a single colon.

Example:
```
{
    foo: 2,
    bar: "Hello!",
    baz: (1, 2, 3),
}
```

### Named Struct
A struct that is prefixed by an identifier.

Example:
```
Hello {
    foo: 2,
    bar: "Hello!",
    baz: (1, 2, 3),
}
```

# Converting Values to rust types
Values can be converted into rust types with the trait `FromValue`.

The `FromValue` trait has been implemented by the following types in this crate.
 - Integer types, `i8`, `i16`, `i32`, `i64`, `isize`, `u8`, `u16`, `u32`, `u64`, `usize`. Here the identifiers `MAX` and `MIN` are also valid values.

 - Floating types, `f32`, `f64`. Here the identifiers `INFINITY`, `NEG_INFINITY` and `NAN` are also valid values.

 - Ranges, `Range<T: FromValue>`, `RangeInclusive<T: FromValue>`, `RangeFrom<T: FromValue>`, `RangeTo<T: FromValue>`, `RangeToInclusive<T: FromValue>`, `RangeFull`. There is also a `AnyRange` enum defined here which can be any of the other ranges.

 - Arrays, `[T: FromValue; N]`, `Vec<T: FromValue>`, `arrayvec::ArrayVec<T: FromValue, N>`.

 - Maps, `HashMap<K: FromValue, V: FromValue>`, `hashbrown::HashMap<K: FromValue, V: FromValue>` (with feature enabled).

 - Tuples, `(T: FromValue, ...)`

 - Option, `Option<T: FromValue>`

 - bool

 - String

 - char

 - Box, `Box<T: FromValue>`

 - For certain `vek` types with a feature enabled.

This trait can be implemented manually, or with a derive macro.