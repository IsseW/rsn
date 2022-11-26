

# Values

## Integer
A signed integer, either in decimal `123`, binary `0b1111011`, octal `0o173` or hexadecimal `0x7B`. Can be signed by prefixing with `-`.

## Float
A floating point number, expressed either in normal form `123.321`, or in scientific form `5e10`. Can be signed by prefixing with `-`.

## String
A text string. A series of characters in double qoutes i.e `"Hello World!"`. `\` is used as an escape character to enter special characters.

Escape codes:
 - `\n` => newline
 - `\r` => carrage return
 - `\t` => tab
 - `\\` => \
 - `\'` => '
 - `\"` => "
 - `\0` => Null character

## Char
A single character in single qoutes i.e `'A'`. It uses the same escape codes as String.

## Unit
A single identifier i.e `Foo`, `Bar`.

## Path
A list of identifiers seperated by double colon i.e `foo::bar::baz`. Can also be just `::`, where the two identifiers become empty strings.

## Array
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

## Map
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

## Range
A range between two values.

Valid ranges:
 - full range, `..`
 - range to, `..<value>`
 - range to inclusive, `..=<value>`
 - range from, `<value>..`
 - range, `<value>..<value>`
 - range inclusive, `<value>..=<value>`


## Tuple
Much like a sequence this is a list of elements seperated by commas. But is enclosed in parenthesis.

Example: `(1, 2..=5, "Hi")`

Can be empty: `()` or `(,)`.

Values can be different types and is often supported by rust representations.

## Named Tuple
A tuple that is prefixed with an identifier.

Example: `Foo(1, 'B')`

## Struct
A sequence of named values. Enclosed in curly brackets and seperated by commas. Identifiers and vlaues are seperated by a single colon.

Example:
```
{
    foo: 2,
    bar: "Hello!",
    baz: (1, 2, 3),
}
```

## Named Struct
A struct that is prefixed by an identifier.

Example:
```
Hello {
    foo: 2,
    bar: "Hello!",
    baz: (1, 2, 3),
}
```