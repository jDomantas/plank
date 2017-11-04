# Plank language

This is a short guide to the plank programming language.

Plank is a low level language that syntaxically looks a bit like Rust, but its semantics are much closer to C. This guide assumes that you are somewhat (?) familiar with both languages.


## Expressions

Expressions in Plank are similar to most other imperative languages:

* Binary expressions

    There is the usual arithmetic: `1 + 2`, `a - b`, `c * 5`, `d / 8`, `10 % e`. Currently other arithmetic or bit operations are not implemented.

    Numeric comparisions: `a <= b`, `a < b`, `a >= b`, `a > b`.

    Boolean operations: `a && b`, `a || b`. These are short-circuiting.

    Comparisions: `a == b`, `a != b`. These work on any type (both must have the same type). They basically compare memory, so for example you can use them to compare pointers. If comparing structs, be careful about padding - it's not a field by field comparision.

* Unary expressions

    You can use `+` and `-` on integers: `-a`, `+a`.

    And you can negate bools: `!a`.

    You can take address of a lvalue with `&` operator: `&a`.

    You can dereference pointers with `*`: `*ptr`.

* Literals

    There are the usual boolean literals: `true` and `false`.

    Char literals look like this: `'a'`. They must be exactly one char long, and are in fact byte literals (they have type `u8` - unsigned byte). You can use the following escape sequences:

    * `\\` - gives `\`
    * `\n` - newline (ascii code 10)
    * `\'` - single quote
    * `\"` - double quotes
    * `\xHH` - here `H` are base-16 digits, gives the specified byte

    String literals: `"plank is pretty cool!"`. You can use the same escape sequences as in char literals. String literals give a pointer to their first char (thus they have type `*u8`), and are terminated by null bytes.

    Number literals: `123`, `6882u`, `89123i`, `0u32`. They can be suffixed with `u` to specify unsigned number, `i` to specify a signed number, or a concrete numeric type (like `u8` or `i32`).

    There is also a `unit` literal, that is the only value of the `unit` type.

* Names

    Identifiers in plank are alphanumeric strings that do not start with a number (as usual in other languages). Names can also have type parameters, in case you want to explicitly refer to some generic value. For example, if we have a generic `make_pair` function:

    ```rust
    fn make_pair<A, B>(a: A, b: B) -> Pair<A, B> { ... }
    ```

    We can refer to a concrete instantiations of `make_pair` by providing type parameters (with a turbofish :smile:):

    ```rust
    // explicitly provide make_pair type parameters
    call_this_function(make_pair::<u32, SomeStruct>);
    ```

* Field access

    You can access a field on an expression in the same way as in most other languages: `expr.a_field`. You can access a field on a pointer, compiler will automatically insert as many dereferences as it needs to get to a non-pointer type, and try to get the field on that value.

* Casts

    You can cast values to other types using `as` operator: `0u32 as *u8`. Both the source and result type must have the same size - the cast simply reinterprets the bits as requested type. You can't cast types that contain generic type parameters, unless they are behind a pointer and thus have fixed known size.

* Function calls

    Functions calls look the same way as in most other languages: `func(1, a, 2 + 3)`. You can also call using named parameters: if you have a function

    ```rust
    fn foo(bar: u32, baz: *u8) { ... }
    ```

    you can also call it like this:

    ```rust
    foo(baz: "hello", bar: 1)
    ```

    Positional and named parameters cannot be mixed - either all parameters must be named, or all must be positional.

* Assignment

    Nothing surprising here:

    ```rust
    x = 0;
    y = x + 3;

    // right associative, returns assigned value:
    x = y = z = 1;
    ```

## Statements

Statements in Plank should also be familiar to you:

* If statements

    ```rust
    if condition {
        do_something();
    }

    if condition {
        do_something();
    } else {
        do_something_else();
    }

    if condition {
        do_something();
    } else if another_condition {
        do_other_thing();
    } else {
        do_something_else();
    }
    ```

    Just like in Rust, condition does not have to be surrounded by parentheses, and curly braces around branches are mandatory.

* Loop

    Just like Rust, Plank has a simple `loop`:

    ```rust
    loop {
        do_this_again();
    }
    ```

    It will loop forever, and is equivalent to C `while true`.

* While loop

    The same way as in `if` - no parentheses on the condition, and curly braces are mandatory.

    ```rust
    while a > 0 {
        a = a - 1;
    }
    ```

* Break and continue

    Same behaviour as in other imperative languages. Cannot be used outside a loop.

    ```rust
    loop {
        if something {
            continue;
        }
        if nothing {
            break;
        }
    }
    ```

* Return

    ```rust
    return value;
    ```

    `value` can be omitted in functions returning `unit` - `return;` and `return unit;` are equivalent. Also, in those functions `return` can be omitted altogehter - it will be automatically inserted at the end of the function.

* Let

    You can declare local variables with `let`:

    ```rust
    let x: u32 = 1;
    let x = 1;
    let x: u32;
    let x;
    ```

    Both the type and the initial value are optional. However, variable cannot be used before being assigned. Local is in scope starting from the statement following the declaration, and locals can be shadowed (same as in Rust). Therefore, this code is legal:

    ```rust
    let x: i32 = 1;
    let x: *u8 = int_to_string(x);
    ```

* Block

    You can freely wrap a bunch of statements in a block:

    ```rust
    {
        let x = 1;
        x = x + 1;
    }

    // x is no longer in scope
    ```

* Expression

    Nothing surprising here:

    ```rust
    call_a_function();
    this + is - technically * legal;
    ```

## Functions

Function declarations look the same as they do in Rust:

```rust
fn my_function(x: i32, y: *u8) -> *u8 { ... }
```

Return type can be omitted, and in that case it is `unit`:

```rust
fn returns_a_unit() { ... }
```

Functions can be declared without a body, and can also be annotated with `extern` (which currently does not do anything):

```rust
fn no_body(x: u32) -> u32;
extern fn foo() -> u8;
```

## Structs

Just like functions, structs are ~~stolen from~~ inspired by Rust:

```rust
struct Foo {
    x: i32,
    y: *u8,
}
```

When you declare a struct, compiler generates a constructor function for you, which you can use to build instances of the struct:

```rust
fn Foo(x: i32, y: *u8) -> Foo { ... }
```

Note that here is a very good place to use named function parameters:

```rust
let foo = Foo(
    x: 1,
    y: "pretty cool, right?",
);
```

Structs cannot be infinitely deep. These examples are not legal:

```rust
// directly recursive
struct Foo {
    foo: Foo,
}

// mutually recursive
struct Foo {
    bar: Bar,
}

struct Bar {
    foo: Foo,
}
```

## Types

There are 10 built-in types in Plank:

* `unit`, with a single value `unit`.
* `bool`, with two values `true` and `false`.
* number types `u8`, `i8`, `u16`, `i16`, `u32`, `i32`.
* pointers: `*<type>`, for example: `*u8`, `*unit`, `*********bool`.
* function pointers: `fn(<type-list>) -> <type>`, for example: `fn(u8) -> bool`, `fn()`. Return type can be omitted, in that case it is `unit`.

You can also declare you own types (structs).

There is also a special wildcard "type": `_`. It is not really a type, but a piece of syntax to tell the compiler "infer this type for me". For example, when you skip the type in a variable declaration, it is actually the same as giving `_` as the type:

```rust
let x = 1;
let x: _ = 1;
```

However, `_` cannot be used in function and struct declarations.

## Generics

Plank has generics!

You can list generic parameters on generic functions and structs the same way as in Rust:

```rust
fn generic<T>(x: T, y: u8) -> T { ... }

struct List<T> {
    item: T,
    next: *List<T>,
}
```

When you refer to generic structs as types, you need to provide correct amount of type parameters (for example `List<u8>`). `List` and `List<u8, i32>` are illegal.

When you refer to generic functions, type parameters can be omitted (`generic` is the same as `generic::<_>`). However, if you do provide them, you must give the correct amount. Syntax for providing type parameters is the same as Rust's "turbofish".

## Built-ins

There are four built-in functions:

* `size_of`

    ```rust
    fn size_of<T>() -> u32;
    ```

    Returns the size of its type parameter.

* `align_of`

    ```rust
    fn align_of<T>() -> u32;
    ```

    Returns the alingment of its type parameter.

* `putc`

    ```rust
    fn putc(ch: u32);
    ```

    Writes the given byte to standard output.

* `getc`

    ```rust
    fn getc() -> i32;
    ```

    Reads a byte from standard input. Returns -1 if end of stream is reached.
