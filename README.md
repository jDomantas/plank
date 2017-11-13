# Plank

This is an implementation of plank - a little programming language I am building for "Compiling methods" course.

Here is a simple guide to the language: [plank-language.md](./plank-language.md).

Currently there is a compiler frontend that can generate plank IR, and a simple interpreter that can execute generated IR.

This repository currently consists of 8 crates:

* `plank-errors` - defines `Position` and `Span` types, handles error reporting and formatting.
* `plank-syntax` - defines plank AST, and contains parser for plank source code.
* `plank-frontend` - validates plank programs and converts AST to intermediate representation.
* `plank-ir` - defines plank intermediate representation.
* `plank-interpreter` - a simple virtual machine for executing plank intermediate representation.
* `plank` - driver program that glues everything together.
* `plank-server` - plank language server.
* `tests` - a simple program that builds and runs tests.

## Examples

Here's a hello world program:

```rust
fn puts(mut string: *u8) {
    while *string != 0 {
        putc(*string);
        // this doesn't look very nice :(
        string = (string as u32 + 1) as *u8;
    }
    putc('\n');
}

fn main() -> i32 {
    puts("Hello, world!");
    return 0;
}
```

You can find more in [examples](./examples).

## Installing

Make sure that you have rust and cargo installed.

```
git clone https://github.com/jDomantas/plank.git
cd plank
cargo install --path plank
```

## Editor support

If you are using Visual Studio Code, there is an extension that provides syntax highligting and displays diagnostics provided by plank language server.

You can find the extension [on the VSCode extension marketplace](https://marketplace.visualstudio.com/items?itemName=jDomantas.plank), and you can find its code on [jDomantas/plank-vscode](https://github.com/jDomantas/plank-vscode).

You can install plank language server from this repo:

```
git clone https://github.com/jDomantas/plank.git
cd plank
cargo install --path plank-server
```

By default (I think) cargo installs binaries to a place that is on your path, so everything should work without further intervention. If for some reason that isn't the case, you can provide path to `plank-server` executable in vscode configuration.

## Running tests

Compiler and interpreter are tested by throwing programs at them and verifying that the outcome matches the expected one. The program that is responsible for that is in `tests` crate.

You can run tests by running `cargo run -p tests` in repository root. More precisely, test runner expects to find the following directories:

* `./examples` - we want to make sure that the examples aren't broken
* `./tests/compile-fail` - programs that should not build
* `./tests/pass` - programs that should produce correct output when ran with given input.

Currently there are only a couple of test programs, but this will be improved over time.
