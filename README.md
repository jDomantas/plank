# Plank

This is an implementation of plank - a little programming language I am building for "Compiling methods" course.

This repository currently consists of 7 crates:

* `plank-errors` - defines `Position` and `Span` types, handles error reporting and formatting.
* `plank-syntax` - defines plank AST, and contains parser for plank source code.
* `plank-frontend` - validates plank programs and converts AST to intermediate representation.
* `plank-ir` - defines plank intermediate representation.
* `plank-interpreter` - a simple virtual machine for executing plank intermediate representation.
* `plank` - driver program that glues everything together.
* `plank-server` - plank language server.

## Examples

Coming soon™.

## Installing

Make sure that you have rust and cargo installed.

```
git clone https://github.com/jDomantas/plank.git
cd plank
cargo install --path plank
```

## Editor support

If you are using Visual Studio Code, you there is an extension that provides syntax highligting and displays diagnostics provided by plank language server.

You can find the extension [on the VSCode extension marketplace](https://marketplace.visualstudio.com/items?itemName=jDomantas.plank), and you can find its code on [jDomantas/plank-vscode](https://github.com/jDomantas/plank-vscode).

You can install plank language server from this repo:

```
git clone https://github.com/jDomantas/plank.git
cd plank
cargo install --path plank-server
```

By default (I think) cargo installs binaries to a place that is on your path, so everything should work without further intervention. If for some reason that isn't the case, you can provide path to `plank-server` executable in vscode configuration.

## Running tests

Tests are for the weak.

Just kidding. But the current situation is pretty sad.
