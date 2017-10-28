# Plank

This is an implementation of plank - a little programming language I am building for "Compiling methods" course.

This repository currently consists of 7 crates:

* `plank-errors` - defines `Position` and `Span` types, handles error reporting and formatting.
* `plank-syntax` - defines plank AST, and contains parser for plank source code.
* `plank-frontend` - validates plank programs and converts AST to intermediate representation.
* `plank-ir` - defines plank intermediate representation.
* `plank-interpreter` - a simple virtual machine for executing plank intermediate representation.
* `plank` - driver program that glues everything together.
* `plank-server` - plank language server. However, currently there is no plugin for any editor.
