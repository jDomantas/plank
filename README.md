# Plank

This is an implementation of plank - a little programming language I am building for "Compiling methods" course.

This repository currently consists of 4 crates:

* `plank-errors` - defines `Position` and `Span` types, and handles error reporting and formatting.
* `plank-syntax` - defines plank AST, and implements lexing and parsing plank source code.
* `plank` - driver program for all the rest.
* `plank-server` - plank language server. However, currently there is no plugin for any editor.
