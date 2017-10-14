# Plank

This is an implementation of plank - a little programming language I am building for "Compiling methods" course.

This repository currently consists of 5 crates:

* `plank-errors` - defines `Position` and `Span` types, handles error reporting and formatting.
* `plank-syntax` - defines plank AST, and contains parser for plank source code.
* `plank-frontend` - WIP
* `plank` - driver program for all the rest.
* `plank-server` - plank language server. However, currently there is no plugin for any editor.
