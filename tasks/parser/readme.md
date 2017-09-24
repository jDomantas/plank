# Parser

Parser is implemented in [syntax/src/parser.rs](../../src/syntax/src/parser.rs). The module exports a single function -  `parse`. Parsing always produces a `Program` - if errors are encountered while parsing an item or statement, they will be absent from the parsed program. The `Program` and the rest of the AST is defined in [syntax/src/ast.rs](../../src/syntax/src/ast.rs). All errors are emitted to given `Reporter`.


## Parsing algorithm

Parser uses two strategies for parsing Plank code: recursive descent parser for item declarations and statements, and [Pratt parser](http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/) for expressions. Reasons for using recursive descent parser for items are these:

* Very simple to write.
* Grammar is well suited for such parser - for most cases one token of lookahead is sufficient. There are only two cases where two token lookahead is used. Therefore parser does not ever need to backtrack, making it run in linear time.
* Pretty good and simple error recovery. Because almost all statements start with unique token, it is very easy to synchronize after encountering an error.

And the reasons for using Pratt parser to parse expressions:

* Pretty simple to write.
* Cleanly handles operator precedence and associativity.
* Easy to expand the language by adding new operators.


## Error recovery

Error recovery while parsing a block:

* If failed to parse a statement, skip until the next token is one of: `if`, `loop`, `while` `break`, `continue`, `let`, `return`, `{`, `}`.
* If when skipping we find token `fn`, check the token after that (this is one of the places where 2 token lookahead is neccessary):
    * If it is a name, then we probably are at the start of a function definition. So we failed to parse a block.
    * If it is not, then it's probably just a function type - continue skipping tokens.
* If when skipping we find `extern` or `struct` then fail parsing a block - these tokens can only appear at the start of item definition.

Error recovery while parsing items is very similar - cases are basically the same, but now we stop on those which can start an item:

* `fn`, followed by a name.
* `extern`
* `struct`

Also, when parsing statements the parser handles one little special case: if one of expected tokens is `;`, and the next token is on the next line, then programmer probably just forgot the semicolon. Parser emits an error with a hint, and continues parsing as if the semicolon was there.

For example, if try to parse this program (note the missing semicolon on the second line):

```plank
fn main() -> Unit {
    let x = 1
    let y = 2;
}
```

We get this error:

```
error: expected one of `(`, `.`, `;`, operator, got `let`.
2 |      let x = 1
  |               ~ maybe you missed a `;`?
3 |      let y = 2;
  |      ^^^ unexpected `let`
```

There is no error recovery when parsing expressions, mainly because it is difficult to have good error recovery in expressions. However, as the language is not expressions oriented, the expressions are usually quite small, and having just one error pointed out is usually enough to fix whole expression. Therefore error recovery only at statement and item level was deemed sufficient.
