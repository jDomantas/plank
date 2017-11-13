# Lexer

Lexer is implemented in [plank-syntax/src/lexer.rs](../../plank-syntax/src/lexer.rs). The module exports a single function - `lex`. Lexing always produces some list of tokens - invalid input is represented as token `Token::Error`. All errors are emitted to given `Reporter`.

## Grammar

Second part of [grammar](../grammar/grammar.bnf) is implemented in the lexer:

```bnf
<ident>             ::= <letter> <ident-suffix> | "_" <ident-char> <ident-suffix>
<ident-suffix>      ::= "" | <ident-char> <ident-suffix>
<ident-char>        ::= "_" | <letter> | <digit>

<literal>           ::= <bool> | <int> | <char> | <string>

<bool>              ::= "true" | "false"
<digit-sequence>    ::= <digit> | <non-zero-digit> <int>
<int>               ::= <digit-sequence> | <digit-sequence> <int-suffix>
<int-suffix>        ::= "i" | "u" | "i8" | "i16" | "i32" | "u8" | "u16" | "u32"
<char>              ::= "'" <string-char> "'" | "'\"'"
<string>            ::= "\"" <string-suffix>
<string-suffix>     ::= "\"" | <string-char> <string-suffix> | "'" <string-suffix>

<letter>            ::= <uppercase> | <lowercase>
<uppercase>         ::= "A" | "B" | "C" | ... | "Z"
<lowercase>         ::= "a" | "b" | "c" | ... | "z"
<non-zero-digit>    ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<digit>             ::= "0" | <non-zero-digit>

<hex-digit>         ::= <digit> | "a" | ... | "f" | "A" | ... | "F"
<escape>            ::= "\\x" <hex-digit> <hex-digit>
<string-char>       ::= <printable> | "\\n" | "\\\"" | "\\'" | "\\\\" | <escape>
<printable>         ::= <letter> | <digit> | " " | "!" | "#" | "$" | ... | "~"
```

Also, all terminals in first part of grammar (like `fn`, `let`, `+`, and similar) are also recognized by the lexer.

## Tokens

Lexer returns a list of `Spanned<Token>`.
* `Token` is defined in [plank-syntax/src/tokens.rs](../../plank-syntax/src/tokens.rs).
* `Spanned` is defined in [plank-syntax/src/position.rs](../../plank-syntax/src/position.rs).
* `Span`, which is used in `Spanned`, is defined in [plank-errors/src/position.rs](../../plank-errors/src/position.rs).

Here is a table of tokens and their internal representation:

| Token        | Internal representation                    | Notes           |
| ------------ | ------------------------------------------ | --------------- |
| `identifier` | `Token::Ident("identifier")`               |                 |
| `123`        | `Token::Number(Number { value: 123, .. })` |                 |
| `true`       | `Token::Bool(true)`                        |                 |
| `false`      | `Token::Bool(false)`                       |                 |
| `'A'`        | `Token::Char(65)`                          |                 |
| `"ABC"`      | `Token::Str(vec![65, 66, 67])`             |                 |
| keyword      | `Token::Keyword(_)`                        | Full list below |
| `<`          | `Token::Less`                              |                 |
| `<=`         | `Token::LessEqual`                         |                 |
| `>`          | `Token::Greater`                           |                 |
| `>=`         | `Token::GreaterEqual`                      |                 |
| `==`         | `Token::Equal`                             |                 |
| `!=`         | `Token::NotEqual`                          |                 |
| `+`          | `Token::Plus`                              |                 |
| `-`          | `Token::Minus`                             |                 |
| `*`          | `Token::Star`                              |                 |
| `/`          | `Token::Slash`                             |                 |
| `%`          | `Token::Percent`                           |                 |
| `&`          | `Token::Ampersand`                         |                 |
| `.`          | `Token::Dot`                               |                 |
| `(`          | `Token::LeftParen`                         |                 |
| `)`          | `Token::RightParen`                        |                 |
| `{`          | `Token::LeftBrace`                         |                 |
| `}`          | `Token::RightBrace`                        |                 |
| `->`         | `Token::Arrow`                             |                 |
| `&&`         | `Token::And`                               |                 |
| `\|\|`       | `Token::Or`                                |                 |
| `,`          | `Token::Comma`                             |                 |
| `_`          | `Token::Underscore`                        |                 |
| `:`          | `Token::Colon`                             |                 |
| `::`         | `Token::DoubleColon`                       |                 |
| `;`          | `Token::Semicolon`                         |                 |
| `!`          | `Token::Not`                               |                 |
| `=`          | `Token::Assign`                            |                 |


| Keyword    | Internal representation |
| ---------- | ----------------------- |
| `extern`   | `Keyword::Extern`       |
| `fn`       | `Keyword::Fn`           |
| `struct`   | `Keyword::Struct`       |
| `if`       | `Keyword::If`           |
| `else`     | `Keyword::Else`         |
| `loop`     | `Keyword::Loop`         |
| `while`    | `Keyword::While`        |
| `continue` | `Keyword::Continue`     |
| `break`    | `Keyword::Break`        |
| `let`      | `Keyword::Let`          |
| `return`   | `Keyword::Return`       |
| `as`       | `Keyword::As`           |
| `mut`      | `Keyword::Mut`          |
| `bool`     | `Keyword::Bool`         |
| `i8`       | `Keyword::I8`           |
| `u8`       | `Keyword::U8`           |
| `i16`      | `Keyword::I16`          |
| `u16`      | `Keyword::U16`          |
| `i32`      | `Keyword::I32`          |
| `u32`      | `Keyword::U32`          |
| `unit`     | `Keyword::Unit`         |
