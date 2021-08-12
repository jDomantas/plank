use ast::resolved::Symbol;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Symbols {
    next_symbol: u32,
    symbol_names: HashMap<Symbol, String>,
}

impl Symbols {
    pub fn new() -> Symbols {
        let mut names = HashMap::new();
        names.insert(::builtins::SIZE_OF, "size_of".into());
        names.insert(::builtins::ALIGN_OF, "align_of".into());
        names.insert(::builtins::GETC, "@getc".into());
        names.insert(::builtins::PUTC, "@putc".into());
        names.insert(::builtins::SIZE_OF_TYPE_PARAM, "T".into());
        names.insert(::builtins::ALIGN_OF_TYPE_PARAM, "T".into());
        names.insert(::builtins::PUTC_PARAM, "ch".into());
        Symbols {
            next_symbol: names.len() as u32,
            symbol_names: names,
        }
    }

    pub fn new_symbol<S: Into<String>>(&mut self, name: S) -> Symbol {
        let symbol = Symbol(self.next_symbol);
        self.next_symbol += 1;
        self.symbol_names.insert(symbol, name.into());
        symbol
    }

    pub fn get_name(&self, symbol: Symbol) -> &str {
        &self.symbol_names[&symbol]
    }
}
