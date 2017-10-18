use std::collections::HashMap;
use ast::resolved::Symbol;


#[derive(Debug, Default)]
pub struct Symbols {
    next_symbol: u32,
    symbol_names: HashMap<Symbol, String>,
}

impl Symbols {
    pub fn new_symbol<S: Into<String>>(&mut self, name: S) -> Symbol {
        let symbol = Symbol::new(self.next_symbol);
        self.next_symbol += 1;
        self.symbol_names.insert(symbol, name.into());
        symbol
    }

    #[allow(dead_code)]
    pub fn get_name(&self, symbol: Symbol) -> &str {
        &self.symbol_names[&symbol]
    }
}
