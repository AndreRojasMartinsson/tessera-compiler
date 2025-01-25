use std::sync::{Arc, LazyLock, Mutex};

use gxhash::HashMap;
use interner::Atom;

use crate::{Symbol, SymbolTable};

#[derive(Default, Debug)]
pub struct Globals {
    scopes: HashMap<Atom, Arc<SymbolTable>>,
    symbols: HashMap<Atom, Symbol>,
}

impl Globals {
    pub fn insert_scope(&mut self, scope_id: Atom, scope: Arc<SymbolTable>) {
        self.scopes.insert(scope_id, scope);
    }

    pub fn insert_symbol(&mut self, symbol_id: Atom, symbol: Symbol) {
        self.symbols.insert(symbol_id, symbol);
    }

    pub fn update_symbol<F>(&mut self, symbol_id: Atom, transform: F)
    where
        F: FnOnce(&mut Symbol),
    {
        self.symbols.entry(symbol_id).and_modify(transform);
    }

    pub fn lookup_symbol(&self, symbol_id: Atom) -> Option<&Symbol> {
        self.symbols.get(&symbol_id)
    }

    pub fn lookup_scope(&self, scope_id: Atom) -> Option<&SymbolTable> {
        self.scopes.get(&scope_id).map(|v| &**v)
    }
}

pub static GLOBALS: LazyLock<Mutex<Globals>> = LazyLock::new(|| Mutex::new(Globals::default()));
