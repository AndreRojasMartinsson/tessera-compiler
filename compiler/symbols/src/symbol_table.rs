use gxhash::HashMap;
use interner::{intern, Atom};
use uuid::Uuid;

use crate::{Symbol, GLOBALS};

#[derive(Default, Debug, Clone)]
pub struct SymbolTable {
    pub id: Atom,
    symbols: HashMap<Atom, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            id: intern!(&Uuid::new_v4().to_string()),
            symbols: HashMap::default(),
        }
    }

    /// Updates a existing symbol by the use of a transform function
    /// @throws
    pub fn update<F>(&mut self, name: &Atom, transform: F)
    where
        F: FnOnce(&mut Symbol),
    {
        if self.get(name).is_some() {
            self.symbols.entry(*name).and_modify(transform);
        } else {
            panic!(
                "Symbol does not exist. You attempted to update a non-existant symbol, make sure it exists in the symbol table before updating it."
            )
        }
    }

    pub fn define(&mut self, name: &Atom, sym: Symbol) {
        GLOBALS.lock().unwrap().insert_symbol(sym.id(), sym.clone());
        self.symbols.insert(*name, sym);
    }

    pub fn get(&self, name: &Atom) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}
