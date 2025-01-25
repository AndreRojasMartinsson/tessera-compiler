pub use globals::GLOBALS;
pub use symbol::{
    FunctionSymbol, ImportSymbol, Mutability, SolvedType, Symbol, VariableSymbol, Visibility,
};
pub use symbol_table::SymbolTable;

mod globals;
mod symbol;
mod symbol_table;

#[cfg(test)]
mod symbol_tests {
    use super::*;

    #[test]
    fn test_default_visibility() {
        let default_visibility = Visibility::default();
        match default_visibility {
            Visibility::Public => {}
            _ => panic!("Expected default visibility to be Public"),
        }
    }

    #[test]
    fn test_private_visibility() {
        let private_visibility = Visibility::Private;
        match private_visibility {
            Visibility::Private => {}
            _ => panic!("Expected visibility to be Private"),
        }
    }

    #[test]
    fn test_default_mutability() {
        let default_mutability = Mutability::default();
        match default_mutability {
            Mutability::Immutable => {}
            _ => panic!("Expected default mutability to be Immutable"),
        }
    }

    #[test]
    fn test_mutable_mutability() {
        let mutable_mutability = Mutability::Mutable;
        match mutable_mutability {
            Mutability::Mutable => {}
            _ => panic!("Expected mutability to be Mutable"),
        }
    }

    #[test]
    fn test_static_mutability() {
        let static_mutability = Mutability::Static;
        match static_mutability {
            Mutability::Static => {}
            _ => panic!("Expected mutability to be Static"),
        }
    }
}
