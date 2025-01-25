use ast::Type as AstType;
use interner::{intern, Atom};
use ir_builder::Type;
use uuid::Uuid;

#[derive(Debug, Default, Clone, Copy)]
pub enum Visibility {
    Private,

    #[default]
    Public,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Mutability {
    Mutable,
    Static,

    #[default]
    Immutable,
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(VariableSymbol),
    Function(FunctionSymbol),
    Import(ImportSymbol),
}

impl Symbol {
    pub fn visibility(&self) -> Visibility {
        match self {
            Symbol::Variable(var) => var.visibility,
            Symbol::Function(func) => func.visibility,
            _ => todo!(),
        }
    }

    pub fn id(&self) -> Atom {
        match self {
            Symbol::Variable(var) => var.id,
            Symbol::Function(func) => func.id,
            Symbol::Import(imp) => imp.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportSymbol {
    pub id: Atom,
    /// Vector of atom's SID's used to get variable symbol from global map
    pub name: Atom,
    pub is_alias: bool,
    /// Reference count, can be used to determine of symbol is unused
    pub ref_count: usize,
}

#[derive(Clone, Debug)]
pub enum SolvedType {
    Computed(Type),
    Function(Type, Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub id: Atom,
    pub name: Atom,
    pub ty: AstType,
    pub visibility: Visibility,
    pub mutability: Mutability,
    /// Reference count, can be used to determine of symbol is unused
    pub ref_count: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub id: Atom,
    pub name: Atom,
    pub return_ty: AstType,
    /// Vector of atom's SID's used to get variable symbol from global map.
    pub parameters: Vec<Atom>,
    pub visibility: Visibility,
    /// Reference count, can be used to determine of symbol is unused
    pub ref_count: usize,
}

impl FunctionSymbol {
    pub fn new(
        name: Atom,
        return_ty: AstType,
        parameters: Vec<Atom>,
        visibility: Visibility,
    ) -> (Self, Atom) {
        let id = intern!(&Uuid::new_v4().to_string());
        (
            Self {
                id,
                name,
                return_ty,
                parameters,
                visibility,
                ref_count: 0,
            },
            id,
        )
    }
}
impl VariableSymbol {
    pub fn new(
        name: Atom,
        ty: AstType,
        visibility: Visibility,
        mutability: Mutability,
    ) -> (Self, Atom) {
        let id = intern!(&Uuid::new_v4().to_string());
        (
            Self {
                id,
                name,
                ty,
                mutability,
                visibility,
                ref_count: 0,
            },
            id,
        )
    }
}
impl ImportSymbol {
    pub fn new(name: Atom, is_alias: bool) -> (Self, Atom) {
        let id = intern!(&Uuid::new_v4().to_string());

        (
            Self {
                id,
                name,
                is_alias,
                ref_count: 0,
            },
            id,
        )
    }
}
