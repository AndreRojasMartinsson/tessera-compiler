use std::fmt::Display;

pub use expr::*;
use interner::Atom;
use node::Node;

#[derive(Debug, Clone)]
pub struct Program {
    pub node: Node,
    pub items: Vec<ProgramItem>,
    pub sid: Atom,
}

#[derive(Debug, Clone)]
pub enum ProgramItem {
    Module {
        node: Node,
        tree: MemberExpr,
    },
    Import {
        node: Node,
        tree: MemberExpr,
    },
    ExternalFunction {
        node: Node,
        ty: Type,
        ident: Identifier,
        parameters: Vec<Parameter>,
    },
    Function {
        node: Node,
        ty: Type,
        ident: Identifier,
        parameters: Vec<Parameter>,
        public: bool,
        body: Block,
    },
}

#[derive(Debug, Clone)]
pub struct Type {
    pub node: Node,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Ty {
    U32,
    U64,
    I32,
    I64,
    Str,
    Bool,
    Double,
    Single,
    Void,
    Array(Box<Type>, Box<Option<Expr>>),
    Identifier(Atom),
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(atom) => write!(f, "{atom}"),
            Self::Array(ty, _) => write!(f, "{}", ty.ty,),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Str => write!(f, "str"),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Double => write!(f, "double"),
            Self::Single => write!(f, "single"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub node: Node,
    pub ty: Type,
    pub ident: Identifier,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub node: Node,
    pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub struct LetBinding {
    pub node: Node,
    pub ident: Identifier,
    pub ty: Type,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Expr {
        node: Node,
        semicolon: bool,
        expr: Expr,
    },
    Out {
        node: Node,
        format_str: Expr,
        arguments: Vec<Expr>,
    },
    Let(LetBinding),
    Break {
        node: Node,
    },
    Continue {
        node: Node,
    },
}

mod expr;
