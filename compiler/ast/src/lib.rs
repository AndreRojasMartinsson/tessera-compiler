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
        ident: Identifier,
    },
    Import {
        node: Node,
        tree: Expr,
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
    Array(Box<Type>, Box<Expr>),
    Identifier(Atom),
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
