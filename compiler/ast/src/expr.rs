use interner::Atom;
use lexer::{
    operator::{AssignOp, BinaryOp, PostfixOp, PrefixOp},
    value::Value,
};
use node::Node;

use crate::{Block, LetBinding, Type};

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(Atom),
    Float(f64),
    Integer(i64),
    Boolean(bool),
}

impl LiteralValue {
    pub fn from_value(value: Value) -> Self {
        match value {
            Value::String(atom) => Self::String(atom),
            Value::Boolean(boolean) => Self::Boolean(boolean),
            Value::Integer(int) => Self::Integer(int),
            Value::Float(float) => Self::Float(float),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        node: Node,
        left: Box<Expr>,
        operator: BinaryOp,
        right: Box<Expr>,
    },
    PrefixUnary {
        node: Node,
        operator: PrefixOp,
        operand: Box<Expr>,
    },
    PostfixUnary {
        node: Node,
        operand: Box<Expr>,
        operator: PostfixOp,
    },
    Paren {
        node: Node,
        expr: Box<Expr>,
    },
    Assign {
        node: Node,
        target: AssignTarget,
        operator: AssignOp,
        expr: Box<Expr>,
    },
    Literal {
        node: Node,
        value: LiteralValue,
    },
    Break {
        node: Node,
    },
    Continue {
        node: Node,
    },
    Call {
        node: Node,
        callee: AssignTarget,
        args: Vec<Expr>,
    },
    /// some_array[5] or obj::another_array(2 + 5)
    IndexExpr {
        node: Node,
        member: AssignTarget,
        index: Box<Expr>,
    },
    /// (double)70 or (int)5.5
    Cast {
        node: Node,
        ty: Type,
        expr: Box<Expr>,
    },
    /// new [double; 20] or [str; 10]
    ArrayInit {
        node: Node,
        ty: Type,
        size: Box<Expr>,
    },
    /// [1.0, 5.0, 35.38]
    Array {
        node: Node,
        elements: Vec<Expr>,
    },
    /// |> 50.3 or any expr that does not end with a semicolon like 30.45
    Return {
        node: Node,
        init: Box<Expr>,
    },
    For {
        node: Node,
        init: ForInit,
        condition: Box<Expr>,
        update: Box<Expr>,
        block: Block,
    },
    While {
        node: Node,
        condition: Box<Expr>,
        block: Block,
    },
    Member(MemberExpr),
    Identifier(Identifier),
    If(IfExpr),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Let(Box<LetBinding>),
    Expr(Box<Expr>),
}

impl Expr {
    pub fn node(&self) -> Node {
        match self {
            Self::Binary { node, .. } => *node,
            Self::PrefixUnary { node, .. } => *node,
            Self::PostfixUnary { node, .. } => *node,
            Self::Paren { node, .. } => *node,
            Self::Assign { node, .. } => *node,
            Self::Break { node, .. } => *node,
            Self::Continue { node, .. } => *node,
            Self::Literal { node, .. } => *node,
            Self::Call { node, .. } => *node,
            Self::IndexExpr { node, .. } => *node,
            Self::Cast { node, .. } => *node,
            Self::ArrayInit { node, .. } => *node,
            Self::Array { node, .. } => *node,
            Self::While { node, .. } => *node,
            Self::Return { node, .. } => *node,
            Self::For { node, .. } => *node,
            Self::Member(member_expr) => member_expr.node,
            Self::If(if_expr) => if_expr.node,
            Self::Identifier(ident) => ident.node,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemberTy {
    Namespace,
    Property,
}

#[derive(Debug, Clone)]
pub struct MemberExpr {
    pub node: Node,
    pub ty: MemberTy,
    pub segments: Vec<Identifier>,
}

#[derive(Debug, Clone)]
pub enum IfAlternate {
    Else(Block),
    If(IfExpr),
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub node: Node,
    pub condition: Box<Expr>,
    pub block: Block,
    pub alternate: Option<Box<IfAlternate>>,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub node: Node,
    pub sid: Option<Atom>,
    pub name: Atom,
}

#[derive(Debug, Clone)]
pub enum AssignTarget {
    Identifier(Identifier),
    Member(MemberExpr),
}

impl AssignTarget {
    pub fn node(&self) -> Node {
        match self {
            Self::Member(member_expr) => member_expr.node,
            Self::Identifier(ident) => ident.node,
        }
    }
}
