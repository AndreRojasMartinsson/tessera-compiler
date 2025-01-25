use std::fmt::Display;

use crate::Kind;

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
    /// ++,
    Increment,
    /// --,
    Decrement,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    /// -,
    Neg,
    /// !,
    LogNot,
    /// ~,
    Not,
    /// *,
    Deref,
    /// &,
    Addr,
    /// new,
    New,
    /// sizeof,
    Sizeof,

    /// ++,
    Increment,
    /// --,
    Decrement,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    /// +=,
    Add,
    /// -=,
    Sub,
    /// *=,
    Mul,
    /// /=,
    Div,
    Assign,
    /// %=,
    Rem,
    /// &=,
    And,
    /// |=,
    Or,
    /// ^=,
    Xor,
    /// <<=,
    Shl,
    /// >>=,
    Shr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    /// ==,
    Equal,
    /// !=,
    NotEqual,
    /// +,
    Add,
    /// -,
    Sub,
    /// *,
    Mul,
    /// /,
    Div,
    /// %,
    Rem,
    /// &,
    And,
    /// |,
    Or,
    /// ^,
    Xor,
    /// <
    Lt,
    /// >,
    Gt,
    /// >=,
    Gte,
    /// <=,
    Lte,
    /// <<,
    Shl,
    /// >>,
    Shr,
    /// &&,
    LogAnd,
    /// ||,
    LogOr,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Xor => write!(f, "^"),
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
            Self::Lte => write!(f, "<="),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
            Self::LogAnd => write!(f, "&&"),
            Self::LogOr => write!(f, "||"),
        }
    }
}

impl PostfixOp {
    pub fn from_kind(kind: Kind) -> Option<Self> {
        match kind {
            Kind::Increment => Some(Self::Increment),
            Kind::Decrement => Some(Self::Decrement),
            _ => None,
        }
    }
}
impl PrefixOp {
    pub fn from_kind(kind: Kind) -> Option<Self> {
        match kind {
            Kind::Dash => Some(Self::Neg),
            Kind::Bang => Some(Self::LogNot),
            Kind::Tilde => Some(Self::Not),
            Kind::Increment => Some(Self::Increment),
            Kind::Decrement => Some(Self::Decrement),
            Kind::Asterisk => Some(Self::Deref),
            Kind::Ampersand => Some(Self::Addr),
            Kind::SizeofKw => Some(Self::Sizeof),
            Kind::NewKw => Some(Self::New),
            _ => None,
        }
    }
}

impl AssignOp {
    pub fn from_kind(kind: Kind) -> Option<Self> {
        match kind {
            Kind::AddAssign => Some(Self::Add),
            Kind::SubAssign => Some(Self::Sub),
            Kind::MulAssign => Some(Self::Mul),
            Kind::DivAssign => Some(Self::Div),
            Kind::ModAssign => Some(Self::Rem),
            Kind::AndAssign => Some(Self::And),
            Kind::OrAssign => Some(Self::Or),
            Kind::XorAssign => Some(Self::Xor),
            Kind::Assign => Some(Self::Assign),
            Kind::ShlAssign => Some(Self::Shl),
            Kind::ShrAssign => Some(Self::Shr),
            _ => None,
        }
    }
}

impl BinaryOp {
    pub fn from_kind(kind: Kind) -> Option<Self> {
        match kind {
            Kind::Equal => Some(Self::Equal),
            Kind::NotEqual => Some(Self::NotEqual),
            Kind::Plus => Some(Self::Add),
            Kind::Dash => Some(Self::Sub),
            Kind::Asterisk => Some(Self::Mul),
            Kind::Slash => Some(Self::Div),
            Kind::Percent => Some(Self::Rem),
            Kind::Ampersand => Some(Self::And),
            Kind::Pipe => Some(Self::Or),
            Kind::Xor => Some(Self::Xor),
            Kind::Lt => Some(Self::Lt),
            Kind::Gt => Some(Self::Gt),
            Kind::Gte => Some(Self::Gte),
            Kind::Lte => Some(Self::Lte),
            Kind::Shl => Some(Self::Shl),
            Kind::Shr => Some(Self::Shr),
            Kind::LogicalAnd => Some(Self::LogAnd),
            Kind::LogicalOr => Some(Self::LogOr),
            _ => None,
        }
    }
}
