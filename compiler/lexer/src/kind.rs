#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum Kind {
    // Miscellaneous
    Eof,
    Semicolon,
    #[default]
    Illegal,
    Identifier,

    // Single character tokens
    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    LSquirly,
    RSquirly,
    Comma,
    Colon,
    Period,

    // Operators
    Plus,
    Bang,
    Dash,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Xor,
    Tilde,
    Asterisk,
    Gt,
    Lt,
    Assign,

    // Double Character Tokens
    Accessor,
    FatArrow,
    Arrow,
    Chevron,

    // Operators
    Shl,
    Shr,
    Gte,
    Lte,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
    Increment,
    Decrement,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,

    // Triple character Operators
    ShlAssign,
    ShrAssign,

    // Keywords
    LetKw,
    IfKw,
    ForKw,
    ElseKw,
    PubKw,
    NewKw,
    WhileKw,
    OutKw,
    FuncKw,
    ModuleKw,
    ImportKw,
    DefKw,
    ContinueKw,
    BreakKw,
    ReturnKw,
    SizeofKw,
    UnionKw,
    StructKw,
    EnumKw,
    ExternKw,

    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    BoolLiteral,

    // Types
    /// void, u32, i32, double, single, str, bool, u64, i64
    PrimitiveType,
}
