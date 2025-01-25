use interner::Atom;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumberSuffix {
    Single,
    Double,
    I32,
    I64,
    U32,
    U64,
    Isz,
    Usz,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    String(Atom),
    Boolean(bool),
    Integer(i64),
    Float(f64),
}
