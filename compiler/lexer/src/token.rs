use std::ops::Range;

use interner::{intern, Atom};

use crate::{value::Value, Kind};

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Token {
    pub kind: Kind,
    pub lexeme: Atom,
    pub value: Option<Value>,
    pub start: usize,
    pub end: usize,
}

impl Token {
    pub fn new(kind: Kind, value: Option<Value>, span: Range<usize>, lexeme: &str) -> Self {
        Self {
            value,
            kind,
            start: span.start,
            end: span.end,
            lexeme: intern!(lexeme),
        }
    }
}
