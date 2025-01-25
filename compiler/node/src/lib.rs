use std::ops::Range;

#[derive(Debug, Copy, Clone, Default)]
pub struct Node {
    pub start: usize,
    pub end: usize,
}

impl Node {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn from_span(span: Range<usize>) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}
