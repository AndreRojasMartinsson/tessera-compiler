use std::{ffi::OsStr, path::Path};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context<'ctx> {
    pub name: &'ctx OsStr,
    pub path: &'ctx Path,
    pub diagnostics: Vec<()>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(name: &'ctx OsStr, path: &'ctx Path) -> Self {
        Self {
            name,
            diagnostics: Vec::new(),
            path,
        }
    }
}
