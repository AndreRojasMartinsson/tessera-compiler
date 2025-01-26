use ast::Program;

#[derive(Default)]
pub struct ImportResolver {}

impl ImportResolver {
    pub fn resolve_program(&mut self, node: &Program) {
        // Inject standard library
    }
}
