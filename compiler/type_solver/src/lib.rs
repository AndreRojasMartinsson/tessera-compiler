use ast::{
    Block, Expr, ForInit, Identifier, IfAlternate, LetBinding, Parameter, Program, ProgramItem, Ty,
    Type,
};
use gxhash::HashMap;
use interner::{Atom, lookup};
use qbe::Type as IRType;
use symbols::SolvedType;

#[derive(Default, Debug)]
pub struct TypeSolver<'a> {
    resolved_identifiers: HashMap<Atom, SolvedType<'a>>,
}

pub fn ty_to_ir_type(ty: Ty, resolved_identifiers: HashMap<Atom, SolvedType>) -> IRType {
    match ty {
        Ty::U32 | Ty::I32 => IRType::Word,
        Ty::U64 | Ty::I64 => IRType::Long,
        Ty::Double => IRType::Double,
        Ty::Single => IRType::Single,
        Ty::Identifier(ident) => match resolved_identifiers.get(&ident).unwrap() {
            SolvedType::Computed(ty) => ty.clone(),
            SolvedType::Function(ty, _) => ty.clone(),
        },
        Ty::Array(_ty, _expr) => todo!(),
        Ty::Bool => todo!(),
        Ty::Str => IRType::Long,
        _ => unreachable!(),
    }
}

fn solve_type(ty: Ty, resolved_identifiers: HashMap<Atom, SolvedType>) -> Option<SolvedType> {
    match ty {
        Ty::U32 | Ty::I32 => Some(SolvedType::Computed(IRType::Word)),
        Ty::U64 | Ty::I64 => Some(SolvedType::Computed(IRType::Long)),
        Ty::Double => Some(SolvedType::Computed(IRType::Double)),
        Ty::Single => Some(SolvedType::Computed(IRType::Single)),
        Ty::Identifier(ident) => Some(resolved_identifiers.get(&ident).unwrap().clone()),
        Ty::Array(_ty, _expr) => todo!(),
        Ty::Bool => todo!(),
        Ty::Void => None,
        Ty::Str => Some(SolvedType::Computed(IRType::Long)),
    }
}

impl TypeSolver<'_> {
    pub fn solve(&mut self, node: &Program) {
        for item in node.items.clone() {
            match item {
                ProgramItem::Module { .. } => {}
                ProgramItem::Import { .. } => {}
                ProgramItem::Function {
                    ty,
                    ident,
                    parameters,
                    body,
                    ..
                } => self.solve_func(ty, ident, parameters, body),
            }
        }
    }

    pub fn get_resolved_ident(&self, ident: Identifier) -> Option<&SolvedType> {
        self.resolved_identifiers.get(&ident.sid.unwrap())
    }

    fn solve_func(
        &mut self,
        return_ty: Type,
        ident: Identifier,
        parameters: Vec<Parameter>,
        body: Block,
    ) {
        if let Some(return_ty) = solve_type(return_ty.ty, self.resolved_identifiers.clone()) {
            let params: Vec<IRType> = parameters
                .iter()
                .map(|param| {
                    let ident = param.ident.clone();
                    let param_ty = param.ty.clone();

                    self.resolved_identifiers.insert(
                        ident.sid.unwrap(),
                        SolvedType::Computed(ty_to_ir_type(
                            param_ty.ty.clone(),
                            self.resolved_identifiers.clone(),
                        )),
                    );

                    ty_to_ir_type(param_ty.ty, self.resolved_identifiers.clone())
                })
                .collect();

            self.resolved_identifiers.insert(
                ident.sid.unwrap(),
                SolvedType::Function(
                    match return_ty {
                        SolvedType::Computed(var) => var,
                        SolvedType::Function(var, _) => var,
                    },
                    params,
                ),
            );
        }

        self.solve_block(body);
    }

    fn solve_block(&mut self, block: Block) {
        for item in block.items {
            match item {
                ast::BlockItem::Expr {
                    semicolon, expr, ..
                } => self.solve_expr(semicolon, expr),
                ast::BlockItem::Let(binding) => self.solve_let_binding(binding),
                ast::BlockItem::Continue { .. }
                | ast::BlockItem::Break { .. }
                | ast::BlockItem::Out { .. } => {}
            }
        }
    }

    fn solve_expr(&mut self, _semicolon: bool, expr: Expr) {
        match expr {
            Expr::Literal { .. }
            | Expr::Break { .. }
            | Expr::Continue { .. }
            | Expr::Identifier(_) => {}
            Expr::If(if_expr) => {
                self.solve_expr(false, *if_expr.condition);
                self.solve_block(if_expr.block);

                if let Some(alternate) = if_expr.alternate {
                    match *alternate {
                        IfAlternate::If(expr) => self.solve_expr(false, Expr::If(expr)),
                        IfAlternate::Else(block) => self.solve_block(block),
                    };
                };
            }
            Expr::Return { init, .. } => {
                self.solve_expr(false, *init);
            }
            Expr::While {
                condition, block, ..
            } => {
                self.solve_expr(false, *condition);
                self.solve_block(block);
            }
            Expr::For {
                init,
                condition,
                update,
                block,
                ..
            } => {
                match init {
                    ForInit::Let(binding) => self.solve_let_binding(*binding),
                    ForInit::Expr(expr) => self.solve_expr(false, *expr),
                };

                self.solve_expr(false, *condition);
                self.solve_expr(false, *update);
                self.solve_block(block);
            }
            Expr::Call { args, .. } => {
                args.iter()
                    .for_each(|arg| self.solve_expr(false, arg.clone()));
            }
            Expr::PrefixUnary { operand, .. } => {
                self.solve_expr(false, *operand);
            }
            Expr::PostfixUnary { operand, .. } => {
                self.solve_expr(false, *operand);
            }
            Expr::Cast { ty, expr, .. } => {
                self.solve_expr(false, *expr);
            }
            Expr::Binary { left, right, .. } => {
                self.solve_expr(false, *left);
                self.solve_expr(false, *right);
            }
            Expr::Assign { expr, .. } => {
                self.solve_expr(false, *expr);
            }

            c => unreachable!("{c:#?}"),
        }
    }

    fn solve_let_binding(&mut self, binding: LetBinding) {
        let ty = ty_to_ir_type(binding.ty.ty, self.resolved_identifiers.clone());

        self.resolved_identifiers
            .insert(binding.ident.sid.unwrap(), SolvedType::Computed(ty));
    }
}
