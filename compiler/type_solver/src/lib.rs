use ast::{
    Block, Expr, ForInit, Identifier, IfAlternate, LetBinding, Parameter, Program, ProgramItem,
    TypeNode,
};

use gxhash::HashMap;
use interner::{intern, Atom};
use ir_builder::Type;
use symbols::SolvedType;

#[derive(Default, Debug)]
pub struct TypeSolver {
    resolved_identifiers: HashMap<Atom, SolvedType>,
}

pub fn ty_to_ir_type(ty: Type, resolved_identifiers: HashMap<Atom, SolvedType>) -> Type {
    match ty {
        Type::Unknown(ident) => match resolved_identifiers.get(&intern!(ident.as_ref())).unwrap() {
            SolvedType::Computed(ty) => ty.clone(),
            SolvedType::Function(ty, _) => ty.clone(),
        },
        // Ty::Identifier(ident) => match resolved_identifiers.get(&ident).unwrap() {
        //     SolvedType::Computed(ty) => ty.clone(),
        //     SolvedType::Function(ty, _) => ty.clone(),
        // },
        _ => ty,
    }
}

fn solve_type(ty: Type, resolved_identifiers: HashMap<Atom, SolvedType>) -> Option<SolvedType> {
    match ty {
        Type::Unknown(ident) => Some(
            resolved_identifiers
                .get(&intern!(ident.as_ref()))
                .unwrap()
                .clone(),
        ),
        Type::Void => None,
        _ty => Some(SolvedType::Computed(_ty)),
    }
}

impl TypeSolver {
    pub fn solve(&mut self, node: &Program) {
        for item in node.items.clone() {
            match item {
                ProgramItem::Module { .. } => {}
                ProgramItem::Import { .. } => {}
                ProgramItem::ExternalFunction { .. } => {}
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
        return_ty: TypeNode,
        ident: Identifier,
        parameters: Vec<Parameter>,
        body: Block,
    ) {
        if let Some(return_ty) = solve_type(return_ty.ty, self.resolved_identifiers.clone()) {
            let params: Vec<Type> = parameters
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
            Expr::Cast { expr, .. } => {
                self.solve_expr(false, *expr);
            }
            Expr::Binary { left, right, .. } => {
                self.solve_expr(false, *left);
                self.solve_expr(false, *right);
            }
            Expr::Assign { expr, .. } => {
                self.solve_expr(false, *expr);
            }
            Expr::Paren { expr, .. } => {
                self.solve_expr(_semicolon, *expr);
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
