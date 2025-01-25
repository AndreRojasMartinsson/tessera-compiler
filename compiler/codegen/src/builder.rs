// use crate::{ExprResult, context::Context};
// use ast::{Expr, Identifier, LetBinding, Parameter, Program, ProgramItem, Type as AstType};
// use gxhash::HashMap;
// use interner::{Atom, lookup};
// use lexer::operator::BinaryOp;
// use qbe::{Function, Instr, Linkage, Type, Value};
// use symbols::{GLOBALS, SolvedType, Visibility};
//
// pub struct IRBuilder<'a> {
//     ctx: &'a mut Context<'a>,
//     variables: HashMap<Atom, Value>,
// }
//
// impl<'a> IRBuilder<'a> {
//     pub fn build(&mut self, node: Program) {
//         for item in node.items {
//             match item {
//                 ProgramItem::Module { .. } => {}
//                 ProgramItem::Import { .. } => {}
//                 ProgramItem::Function {
//                     ty,
//                     ident,
//                     parameters,
//                     body,
//                     ..
//                 } => {
//                     self.build_function(ident, parameters, body);
//                 }
//             }
//         }
//     }
//
//     fn build_function(&mut self, ident: Identifier, parameters: Vec<Parameter>, body: ast::Block) {
//         let ir_type = self
//             .ctx
//             .type_solver
//             .get_resolved_ident(ident.clone())
//             .expect("Expected a resolved identifier, got None.");
//
//         let globals = GLOBALS.lock().expect("Expected GLOBALS instance.");
//         let param_names: Vec<Atom> = parameters.iter().map(|param| param.ident.name).collect();
//         let sym = globals.lookup_symbol(ident.sid.unwrap()).unwrap();
//
//         let linkage = match sym.visibility() {
//             Visibility::Public => Linkage::public(),
//             Visibility::Private => Linkage::private(),
//         };
//
//         let (return_ty, params) = match ir_type {
//             SolvedType::Computed(ty) => (ty, vec![]),
//             SolvedType::Function(return_ty, params) => (
//                 return_ty,
//                 params
//                     .iter()
//                     .enumerate()
//                     .map(|(idx, param)| {
//                         (
//                             param.clone(),
//                             Value::Temporary(lookup!(param_names.get(idx).unwrap())),
//                         )
//                     })
//                     .collect(),
//             ),
//         };
//
//         let mut func = Function::new(
//             linkage,
//             lookup!(ident.name),
//             params,
//             Some(return_ty.clone()),
//         );
//
//         func.add_block("start");
//
//         let return_temp = self.ctx.new_temporary();
//         func.assign_instr(return_temp.clone(), Type::Long, Instr::Alloc4(4));
//
//         self.ctx.function = Some(func);
//
//         for item in body.items {
//             match item {
//                 ast::BlockItem::Let(binding) => self.build_let_binding(binding),
//                 _ => unreachable!(),
//             }
//         }
//
//         if let Some(ref mut func) = self.ctx.function {
//             func.add_instr(Instr::Ret(Some(return_temp)));
//             self.ctx.module.add_function(func.clone());
//         }
//     }
//
//     fn build_let_binding(&mut self, item: LetBinding) {
//         let var_name = self.ctx.new_temporary();
//         let return_temp = self.ctx.new_temporary();
//
//         if let Some(ref mut func) = self.ctx.function {
//             self.variables
//                 .insert(item.ident.sid.unwrap(), return_temp.clone());
//
//             func.assign_instr(return_temp, Type::Long, Instr::Alloc4(4));
//
//             if let Some(init) = item.init {
//                 let expr = self.build_expr(init);
//             }
//         }
//     }
//
//     fn allocate_value(&mut self, expr_result: ExprResult) -> Value {
//         if expr_result.in_memory {
//             expr_result.value.clone()
//         } else {
//             self.ctx.new_temporary().clone()
//         }
//     }
//
//     fn build_expr(&mut self, item: Expr) -> ExprResult<'_> {
//         match item {
//             Expr::Binary {
//                 left,
//                 operator,
//                 right,
//                 ..
//             } => {
//                 let left_expr = self.build_expr(*left);
//                 let right_expr = self.build_expr(*right);
//
//                 let expr_type: Type;
//
//                 match (left_expr.ty.clone(), right_expr.ty.clone()) {
//                     (Type::Double, Type::Double) => expr_type = Type::Double,
//                     (Type::Single, Type::Single) => expr_type = Type::Single,
//                     (Type::Word, Type::Word) => expr_type = Type::Word,
//                     (Type::Long, Type::Long) => expr_type = Type::Long,
//                     (Type::Byte, Type::Byte) => expr_type = Type::Byte,
//                     (Type::SignedByte, Type::SignedByte) => expr_type = Type::SignedByte,
//                     (Type::SignedHalfword, Type::SignedHalfword) => {
//                         expr_type = Type::SignedHalfword
//                     }
//                     (Type::Zero, Type::Zero) => expr_type = Type::Zero,
//                     (Type::Halfword, Type::Halfword) => expr_type = Type::Halfword,
//                     (Type::UnsignedByte, Type::UnsignedByte) => expr_type = Type::UnsignedByte,
//                     (Type::UnsignedHalfword, Type::UnsignedHalfword) => {
//                         expr_type = Type::UnsignedHalfword
//                     }
//                     (Type::Aggregate(agg), Type::Aggregate(agg2)) if agg == agg2 => {
//                         expr_type = Type::Aggregate(agg)
//                     }
//                     _ => panic!("Failed to infer"),
//                 };
//
//                 let left_value = self.allocate_value(left_expr.clone());
//             }
//             _ => todo!(),
//         };
//
//         ExprResult {
//             value: Value::Temporary("".into()),
//             ty: Type::Long,
//             in_memory: false,
//         }
//     }
// }
