use gxhash::{HashMap, HashMapExt};
use interner::{lookup, Atom};
use lexer::operator::{AssignOp, BinaryOp, PostfixOp, PrefixOp};
use node::Node;
use std::{
    cell::RefCell,
    cmp::Ordering,
    fs::{self, File},
    io::{BufWriter, Write},
    path::PathBuf,
    sync::Arc,
};

use ast::{
    AssignTarget, Block as AstBlock, Expr, ForInit, IfAlternate, LetBinding, LiteralValue,
    Parameter, Program, ProgramItem, Ty, Type as AstType,
};
use ir_builder::{
    Block, Cmp, Data, DataItem, Function, ImutStr, Instruction, Linkage, Module, Prefix, Statement,
    StructPool, Type, Value, GC_NOOP,
};

macro_rules! hashmap {
    () => {
        gxhash::HashMap::new()
    };

    ($key:ty, $val:ty) => {
        gxhash::HashMap::new() as gxhash::HashMap<$key, $val>
    };

    ($( $key:expr => $value:expr ),* $(,)?) => {{
        let mut map = gxhash::HashMap::new();
        $(
            map.insert($key, $value);
        )*
        map
    }};
}

#[macro_export]
macro_rules! is_generic {
    ($name:expr $(,)?) => {
        $name.contains(&format!(".{}.", ir_builder::GENERIC_IDENTIFIER))
    };
}

#[macro_export]
macro_rules! is_unknown {
    ($name:expr $(,)?) => {
        $name.contains(&format!(".{}.", ir_builder::GENERIC_UNKNOWN))
    };
}

#[derive(Debug, Clone)]
struct Context<'a> {
    func: &'a RefCell<Function>,
    module: &'a RefCell<Module>,
}

pub struct CodeGen {
    temp_count: u32,
    data_sections: Vec<Data>,
    scopes: Vec<HashMap<String, (Type, Value)>>,
    // Struct Name => ((Field Name, Field Type)[])
    #[allow(dead_code)]
    struct_pool: StructPool,
    loop_labels: Vec<ImutStr>,
    tree: Program,
    /// Map from temporary ot its stack allocated address
    address_pool: HashMap<String, Value>,
}

struct ExtFunc {
    params: Vec<(Type, Value)>,
    name: String,
    return_ty: Option<Type>,
}

impl ExtFunc {
    pub fn new(params: &[(Type, &str)], name: &str, return_ty: Option<Type>) -> Self {
        Self {
            params: params
                .to_vec()
                .iter()
                .map(|(param_ty, param_name)| {
                    (param_ty.clone(), Value::Temp(param_name.to_string().into()))
                })
                .collect::<Vec<(Type, Value)>>(),
            name: name.to_string(),
            return_ty,
        }
    }
}

impl CodeGen {
    fn inject_libc_functions(module: &RefCell<Module>) {
        // let funcs = [];
        let funcs: &[ExtFunc] = &[
            ExtFunc::new(&[(Type::Word, "exit_code")], "exit", None),
            ExtFunc::new(&[(Type::Double, "x")], "log", Some(Type::Double)),
            ExtFunc::new(&[(Type::Double, "x")], "logf", Some(Type::Double)),
            ExtFunc::new(&[(Type::Double, "x")], "atan", Some(Type::Double)),
            ExtFunc::new(&[(Type::Double, "x")], "atanf", Some(Type::Double)),
            // string comparison
            ExtFunc::new(
                &[(Type::Pointer(Box::new(Type::Char)), "s")],
                "strlen",
                Some(Type::Long),
            ),
            ExtFunc::new(
                &[
                    (Type::Pointer(Box::new(Type::Void)), "a1"),
                    (Type::Pointer(Box::new(Type::Void)), "a2"),
                    (Type::Long, "n"),
                ],
                "memcmp",
                Some(Type::Long),
            ),
            ExtFunc::new(
                &[
                    (Type::Pointer(Box::new(Type::Char)), "s1"),
                    (Type::Pointer(Box::new(Type::Char)), "s2"),
                ],
                "strcmp",
                Some(Type::Long),
            ),
        ];

        for func_signature in funcs {
            let func = Function {
                name: func_signature.name.clone().into(),
                external: true,
                return_type: func_signature.return_ty.clone(),
                blocks: Vec::with_capacity(0),
                linkage: Linkage::public(),
                parameters: func_signature.params.clone(),
            };

            module.borrow_mut().add_function(func);
        }
    }

    pub fn compile(tree: Program, object_output: bool, print_ir: bool) -> Result<String, String> {
        let mut generator = Self {
            temp_count: 0,
            scopes: vec![],
            data_sections: vec![],
            struct_pool: hashmap![],
            loop_labels: vec![],
            tree,
            address_pool: hashmap![],
        };

        let module = Module::new();
        let module_ref = RefCell::new(module);

        if generator.tree.items.iter().any(|item|
            matches!(item, ProgramItem::Function { ident, .. } if &(lookup!(ident.name).to_owned()) == "main"))
                && !object_output
        {
            panic!(
                "ERROR: Could not compile module\n{}\n\n{}\n{}\n{}\n",
                "-".repeat(40),
                "Module has no entry-point. To create one, write:",
                "pub func u32 main() {\n\n\n}",
                "-".repeat(40)
            )
        }

        Self::inject_libc_functions(&module_ref);

        generator.tree.items.clone().iter().for_each(|item| {
            if let ProgramItem::Function {
                ty,
                ident,
                parameters,
                public,
                body,
                ..
            } = item
            {
                let func = generator.generate_function(
                    ident.name,
                    *public,
                    parameters,
                    Some(Self::ast_type_to_type(ty.clone())),
                    body.clone(),
                    &module_ref,
                );

                module_ref.borrow_mut().add_function(func);
            }
        });

        for data in generator.data_sections {
            module_ref.borrow_mut().add_data(data);
        }
        //
        // module_ref
        //     .borrow_mut()
        //     .remove_unused_functions(object_output, None);
        //
        module_ref.borrow_mut().remove_unused_data();
        module_ref.borrow_mut().remove_empty_structs();

        module_ref
            .borrow_mut()
            .functions
            .retain(|func| !func.external);

        if print_ir {
            println!("{}", module_ref.borrow());
        }

        let ir = module_ref.borrow().to_string();

        Ok(ir)
    }

    fn tmp_name_with_debug_assertions(&self, name: &ImutStr, minify: bool) -> String {
        if cfg!(debug_assertions) || !minify {
            format!("{}.{}", name, self.temp_count)
        } else {
            format!(".{}", self.temp_count)
        }
    }

    fn new_temporary(&mut self, name: Option<&str>, minify: bool) -> Value {
        self.temp_count += 1;
        Value::Temp(
            self.tmp_name_with_debug_assertions(&name.unwrap_or("tmp").into(), minify)
                .into(),
        )
    }

    fn new_variable(&mut self, ty: &Type, name: ImutStr, new: bool, minify: bool) -> Value {
        let tmp = if new {
            self.new_temporary(Some(&name), minify)
        } else {
            let existing_var = self.get_variable(&name, None);

            match existing_var {
                Ok((_, val)) => match val {
                    Value::Temp(_) => val,
                    _ => self.new_temporary(Some(&name), minify),
                },
                Err(_) => self.new_temporary(Some(&name), minify),
            }
        };

        let scope = self
            .scopes
            .last_mut()
            .expect("Expected last scope to exist");

        scope.insert(name.to_string(), (ty.to_owned(), tmp.to_owned()));
        tmp
    }

    fn get_variable(
        &mut self,
        name: &ImutStr,
        module: Option<&RefCell<Module>>,
    ) -> Result<(Type, Value), String> {
        let var = self
            .scopes
            .iter()
            .rev()
            .filter_map(|s| s.get(&name.to_string()))
            .next()
            .ok_or_else(|| format!("\nUndefined variable '{}'{}", name, " "));

        if var.is_err() {
            for item in self.tree.items.iter().cloned() {
                if let ProgramItem::Function { ident, .. } = item {
                    let op_name: ImutStr = lookup!(ident.name).into();
                    if *name == op_name {
                        return Ok((
                            Type::Function(Box::new(if let Some(module) = module {
                                module
                                    .borrow()
                                    .functions
                                    .iter()
                                    .find(|func| func.name == *name)
                                    .cloned()
                            } else {
                                None
                            })),
                            Value::Global(name.clone()),
                        ));
                    }
                }
            }
        }

        var.cloned()
    }

    fn get_variable_lazy(
        &mut self,
        name: &ImutStr,
        func: Option<&RefCell<Function>>,
        module: Option<&RefCell<Module>>,
    ) -> Option<(Type, Value)> {
        let var = self.get_variable(name, module);

        match var {
            Ok((ty, val)) => {
                let res = self.get_variable(&format!("{}.addr", name).into(), module);
                if res.is_ok() && func.is_some() {
                    let (_, addr_val) = res.unwrap();

                    func.unwrap().borrow_mut().assign_instruction(
                        &val,
                        &ty,
                        Instruction::Load(ty.clone(), addr_val),
                    );

                    return Some((ty, val));
                }

                Some((ty, val))
            }
            Err(msg) => {
                macro_rules! undefined_error {
                    () => {
                        panic!(
                            "Unexpected error when trying to get a variable called '{}': {}",
                            name, msg
                        )
                    };
                }

                if module.is_none() {
                    undefined_error!();
                }

                let tmp_modle = module.unwrap().borrow();
                let global = tmp_modle.data.iter().find(|item| item.name == name.clone());

                if let Some(item) = global {
                    Some((Type::Long, Value::Global(item.name.clone())))
                } else {
                    undefined_error!()
                }
            }
        }
    }

    fn ast_type_to_type(ast_ty: AstType) -> Type {
        match ast_ty.ty {
            Ty::U32 => Type::UnsignedWord,
            Ty::U64 => Type::UnsignedLong,
            Ty::I32 => Type::Word,
            Ty::I64 => Type::Long,
            Ty::Double => Type::Double,
            Ty::Single => Type::Single,
            Ty::Bool => Type::Boolean,
            Ty::Str => Type::Pointer(Box::new(Type::Char)),
            Ty::Void => Type::Void,
            c => unreachable!("{c:?}"),
        }
    }

    fn generate_function(
        &mut self,
        ident: Atom,
        public: bool,
        parameters: &Vec<Parameter>,
        return_ty: Option<Type>,
        body: AstBlock,
        module: &RefCell<Module>,
    ) -> Function {
        self.scopes.push(hashmap!());

        let mut params = vec![];

        for parameter in parameters {
            let ty = Self::ast_type_to_type(parameter.ty.clone());

            let tmp = self.new_variable(
                &ty,
                lookup!(parameter.ident.name.clone()).into(),
                false,
                false,
            );

            params.push((ty.into_abi(), tmp));
        }

        let mut func = Function {
            linkage: if public || lookup!(ident) == "main" {
                Linkage::public()
            } else {
                Linkage::private()
            },
            name: lookup!(ident).into(),
            external: false,
            parameters: params,
            return_type: return_ty,
            blocks: vec![],
        };

        func.add_block("start");

        let func_ref = RefCell::new(func.clone());

        // Could be a tail call recursion
        //
        // The compiler is single pass which means that
        // we need to forward-declare the function with an empty body
        //
        // TODO: Forward declare *all* functions without their bodies
        module.borrow_mut().add_function(func.clone());

        for item in body.items.iter() {
            match item {
                ast::BlockItem::Out {
                    format_str,
                    arguments,
                    ..
                } => {
                    self.generate_out(&func_ref, module, format_str.clone(), arguments.clone());
                }
                ast::BlockItem::Let(item) => {
                    self.generate_let(&func_ref, module, item.clone());
                }
                ast::BlockItem::Expr {
                    semicolon, expr, ..
                } => {
                    self.generate_expr(&func_ref, module, expr.clone(), &None, None, !semicolon);
                }
                ast::BlockItem::Break { .. } => {
                    if let Some(label) = &self.loop_labels.last() {
                        func_ref
                            .borrow_mut()
                            .add_instruction(Instruction::Jmp(format!("{}.end", label).into()));
                    } else {
                        panic!("Break can only be used in a loop.");
                    };
                }
                ast::BlockItem::Continue { .. } => {
                    if let Some(label) = &self.loop_labels.last() {
                        func_ref
                            .borrow_mut()
                            .add_instruction(Instruction::Jmp(format!("{}.step", label).into()));
                    } else {
                        panic!("Continue can only be used in a loop.");
                    };
                }
            };
        }

        let mut first_ty: Option<Type> = None;

        macro_rules! ty_err_message {
            ($first:expr, $second:expr, $extra:expr $(,)?) => {{
                format!(
                    "Inconsistent return types in function '{}': {} and {}.{}",
                    if is_generic!(func.name) {
                        let mut parts = func.name.split(".").map(|x| x.to_string()).peekable();
                        let mut name = parts.next().unwrap();

                        if let Some(next) = parts.peek() {
                            if next != "0" {
                                name.push_str(&format!("::{}", parts.next().unwrap()));
                            }
                        }

                        name.to_string()
                    } else {
                        func.name.to_string()
                    }
                    .replace(".", "::"),
                    $first,
                    $second,
                    if $extra.is_some() {
                        format!("\n{}", $extra.unwrap())
                    } else {
                        "".into()
                    }
                )
            }};
        }

        macro_rules! maybe_void_pointer {
            ($first:expr, $second:expr $(,)?) => {
                $first.is_pointer()
                    && $second.is_pointer()
                    && ($first.get_pointer_inner().unwrap().is_void()
                        || $second.get_pointer_inner().unwrap().is_void())
            };
        }

        macro_rules! maybe_generic {
            ($first:expr, $second:expr $(,)?) => {
                $first.is_struct()
                    && $second.is_struct()
                    && is_generic!($first.get_struct_inner().unwrap())
                    && is_generic!($second.get_struct_inner().unwrap())
            };
        }

        macro_rules! handle_inconsistent_types {
            ($return_type:expr, $first_type:expr $(,)?) => {
                if $return_type != $first_type && !(maybe_void_pointer!($return_type, $first_type)) {
                    if maybe_generic!($return_type, $first_type) {
                        let (a, a_parts) =
                            Type::from_internal_id($return_type.get_struct_inner().unwrap());

                        let (b, b_parts) =
                            Type::from_internal_id($first_type.get_struct_inner().unwrap());

                        if a != b || a_parts != b_parts {
                            panic!(
                                "{}",
                                ty_err_message!(
                                    $return_type.display(),
                                    $first_type.display(),
                                    Some(
                                        format!("This function's return type is {} but this statement returns {}",
                                            $return_type.display(), $first_type.display()
                                        )
                                    )
                                )
                            )
                        }
                    } else {
                        panic!(
                            "{}",
                            ty_err_message!(
                                $return_type.display(),
                                $first_type.display(),
                                Some(
                                    format!("This error was caused because the return type is {} but this statement returns {}",
                                        $return_type.display(), $first_type.display()
                                    )
                                )
                            )
                        )
                    }
                }
            };
        }

        for block in func_ref.borrow().blocks.iter() {
            for statement in block.statements.clone() {
                if let Statement::Volatile(Instruction::Ret(Some((ty, val)))) = statement {
                    if first_ty.is_none() {
                        first_ty = Some(ty.clone());

                        if let Some(real_return_type) = func_ref.borrow().return_type.clone() {
                            println!("{:?} {:?} {:?}", real_return_type, ty, val);
                            handle_inconsistent_types!(real_return_type, ty)
                        }
                    } else {
                        let return_type = ty.clone();
                        let first_type = first_ty.clone().unwrap();

                        if let Some(real_return_type) = func_ref.borrow().return_type.clone() {
                            handle_inconsistent_types!(real_return_type, return_type)
                        }

                        if return_type != first_type
                            && !matches!(val, Value::Const(_, _))
                            && !(maybe_void_pointer!(return_type, first_type))
                        {
                            panic!(
                                    "{}",
                                    ty_err_message!(
                                        ty.display(),
                                        first_ty.unwrap().display(),
                                        Some(format!(
                                            "This error was caused because you returned {} elsewhere, but not here.",
                                            first_type.display(),
                                        ))
                                    )
                                )
                        }
                    }
                }
            }
        }

        if first_ty.is_some() {
            let return_ty = func_ref.borrow().return_type.clone();

            if return_ty.is_none() {
                func_ref.borrow_mut().return_type = first_ty;
            } else {
                let return_type = return_ty.clone().unwrap();
                let first_type = first_ty.clone().unwrap();

                handle_inconsistent_types!(return_type, first_type)
            }
        }

        if !func_ref.borrow_mut().returns() {
            func_ref
                .borrow_mut()
                .add_instruction(Instruction::Ret(Some((
                    Type::Word,
                    Value::Const(Prefix::None, "0".into()),
                ))));
        }

        self.scopes.pop();

        let mut owned_func = func_ref.borrow_mut().to_owned();
        if owned_func.return_type.is_none() {
            owned_func.return_type = Some(Type::Word)
        }

        let name: ImutStr = lookup!(ident).into();

        module
            .borrow_mut()
            .functions
            .retain(|func| *func.name != *name);

        owned_func
    }

    fn generate_out(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        format_str: Expr,
        arguments: Vec<Expr>,
    ) {
        let (format_ty, format_str) = self
            .generate_expr(func, module, format_str, &None, None, false)
            .expect("Expected a format string");

        let mut arguments: Vec<_> = arguments
            .iter()
            .map(|arg| {
                self.generate_expr(func, module, arg.clone(), &None, None, false)
                    .expect("Failed to generate expr")
            })
            .collect();

        let mut args: Vec<(Type, Value)> = vec![(format_ty, format_str)];
        args.append(&mut arguments);

        func.borrow_mut()
            .add_instruction(Instruction::Call(Value::Global("printf".into()), args));
    }

    fn generate_let(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        item: LetBinding,
    ) -> Option<(Type, Value)> {
        let name: ImutStr = lookup!(item.ident.name).into();
        let mut local_ty = Self::ast_type_to_type(item.ty);
        let mut temp = Some(self.new_variable(&local_ty, name.clone(), false, false));

        let parsed = self.generate_expr(
            func,
            module,
            item.init.unwrap_or(Expr::Literal {
                node: Node::default(),
                value: LiteralValue::Float(0f64),
            }),
            &Some(local_ty.clone()),
            temp.clone(),
            false,
        );

        if let Some((ret_ty, value)) = parsed {
            if ret_ty.is_function()
                && local_ty.get_pointer_inner().is_some_and(|ptr| {
                    ptr.get_unknown_inner()
                        .is_some_and(|inner| inner == "fn".into())
                })
            {
                local_ty = ret_ty.clone();
                temp = Some(self.new_variable(&local_ty, name.clone(), false, false))
            }

            let (final_ty, final_val) = if ret_ty != local_ty {
                self.convert_to_type(func, ret_ty, local_ty.clone(), value.clone(), false)
            } else {
                (local_ty.clone(), value.clone())
            };

            let addr_val =
                self.new_variable(&local_ty, format!("{}.addr", name).into(), true, false);

            func.borrow_mut().assign_instruction_front(
                &addr_val,
                &Type::Pointer(Box::new(final_ty.clone())),
                Instruction::Alloc4(Value::Const(
                    Prefix::None,
                    if final_ty.is_struct() {
                        Type::Pointer(Box::new(Type::Void))
                    } else {
                        final_ty.clone()
                    }
                    .size(module)
                    .to_string()
                    .into(),
                )),
            );

            func.borrow_mut().add_instruction(Instruction::Store(
                final_ty.clone(),
                addr_val.clone(),
                final_val.clone(),
            ));

            if final_ty.is_pointer() {
                func.borrow_mut().add_instruction(Instruction::Call(
                    Value::Global(GC_NOOP.into()),
                    vec![(final_ty.clone(), addr_val.clone())],
                ));
            }

            self.address_pool
                .insert(temp.clone().unwrap().to_string(), addr_val.clone());

            return Some((final_ty, final_val));
        }

        todo!()
    }

    fn generate_expr(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        stmt: Expr,
        ty: &Option<Type>,
        value: Option<Value>,
        is_return: bool,
    ) -> Option<(Type, Value)> {
        let res = match stmt {
            Expr::PostfixUnary {
                operand, operator, ..
            } => {
                let (operand_ty, operand_val) = self
                    .generate_expr(func, module, *operand, ty, None, is_return)
                    .expect("Unexpected error when trying to parse operand of an postfix unary operation");

                let temp = self.new_temporary(Some("post_unary"), true);

                match operator {
                    PostfixOp::Increment => {
                        func.borrow_mut().assign_instruction(
                            &temp,
                            &operand_ty,
                            Instruction::Add(
                                operand_val.clone(),
                                Value::Const(
                                    if operand_ty.clone() == Type::Double {
                                        Prefix::Double
                                    } else if operand_ty.clone() == Type::Single {
                                        Prefix::Single
                                    } else {
                                        Prefix::None
                                    },
                                    "1".into(),
                                ),
                            ),
                        );

                        if let Some(addr) = self.address_pool.get(&operand_val.to_string()) {
                            func.borrow_mut().add_instruction(Instruction::Store(
                                operand_ty.clone(),
                                addr.clone(),
                                temp.clone(),
                            ));
                        } else {
                            func.borrow_mut().assign_instruction(
                                &operand_val,
                                &operand_ty,
                                Instruction::Copy(temp.clone()),
                            );
                        }
                    }
                    PostfixOp::Decrement => {
                        func.borrow_mut().assign_instruction(
                            &temp,
                            &operand_ty,
                            Instruction::Sub(
                                operand_val.clone(),
                                Value::Const(
                                    if operand_ty.clone() == Type::Double {
                                        Prefix::Double
                                    } else if operand_ty.clone() == Type::Single {
                                        Prefix::Single
                                    } else {
                                        Prefix::None
                                    },
                                    "1".into(),
                                ),
                            ),
                        );

                        if let Some(addr) = self.address_pool.get(&operand_val.to_string()) {
                            func.borrow_mut().add_instruction(Instruction::Store(
                                operand_ty.clone(),
                                addr.clone(),
                                temp.clone(),
                            ));
                        } else {
                            func.borrow_mut().assign_instruction(
                                &operand_val,
                                &operand_ty,
                                Instruction::Copy(temp.clone()),
                            );
                        }
                    }
                };

                Some((operand_ty, temp))
            }

            Expr::Assign {
                target,
                operator,
                expr,
                ..
            } => {
                let name = self.convert_assign_target_to_name(target);
                let (local_ty, local_val) = match self.get_variable(&name, Some(module)) {
                    Ok((ty, val)) => (ty, val),
                    Err(_) => panic!("Undeclared variable"),
                };

                if self.get_variable(&name, Some(module)).is_err() {
                    panic!("Variable named '{}' hasn't been declared yet.", name);
                }

                if let Some(address) = self.address_pool.get(&local_val.to_string()) {
                    func.borrow_mut().assign_instruction(
                        &Value::Temp("_".into()),
                        &local_ty.clone(),
                        Instruction::Load(local_ty.clone(), address.clone()),
                    );
                }

                let temp = self.new_temporary(None, false);
                let parsed =
                    self.generate_expr(func, module, *expr, &Some(local_ty.clone()), None, false);

                if let Some((_, value)) = parsed {
                    let temp2 = self.new_temporary(None, false);

                    if let Some(address) = self.address_pool.get(&local_val.to_string()) {
                        if operator != AssignOp::Assign {
                            func.borrow_mut().assign_instruction(
                                &temp2,
                                &local_ty.clone(),
                                Instruction::Load(local_ty.clone(), address.clone()),
                            );
                        }

                        match operator {
                            AssignOp::Assign => {
                                func.borrow_mut().add_instruction(Instruction::Store(
                                    local_ty.clone(),
                                    address.clone(),
                                    value.clone(),
                                ))
                            }
                            AssignOp::Add => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Add(temp2, value),
                            ),
                            AssignOp::Or => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Or(temp2, value),
                            ),
                            AssignOp::Sub => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Sub(temp2, value),
                            ),
                            AssignOp::Mul => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Mul(temp2, value),
                            ),
                            AssignOp::Div => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Div(temp2, value),
                            ),
                            AssignOp::Rem => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Rem(temp2, value),
                            ),
                            AssignOp::And => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::And(temp2, value),
                            ),
                            AssignOp::Xor => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Xor(temp2, value),
                            ),
                            AssignOp::Shl => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Shl(temp2, value),
                            ),
                            AssignOp::Shr => func.borrow_mut().assign_instruction(
                                &temp,
                                &local_ty,
                                Instruction::Shr(temp2, value),
                            ),
                        }

                        if operator != AssignOp::Assign {
                            func.borrow_mut().add_instruction(Instruction::Store(
                                local_ty,
                                address.clone(),
                                temp,
                            ));
                        }
                    }
                }

                None
            }
            Expr::For {
                condition,
                block,
                init,
                update,
                ..
            } => {
                self.scopes.push(hashmap!());

                match init {
                    ForInit::Let(item) => self.generate_let(func, module, *item),
                    ForInit::Expr(item) => self.generate_expr(func, module, *item, ty, None, false),
                };

                self.temp_count += 1;

                let cond_block = format!("loop.{}.cond", self.temp_count);
                let step_block = format!("loop.{}.step", self.temp_count);
                let body_block = format!("loop.{}.body", self.temp_count);
                let end_block = format!("loop.{}.end", self.temp_count);

                self.loop_labels
                    .push(format!("loop.{}", self.temp_count).into());

                func.borrow_mut().add_block(&cond_block);

                let (_, val) = self.generate_expr(func, module, *condition, ty, None, false).expect("Unexpected error when trying to compile the condition of an for expression.");

                func.borrow_mut().add_instruction(Instruction::Jnz(
                    val,
                    body_block.clone().into(),
                    end_block.clone().into(),
                ));

                func.borrow_mut().add_block(&step_block);

                self.generate_expr(func, module, *update, ty, None, false);

                func.borrow_mut()
                    .add_instruction(Instruction::Jmp(cond_block.into()));

                func.borrow_mut().add_block(&body_block);

                for item in block.items.iter() {
                    match item {
                        ast::BlockItem::Out {
                            format_str,
                            arguments,
                            ..
                        } => {
                            self.generate_out(func, module, format_str.clone(), arguments.clone());
                        }
                        ast::BlockItem::Let(item) => {
                            self.generate_let(func, module, item.clone());
                        }
                        ast::BlockItem::Expr {
                            semicolon, expr, ..
                        } => {
                            self.generate_expr(func, module, expr.clone(), &None, None, !semicolon);
                        }

                        ast::BlockItem::Break { .. } => {
                            if let Some(label) = &self.loop_labels.last() {
                                func.borrow_mut().add_instruction(Instruction::Jmp(
                                    format!("{}.end", label).into(),
                                ));
                            } else {
                                panic!("Break can only be used in a loop.");
                            };
                        }
                        ast::BlockItem::Continue { .. } => {
                            if let Some(label) = &self.loop_labels.last() {
                                func.borrow_mut().add_instruction(Instruction::Jmp(
                                    format!("{}.step", label).into(),
                                ));
                            } else {
                                panic!("Continue can only be used in a loop.");
                            };
                        }
                    };
                }

                if !func.borrow_mut().blocks.last().is_some_and(|b| b.jumps()) {
                    func.borrow_mut()
                        .add_instruction(Instruction::Jmp(step_block.into()));
                }

                func.borrow_mut().add_block(&end_block);
                self.loop_labels.pop();
                self.scopes.pop();

                None
            }
            Expr::While {
                condition, block, ..
            } => {
                self.scopes.push(hashmap!());
                self.temp_count += 1;

                let cond_block = format!("loop.{}.cond", self.temp_count);
                let step_block = format!("loop.{}.step", self.temp_count);
                let body_block = format!("loop.{}.body", self.temp_count);
                let end_block = format!("loop.{}.end", self.temp_count);

                self.loop_labels
                    .push(format!("loop.{}", self.temp_count).into());

                func.borrow_mut().add_block(&cond_block);

                let (_, val) = self.generate_expr(func, module, *condition, ty, None, false).expect("Unexpected error when trying to compile the condition of an while expression.");

                func.borrow_mut().add_instruction(Instruction::Jnz(
                    val,
                    body_block.clone().into(),
                    end_block.clone().into(),
                ));

                func.borrow_mut().add_block(&step_block);

                func.borrow_mut()
                    .add_instruction(Instruction::Jmp(cond_block.into()));

                func.borrow_mut().add_block(&body_block);

                for item in block.items.iter() {
                    match item {
                        ast::BlockItem::Out {
                            format_str,
                            arguments,
                            ..
                        } => {
                            self.generate_out(func, module, format_str.clone(), arguments.clone());
                        }
                        ast::BlockItem::Let(item) => {
                            self.generate_let(func, module, item.clone());
                        }
                        ast::BlockItem::Expr {
                            semicolon, expr, ..
                        } => {
                            self.generate_expr(func, module, expr.clone(), &None, None, !semicolon);
                        }

                        ast::BlockItem::Break { .. } => {
                            if let Some(label) = &self.loop_labels.last() {
                                func.borrow_mut().add_instruction(Instruction::Jmp(
                                    format!("{}.end", label).into(),
                                ));
                            } else {
                                panic!("Break can only be used in a loop.");
                            };
                        }
                        ast::BlockItem::Continue { .. } => {
                            if let Some(label) = &self.loop_labels.last() {
                                func.borrow_mut().add_instruction(Instruction::Jmp(
                                    format!("{}.step", label).into(),
                                ));
                            } else {
                                panic!("Continue can only be used in a loop.");
                            };
                        }
                    };
                }

                if !func.borrow_mut().blocks.last().is_some_and(|b| b.jumps()) {
                    func.borrow_mut()
                        .add_instruction(Instruction::Jmp(step_block.into()));
                }

                func.borrow_mut().add_block(&end_block);
                self.loop_labels.pop();
                self.scopes.pop();

                None
            }
            Expr::If(expr) => {
                self.scopes.push(hashmap!());

                let (_, val) = self.generate_expr(func, module, *expr.condition, ty, None, false).expect("Unexpected error when trying to compile the condition of an if expression.");

                self.temp_count += 1;

                let then_block = format!("then.{}", self.temp_count);
                let else_block = format!("else.{}", self.temp_count);
                let end_block = format!("end.{}", self.temp_count);

                if expr.alternate.is_some() {
                    func.borrow_mut().add_instruction(Instruction::Jnz(
                        val,
                        then_block.clone().into(),
                        else_block.clone().into(),
                    ));
                } else {
                    func.borrow_mut().add_instruction(Instruction::Jnz(
                        val,
                        then_block.clone().into(),
                        end_block.clone().into(),
                    ));
                }

                func.borrow_mut().add_block(then_block);

                for item in expr.block.items.iter() {
                    match item {
                        ast::BlockItem::Out {
                            format_str,
                            arguments,
                            ..
                        } => {
                            self.generate_out(func, module, format_str.clone(), arguments.clone());
                        }
                        ast::BlockItem::Let(item) => {
                            self.generate_let(func, module, item.clone());
                        }
                        ast::BlockItem::Expr {
                            semicolon, expr, ..
                        } => {
                            self.generate_expr(func, module, expr.clone(), &None, None, !semicolon);
                        }
                        ast::BlockItem::Break { .. } => {
                            if let Some(label) = &self.loop_labels.last() {
                                func.borrow_mut().add_instruction(Instruction::Jmp(
                                    format!("{}.end", label).into(),
                                ));
                            } else {
                                panic!("Break can only be used in a loop.");
                            };
                        }
                        ast::BlockItem::Continue { .. } => {
                            println!("HI");
                            if let Some(label) = &self.loop_labels.last() {
                                func.borrow_mut().add_instruction(Instruction::Jmp(
                                    format!("{}.step", label).into(),
                                ));
                            } else {
                                panic!("Continue can only be used in a loop.");
                            };
                        }
                    };
                }

                if !func.borrow_mut().blocks.last().is_some_and(|b| b.jumps()) {
                    func.borrow_mut()
                        .add_instruction(Instruction::Jmp(end_block.clone().into()));
                }

                match expr.alternate.clone().map(|f| *f) {
                    Some(IfAlternate::Else(block)) => {
                        func.borrow_mut().add_block(else_block);

                        for item in block.items.iter() {
                            match item {
                                ast::BlockItem::Out {
                                    format_str,
                                    arguments,
                                    ..
                                } => {
                                    self.generate_out(
                                        func,
                                        module,
                                        format_str.clone(),
                                        arguments.clone(),
                                    );
                                }
                                ast::BlockItem::Let(item) => {
                                    self.generate_let(func, module, item.clone());
                                }
                                ast::BlockItem::Expr {
                                    semicolon, expr, ..
                                } => {
                                    self.generate_expr(
                                        func,
                                        module,
                                        expr.clone(),
                                        &None,
                                        None,
                                        !semicolon,
                                    );
                                }
                                ast::BlockItem::Break { .. } => {
                                    if let Some(label) = &self.loop_labels.last() {
                                        func.borrow_mut().add_instruction(Instruction::Jmp(
                                            format!("{}.end", label).into(),
                                        ));
                                    } else {
                                        panic!("Break can only be used in a loop.");
                                    };
                                }
                                ast::BlockItem::Continue { .. } => {
                                    if let Some(label) = &self.loop_labels.last() {
                                        func.borrow_mut().add_instruction(Instruction::Jmp(
                                            format!("{}.step", label).into(),
                                        ));
                                    } else {
                                        panic!("Continue can only be used in a loop.");
                                    };
                                }
                            };
                        }

                        func.borrow_mut().add_block(end_block);
                    }
                    Some(IfAlternate::If(if_expr)) => {
                        func.borrow_mut().add_block(else_block);
                        self.generate_expr(
                            func,
                            module,
                            Expr::If(if_expr),
                            ty,
                            value.clone(),
                            false,
                        );

                        func.borrow_mut().add_block(end_block);
                    }
                    None => {
                        func.borrow_mut().add_block(end_block);
                    }
                };

                self.scopes.pop();
                None
            }
            Expr::Paren { expr, .. } => {
                self.generate_expr(func, module, *expr, ty, None, is_return)
            }
            Expr::PrefixUnary {
                operator, operand, ..
            } => {
                if operator == PrefixOp::Neg {
                    if let Some((operand_ty, operand_val)) =
                        self.generate_expr(func, module, *operand, ty, None, false)
                    {
                        let temp = self.new_temporary(Some("unary"), false);

                        func.borrow_mut().assign_instruction(
                            &temp,
                            &operand_ty,
                            Instruction::Mul(
                                operand_val,
                                Value::Const(
                                    if operand_ty.clone() == Type::Double {
                                        Prefix::Double
                                    } else if operand_ty.clone() == Type::Single {
                                        Prefix::Single
                                    } else {
                                        Prefix::None
                                    },
                                    "-1".into(),
                                ),
                            ),
                        );

                        if is_return {
                            func.borrow_mut()
                                .add_instruction(Instruction::Ret(Some((operand_ty, temp))));

                            return None;
                        } else {
                            return Some((operand_ty, temp));
                        }
                    };

                    panic!("AHo")
                }
                let (operand_ty, operand_val) = self
                    .generate_expr(func, module, *operand, ty, None, false)
                    .expect("Unexpected error when trying to parse operand of an unary operation");

                let temp = self.new_temporary(Some("unary"), true);

                match operator {
                    PrefixOp::Not => func.borrow_mut().assign_instruction(
                        &temp,
                        &operand_ty,
                        if operand_ty.is_float() {
                            Instruction::Neg(operand_val)
                        } else {
                            Instruction::Not(operand_val)
                        },
                    ),
                    PrefixOp::Neg => func.borrow_mut().assign_instruction(
                        &temp,
                        &operand_ty,
                        Instruction::Neg(operand_val),
                    ),
                    PrefixOp::LogNot => {
                        func.borrow_mut().assign_instruction(
                            &temp,
                            &Type::Boolean,
                            Instruction::Cmp(
                                Type::Boolean,
                                Cmp::Eq,
                                operand_val,
                                Value::Const(
                                    if operand_ty.clone() == Type::Double {
                                        Prefix::Double
                                    } else if operand_ty.clone() == Type::Single {
                                        Prefix::Single
                                    } else {
                                        Prefix::None
                                    },
                                    "0".into(),
                                ),
                            ),
                        );

                        return Some((Type::Boolean, temp));
                    }
                    PrefixOp::Sizeof => {
                        let size_of = operand_ty.size(module);
                        func.borrow_mut().assign_instruction(
                            &temp,
                            &Type::Long,
                            Instruction::Copy(Value::Const(
                                Prefix::None,
                                size_of.to_string().into(),
                            )),
                        );

                        return Some((Type::Long, temp));
                    }
                    PrefixOp::Increment => {
                        func.borrow_mut().assign_instruction(
                            &temp,
                            &operand_ty,
                            Instruction::Add(
                                operand_val.clone(),
                                Value::Const(
                                    if operand_ty.clone() == Type::Double {
                                        Prefix::Double
                                    } else if operand_ty.clone() == Type::Single {
                                        Prefix::Single
                                    } else {
                                        Prefix::None
                                    },
                                    "1".into(),
                                ),
                            ),
                        );

                        func.borrow_mut().assign_instruction(
                            &operand_val,
                            &operand_ty,
                            Instruction::Copy(temp.clone()),
                        );
                    }
                    PrefixOp::Decrement => {
                        func.borrow_mut().assign_instruction(
                            &temp,
                            &operand_ty,
                            Instruction::Sub(
                                operand_val.clone(),
                                Value::Const(
                                    if operand_ty.clone() == Type::Double {
                                        Prefix::Double
                                    } else if operand_ty.clone() == Type::Single {
                                        Prefix::Single
                                    } else {
                                        Prefix::None
                                    },
                                    "1".into(),
                                ),
                            ),
                        );

                        func.borrow_mut().assign_instruction(
                            &operand_val,
                            &operand_ty,
                            Instruction::Copy(temp.clone()),
                        );
                    }
                    _ => todo!(),
                };

                Some((operand_ty, temp))
            }

            Expr::Return { init, .. } => {
                match self.generate_expr(func, module, *init, ty, None, true) {
                    Some((ret_ty, value)) => {
                        func.borrow_mut()
                            .add_instruction(Instruction::Ret(Some((ret_ty, value))));
                    }
                    None => {
                        func.borrow_mut().add_instruction(Instruction::Ret(None));
                    }
                }

                None
            }
            Expr::Cast { ty, expr, .. } => {
                let value = self.generate_expr(func, module, *expr, &None, None, false);

                if let Some((val_ty, val_val)) = value {
                    let dodo = self.convert_to_type(
                        func,
                        val_ty,
                        Self::ast_type_to_type(ty),
                        val_val,
                        true,
                    );

                    return Some((dodo.0, dodo.1));
                }

                return value;
            }
            Expr::Literal { value, .. } => match value {
                LiteralValue::String(val) => {
                    self.temp_count += 1;

                    let name =
                        self.tmp_name_with_debug_assertions(&func.borrow_mut().name.clone(), true);

                    self.data_sections.push(Data::new(
                        Linkage::private(),
                        name.clone().into(),
                        None,
                        vec![
                            (Type::Byte, DataItem::String(lookup!(val).into())),
                            (Type::Byte, DataItem::Const(0f64)),
                        ],
                    ));

                    Some((
                        Type::Pointer(Box::new(Type::Char)),
                        Value::Global(name.into()),
                    ))
                }
                kind => {
                    let num_ty = match kind {
                        LiteralValue::Float(_) => Type::Double,
                        LiteralValue::Boolean(_) => Type::Boolean,
                        LiteralValue::Integer(_) => Type::Long,
                        _ => Type::Word,
                    };

                    let mut final_ty = if ty.clone().is_some_and(|ty| !ty.is_string()) {
                        ty.clone().unwrap_or(num_ty.clone())
                    } else {
                        num_ty
                    };

                    if is_return {
                        final_ty = func.borrow_mut().return_type.clone().unwrap_or(final_ty);
                    }

                    Some((
                        final_ty.clone(),
                        Value::Const(
                            if final_ty.clone() == Type::Double {
                                Prefix::Double
                            } else if final_ty.clone() == Type::Single {
                                Prefix::Single
                            } else {
                                Prefix::None
                            },
                            match kind {
                                LiteralValue::Float(val) => val.to_string().into(),
                                LiteralValue::Integer(val) => val.to_string().into(),
                                LiteralValue::Boolean(val) => {
                                    if val {
                                        "1".into()
                                    } else {
                                        "0".into()
                                    }
                                }
                                _ => unreachable!(),
                            },
                        ),
                    ))
                }
            },
            Expr::Identifier(ident) => {
                self.get_variable_lazy(&lookup!(ident.name).into(), Some(func), Some(module))
            }
            Expr::Break { .. } => {
                if let Some(label) = &self.loop_labels.last() {
                    func.borrow_mut()
                        .add_instruction(Instruction::Jmp(format!("{}.end", label).into()));
                } else {
                    panic!("Break can only be used in a loop.")
                }

                None
            }
            Expr::Continue { .. } => {
                if let Some(label) = &self.loop_labels.last() {
                    func.borrow_mut()
                        .add_instruction(Instruction::Jmp(format!("{}.step", label).into()));
                } else {
                    panic!("Continue can only be used in a loop.")
                }

                None
            }
            Expr::Binary {
                left,
                operator,
                right,
                ..
            } => {
                let ctx = Context { module, func };

                if matches!(operator, BinaryOp::LogAnd | BinaryOp::LogOr) {
                    return Some(self.handle_short_circuiting_operation(
                        *left,
                        *right,
                        &ctx,
                        ty.clone(),
                        is_return,
                        operator,
                    ));
                }

                let (mut left_ty, left_val_unparsed) = self.generate_expr(func, module, *left.clone(), ty, None, false).expect("Unexpected error when trying to parse left side of an arithmetic operation");
                let (mut right_ty, right_val_unparsed) = self.generate_expr(func, module, *right.clone(), ty, None, false).expect("Unexpected error when trying to parse right side of an arithmetic operation");

                let mut left_val = left_val_unparsed.clone();
                let mut right_val = right_val_unparsed.clone();

                if left_ty.is_string() && right_ty == Type::Char {
                    let char_tmp = self.new_temporary(None, true);

                    func.borrow_mut().assign_instruction(
                        &char_tmp,
                        &Type::Char,
                        Instruction::Load(Type::Char, left_val),
                    );

                    left_ty = Type::Char;
                    left_val = char_tmp;
                }

                if right_ty.is_string() && left_ty == Type::Char {
                    let char_tmp = self.new_temporary(None, true);

                    func.borrow_mut().assign_instruction(
                        &char_tmp,
                        &Type::Char,
                        Instruction::Load(Type::Char, right_val),
                    );

                    right_ty = Type::Char;
                    right_val = char_tmp;
                }

                match left_ty.weight().cmp(&right_ty.weight()) {
                    Ordering::Greater => {
                        let (_, val) = self.convert_to_type(
                            func,
                            right_ty.clone(),
                            left_ty.clone(),
                            right_val_unparsed,
                            false,
                        );

                        right_val = val;
                    }
                    Ordering::Less => {
                        let (ty, val) = self.convert_to_type(
                            func,
                            left_ty.clone(),
                            right_ty.clone(),
                            left_val_unparsed,
                            false,
                        );
                        left_ty = ty;
                        left_val = val;
                    }
                    _ => {}
                }

                if [
                    BinaryOp::Xor,
                    BinaryOp::Or,
                    BinaryOp::And,
                    BinaryOp::Shl,
                    BinaryOp::Shr,
                ]
                .contains(&operator)
                    && (left_ty.is_float() || right_ty.is_float())
                {
                    panic!(
                        "Cannot use the '{:?}' operator on non-integer type '{}'.\nYou can cast it to an itneger if you need this functionality.",
                        operator,
                        if left_ty.is_float() {
                            left_ty.display()
                        } else {
                            right_ty.display()
                        }
                    )
                }

                let instruction_ty = left_ty;
                let cloned_ty = instruction_ty.clone();

                let res = match operator.clone() {
                    BinaryOp::Add => Instruction::Add(left_val, right_val),
                    BinaryOp::Sub => Instruction::Sub(left_val, right_val),
                    BinaryOp::Mul => Instruction::Mul(left_val, right_val),
                    BinaryOp::Div => Instruction::Div(left_val, right_val),
                    BinaryOp::Rem => Instruction::Rem(left_val, right_val),
                    BinaryOp::Gt => Instruction::Cmp(cloned_ty, Cmp::Gt, left_val, right_val),
                    BinaryOp::Gte => Instruction::Cmp(cloned_ty, Cmp::Gte, left_val, right_val),
                    BinaryOp::Lt => Instruction::Cmp(cloned_ty, Cmp::Lt, left_val, right_val),
                    BinaryOp::Lte => Instruction::Cmp(cloned_ty, Cmp::Lte, left_val, right_val),
                    BinaryOp::Equal => Instruction::Cmp(cloned_ty, Cmp::Eq, left_val, right_val),
                    BinaryOp::NotEqual => {
                        Instruction::Cmp(cloned_ty, Cmp::Neq, left_val, right_val)
                    }
                    BinaryOp::And => Instruction::And(left_val, right_val),
                    BinaryOp::Or => Instruction::Or(left_val, right_val),
                    BinaryOp::Xor => Instruction::Xor(left_val, right_val),
                    BinaryOp::Shl => Instruction::Shl(left_val, right_val),
                    BinaryOp::Shr => Instruction::Shr(left_val, right_val),
                    _ => panic!("Invalid operator token: {:?}", operator),
                };

                let op_temp = self.new_temporary(None, true);
                let final_ty = if [
                    BinaryOp::Lt,
                    BinaryOp::Lte,
                    BinaryOp::Gt,
                    BinaryOp::Gte,
                    BinaryOp::Equal,
                    BinaryOp::NotEqual,
                    BinaryOp::LogAnd,
                    BinaryOp::LogOr,
                ]
                .contains(&operator)
                {
                    Type::Boolean
                } else {
                    instruction_ty
                };

                func.borrow_mut()
                    .assign_instruction(&op_temp, &final_ty, res);

                Some((final_ty, op_temp))
            }

            Expr::Call { callee, args, .. } => {
                let name = self.convert_assign_target_to_name(callee);
                let declarative_type = ty.clone().unwrap_or(Type::Void);

                let tmp_func = &module.borrow().functions;

                let tmp_func = tmp_func.iter().find(|func| *func.name == *name);

                if let Some(tmp_func) = tmp_func {
                    let ty = tmp_func.return_type.clone().unwrap_or(declarative_type);
                    let temp = self.new_temporary(None, true);
                    let val = self
                        .get_variable(&name, Some(module))
                        .unwrap_or((Type::Long, Value::Global(name.clone())))
                        .1;

                    let args: Vec<_> = args.iter().map(|arg| {
                        self.generate_expr(func, module, arg.clone(), &None, None, false).unwrap_or_else(|| panic!("Unexpected error when trying to generate a statement for a argument in a function called '{}'", name))
                    }).collect();

                    func.borrow_mut()
                        .assign_instruction(&temp, &ty, Instruction::Call(val, args));

                    Some((ty, temp))
                } else {
                    panic!("Undefined function '{}'", name)
                }
            }
            _ => None,
        };

        if is_return {
            func.borrow_mut()
                .add_instruction(Instruction::Ret(res.clone()));
        }

        res
    }

    fn convert_assign_target_to_name(&self, target: AssignTarget) -> ImutStr {
        match target {
            AssignTarget::Member(member) => member
                .segments
                .iter()
                .map(|segment| lookup!(segment.name).to_string())
                .collect::<Vec<String>>()
                .join(".")
                .into(),
            AssignTarget::Identifier(ident) => lookup!(ident.name).into(),
        }
    }

    fn convert_to_type(
        &mut self,
        func: &RefCell<Function>,
        first: Type,
        second: Type,
        val: Value,
        explicit: bool,
    ) -> (Type, Value) {
        if first.is_struct() || second.is_struct() {
            if first == second {
                return (second, val);
            }

            if explicit
                && ((first.is_struct() && second.is_pointer_like())
                    || (second.is_struct() && first.is_pointer_like()))
            {
                return (second, val);
            }

            if first.is_pointer() && first.get_pointer_inner().unwrap() == second {
                if second.is_struct() {
                    return (second, val);
                } else {
                    let tmp = self.new_temporary(Some("load"), false);

                    func.borrow_mut().assign_instruction(
                        &tmp,
                        &second.clone(),
                        Instruction::Load(second.clone(), val),
                    );

                    return (second, tmp);
                }
            }

            panic!(
                "Cannot convert from the type '{}' to the type '{}'.",
                first.display(),
                second.display()
            )
        }

        if ((first.is_strictly_number() && second.is_string())
            || (second.is_strictly_number() && first.is_string()))
            && !explicit
        {
            panic!(
                "Cannot implicitly convert '{}' to '{}' or vice versa.\nTo explicitely convert, use the C-like '(type)variable' syntax.\n\nThis has the type '{}'",
                first.display(),
                second.display(),
                first.display()
            )
        }

        if first.is_pointer()
            && second.is_pointer()
            && (first.get_pointer_inner().unwrap().is_void()
                || second.get_pointer_inner().unwrap().is_void())
        {
            return (second, val);
        }

        if first.weight() == second.weight() {
            (second, val)
        } else if (first.is_int() && second.is_int()) || (first.is_float() && second.is_float()) {
            let conv = self.new_temporary(Some("conv"), true);
            let is_first_higher = first.weight() > second.weight();

            func.borrow_mut().assign_instruction(
                &conv,
                &second,
                if is_first_higher {
                    if first.is_float() {
                        Instruction::Tnc(val)
                    } else {
                        Instruction::Copy(val)
                    }
                } else {
                    println!("{:?} {:?}", first, val);
                    Instruction::Ext(first, val)
                },
            );

            return (second, conv);
        } else {
            let conv = self.new_temporary(Some("conv"), true);

            func.borrow_mut().assign_instruction(
                &conv,
                &second,
                Instruction::Cnv(first, second.clone(), val),
            );

            return (second, conv);
        }
    }

    fn handle_short_circuiting_operation(
        &mut self,
        left: Expr,
        right: Expr,
        ctx: &Context,
        ty: Option<Type>,
        is_return: bool,
        operator: BinaryOp,
    ) -> (Type, Value) {
        self.temp_count += 1;

        let left_label = format!("{}.left.{}", operator, self.temp_count);
        let right_label = format!("{}.right.{}", operator, self.temp_count);
        let left_matches_label = format!("{}.left.match.{}", operator, self.temp_count);
        let right_matches_label = format!("{}.right.match.{}", operator, self.temp_count);
        let end_label = format!("{}.end.{}", operator, self.temp_count);

        let result_tmp = self.new_temporary(Some(&operator.to_string()), true);

        let (func, module) = (ctx.func, ctx.module);

        let (left_ty, left_val) = self
            .generate_expr(func, module, left, &ty.clone(), None, is_return)
            .expect("Unexpected error when trying to parse left side of an arithmetic operation");

        func.borrow_mut().assign_instruction(
            &result_tmp,
            &left_ty,
            Instruction::Copy(Value::Const(
                if left_ty.clone() == Type::Double {
                    Prefix::Double
                } else if left_ty.clone() == Type::Single {
                    Prefix::Single
                } else {
                    Prefix::None
                },
                "0".into(),
            )),
        );

        func.borrow_mut().add_block(left_label);

        let left_tmp = self.new_temporary(Some(&format!("{}.left", operator)), true);

        func.borrow_mut().assign_instruction(
            &left_tmp,
            &Type::Boolean,
            Instruction::Cmp(
                Type::Boolean,
                Cmp::Eq,
                left_val.clone(),
                Value::Const(Prefix::None, "0".into()),
            ),
        );

        match operator {
            BinaryOp::LogAnd => {
                func.borrow_mut().add_instruction(Instruction::Jnz(
                    left_tmp,
                    end_label.clone().into(),
                    right_label.clone().into(),
                ));
            }
            BinaryOp::LogOr => {
                func.borrow_mut().add_instruction(Instruction::Jnz(
                    left_tmp,
                    right_label.clone().into(),
                    left_matches_label.clone().into(),
                ));
            }
            other => panic!(
                "Invalid operator token for conditional short circuiting '{}'",
                other
            ),
        }

        func.borrow_mut().add_block(right_label);

        let (_, right_val) = self
            .generate_expr(func, module, right, &ty.clone(), None, is_return)
            .expect("Unexpected error when trying to parse right side of an arithmetic operation");

        let right_tmp = self.new_temporary(Some(&format!("{}.right", operator)), true);

        func.borrow_mut().assign_instruction(
            &right_tmp,
            &Type::Boolean,
            Instruction::Cmp(
                Type::Boolean,
                Cmp::Eq,
                right_val.clone(),
                Value::Const(Prefix::None, "0".into()),
            ),
        );

        func.borrow_mut().add_instruction(Instruction::Jnz(
            right_tmp,
            end_label.clone().into(),
            right_matches_label.clone().into(),
        ));

        func.borrow_mut().add_block(left_matches_label);

        func.borrow_mut()
            .assign_instruction(&result_tmp, &left_ty, Instruction::Copy(left_val));

        func.borrow_mut()
            .add_instruction(Instruction::Jmp(end_label.clone().into()));

        func.borrow_mut().add_block(right_matches_label);

        func.borrow_mut()
            .assign_instruction(&result_tmp, &left_ty, Instruction::Copy(right_val));

        func.borrow_mut()
            .add_instruction(Instruction::Jmp(end_label.clone().into()));

        func.borrow_mut().add_block(end_label);

        (left_ty, result_tmp)
    }
}
