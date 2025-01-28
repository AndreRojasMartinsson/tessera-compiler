use std::{
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
};

use ast::{Block, Parameter, Program, ProgramItem};
use gxhash::HashMap;
use interner::lookup;
use ir_builder::Type;
use lexer::{Kind, Lexer};
use parser::parser::Parser;

#[derive(Debug, Clone)]
pub struct ImportedFunction {
    pub name: String,
    pub return_ty: Type,
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

#[derive(Default)]
pub struct ImportResolver {
    pub index: HashMap<String, PathBuf>,
    pub functions: HashMap<PathBuf, Vec<ImportedFunction>>,
}

fn collect_files(root_dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut files = vec![];

    for entry in fs::read_dir(root_dir)? {
        let path = entry?.path();
        if path.is_dir() {
            files.extend(collect_files(&path)?);
        } else if path.is_file() {
            files.push(path);
        }
    }

    Ok(files)
}

impl ImportResolver {
    pub fn index_project(&mut self, root_file: PathBuf) -> Vec<(PathBuf, String)> {
        let root_file_path = root_file.canonicalize().unwrap();
        let root_dir = root_file_path.parent().unwrap();

        let files: Vec<(PathBuf, String)> = collect_files(root_dir)
            .unwrap()
            .into_iter()
            .filter_map(|path| {
                if path.extension() != Some(OsStr::new("tes")) {
                    return None;
                };

                if *path == root_file_path {
                    return None;
                }

                let content = fs::read_to_string(&path).unwrap();
                let mut lexer = Lexer::new(&content);
                let mut tokens = Vec::new();

                loop {
                    let token = lexer.read_next();
                    match token {
                        Ok(token) if token.kind == Kind::Eof => break,
                        Ok(token) => tokens.push(token),
                        Err(err) => panic!("{err}"),
                    }
                }

                let mut parser = Parser::new(&mut tokens, &content);
                let ast = parser.parse();

                if let Some(ProgramItem::Module { tree, .. }) = ast
                    .items
                    .iter()
                    .find(|item| matches!(item, ast::ProgramItem::Module { .. }))
                {
                    let namespace = tree
                        .segments
                        .iter()
                        .map(|segment| lookup!(segment.name).to_string())
                        .collect::<Vec<String>>()
                        .join(".");

                    ast.items
                        .iter()
                        .filter(|item| match item {
                            ProgramItem::Function { public, .. } => *public,

                            _ => false,
                        })
                        .for_each(|item| {
                            let func = match item {
                                ProgramItem::Function {
                                    ty,
                                    ident,
                                    parameters,
                                    body,
                                    ..
                                } => ImportedFunction {
                                    name: format!("{namespace}.{}", lookup!(ident.name)),
                                    return_ty: ty.ty.clone(),
                                    body: body.clone(),
                                    parameters: parameters.to_vec(),
                                },
                                _ => unreachable!(),
                            };

                            if let Some(funcs) = self.functions.get(&path) {
                                let mut funcs = funcs.clone();
                                funcs.push(func);

                                self.functions.insert(path.clone(), funcs.to_vec());
                            } else {
                                self.functions.insert(path.clone(), vec![func]);
                            }
                        });

                    return Some((path, namespace));
                };

                None
            })
            .inspect(|result| {
                self.index.insert(result.1.clone(), result.0.clone());
            })
            .collect();

        files
    }

    pub fn resolve_program(&mut self, node: &Program) {}
}
