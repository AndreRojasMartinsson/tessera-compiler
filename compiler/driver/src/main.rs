use std::cell::RefCell;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process;
use std::process::exit;
use std::process::Stdio;
use std::rc::Rc;
use std::str;

use args::Command;
use codegen::CodeGen;
use context::Context;
use import_resolver::ImportResolver;
use lexer::Kind;
use lexer::Lexer;
use memmap2::Mmap;
use miette::miette;
use miette::IntoDiagnostic;
use parser::parser::Parser;
use symbols::ImportSymbol;
use tsc_std::get_standard_library;
use type_solver::TypeSolver;

mod args;

const CC_LIBS: &[&str] = &["-lc", "-lm"];

fn compile_file(
    file_name: &Path,
    output_dir: &Path,
    print_ir: bool,
    emit_asm: bool,
    emit_ir: bool,
    run: bool,
) -> miette::Result<()> {
    if !file_name.exists() {
        return Err(miette!("file {file_name:?} does not exist"));
    }

    if !file_name.is_file() {
        return Err(miette!("path {file_name:?} is not a file"));
    }

    let file = fs::File::open(file_name).into_diagnostic()?;

    let mmap = unsafe { Mmap::map(&file).into_diagnostic()? };
    let bytes = &mmap[..];
    let original_source = str::from_utf8(bytes).into_diagnostic()?;

    let (mut source, functions) = get_standard_library();
    source.push_str(&format!("\n\n\n{original_source}"));
    println!("{source}");

    println!("{functions:#?}");

    let name = file_name.file_name().expect("Should have a filename");
    let _context = Context::new(name, file_name);

    let mut lexer = Lexer::new(&source);
    let mut tokens = vec![];

    let basename = file_name.file_stem().unwrap();

    loop {
        let token = lexer.read_next();
        match token {
            Ok(token) if token.kind == Kind::Eof => break,
            Ok(token) => tokens.push(token),
            Err(err) => Err(err)?, // Err(e) => match e {
                                   //     LexerError::InvalidInt(err) => Err(err),
                                   //     LexerError::InvalidFloat(err) => panic!("{}", err),
                                   //     LexerError::UnterminatedString(err) => panic!("{}", err),
                                   //     LexerError::UnexpectedChar(err) => panic!("{}", err),
                                   //     LexerError::UnexpectedEnd(err) => panic!("{}", err),
                                   // }?,
        }
    }

    let mut parser = Parser::new(&mut tokens, &source);
    let ast = parser.parse();
    println!("{:#?}", &ast);

    let mut resolver = ImportResolver::default();
    let _ = resolver.index_project(file_name.to_path_buf());

    let resolver_ref = RefCell::new(resolver);
    // resolver.resolve_program(&ast);

    // let mut solver = TypeSolver::default();
    // solver.solve(&ast);

    fs::create_dir_all(output_dir).expect("Could not create output directory");

    if let Ok(ir) = CodeGen::compile(ast, true, print_ir, &resolver_ref) {
        let asm_path = output_dir.join(format!("{}.s", basename.to_str().unwrap()));

        let ir_path = output_dir.join(format!("{}.ssa", basename.to_str().unwrap()));

        if emit_ir {
            fs::write(ir_path, &ir).unwrap();
        }

        let mut child = process::Command::new("qbe")
            .args(["-o", asm_path.to_str().unwrap()])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to spawn command");

        if let Some(mut stdin) = child.stdin.take() {
            stdin
                .write_all(ir.as_bytes())
                .expect("Could not write to stdin");
        }

        let output = child.wait_with_output().expect("Could not wait");
        if output.status.success() {
            let binary_path = output_dir.join(basename);
            let mut cc_args = vec![
                "-o",
                binary_path.to_str().unwrap(),
                asm_path.to_str().unwrap(),
            ];

            cc_args.append(&mut CC_LIBS.to_vec());

            process::Command::new("cc")
                .args(&cc_args)
                .spawn()
                .expect("Could not generate binary")
                .wait()
                .expect("Could not generate binary");

            if !emit_asm {
                fs::remove_file(output_dir.join(format!("{}.s", basename.to_str().unwrap())))
                    .expect("Could not remove file");
            }

            if run {
                let output = process::Command::new(output_dir.join(basename))
                    .output()
                    .unwrap();

                if output.status.success() {
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                } else {
                    eprintln!("ERROR: {}", String::from_utf8_lossy(&output.stderr));
                }
            }

            Ok(())
        } else {
            eprintln!("Compilation failed during binary generation, with error:");
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            exit(1)
        }
    } else {
        panic!("ERROR: Failed to compile module")
    }
}

fn main() -> miette::Result<()> {
    let command = args::parse_args();

    match command {
        Command::Run {
            file_name,
            output_dir,
            print_ir,
            emit_asm,
            emit_ir,
        } => {
            compile_file(&file_name, &output_dir, print_ir, emit_asm, emit_ir, true)?;
        }
        Command::Build {
            file_name,
            output_dir,
            print_ir,
            emit_asm,
            emit_ir,
        } => {
            compile_file(&file_name, &output_dir, print_ir, emit_asm, emit_ir, false)?;
        }
    };

    Ok(())
}
