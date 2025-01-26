use ast::{ProgramItem, Ty};
use interner::lookup;
use lexer::{Kind, Lexer};
use parser::parser::Parser;

/// Function to resolve a part of the language
/// std for use in the import resolver to inject
/// the std.
///
/// Namespace can for example be:
///   - std
///   - std::mem::copy
///   - etc...
pub fn get_standard_library() -> (String, Vec<(Ty, String, Vec<Ty>)>) {
    let std_source = include_str!(concat!(env!("OUT_DIR"), "/_std.tes"));

    let mut lexer = Lexer::new(std_source);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.read_next();
        match token {
            Ok(token) if token.kind == Kind::Eof => break,
            Ok(token) => tokens.push(token),
            Err(err) => panic!("{err}"), // Err(e) => match e {
                                         //     LexerError::InvalidInt(err) => Err(err),
                                         //     LexerError::InvalidFloat(err) => panic!("{}", err),
                                         //     LexerError::UnterminatedString(err) => panic!("{}", err),
                                         //     LexerError::UnexpectedChar(err) => panic!("{}", err),
                                         //     LexerError::UnexpectedEnd(err) => panic!("{}", err),
                                         // }?,
        }
    }

    let mut parser = Parser::new(&mut tokens, std_source);
    let ast = parser.parse();

    (
        std_source.into(),
        ast.items
            .iter()
            .filter_map(|item| match item {
                ProgramItem::Function {
                    ty,
                    ident,
                    parameters,
                    ..
                } => Some((
                    ty.ty.clone(),
                    lookup!(ident.name),
                    parameters.iter().map(|param| param.ty.ty.clone()).collect(),
                )),
                _ => None,
            })
            .collect(),
    )
}
