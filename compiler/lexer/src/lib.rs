pub use kind::Kind;
pub use lexer::Lexer;
pub use token::Token;

mod kind;
mod lexer;
pub mod operator;
mod token;
pub mod value;

#[cfg(test)]
mod tests {
    use diagnostics::errors::lexer::LexerError;
    use interner::intern;

    use super::*;

    fn assert_token(token: Result<Token, LexerError>, kind: Kind, lexeme: Option<&str>) {
        if let Ok(token) = token {
            assert_eq!(&token.kind, &kind);
            if let Some(lexeme) = lexeme {
                assert_eq!(token.lexeme, intern!(lexeme));
            }
        } else {
            panic!("Token is an Err.")
        }
    }

    #[test]
    fn test_single_char_tokens() {
        let input = "()[]{};,:.";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::LParen, Some("(")),
            (Kind::RParen, Some(")")),
            (Kind::LBracket, Some("[")),
            (Kind::RBracket, Some("]")),
            (Kind::LSquirly, Some("{")),
            (Kind::RSquirly, Some("}")),
            (Kind::Semicolon, Some(";")),
            (Kind::Comma, Some(",")),
            (Kind::Colon, Some(":")),
            (Kind::Period, Some(".")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_single_char_operators() {
        let input = "~+-*/%^!&|< > =";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::Tilde, Some("~")),
            (Kind::Plus, Some("+")),
            (Kind::Dash, Some("-")),
            (Kind::Asterisk, Some("*")),
            (Kind::Slash, Some("/")),
            (Kind::Percent, Some("%")),
            (Kind::Xor, Some("^")),
            (Kind::Bang, Some("!")),
            (Kind::Ampersand, Some("&")),
            (Kind::Pipe, Some("|")),
            (Kind::Lt, Some("<")),
            (Kind::Gt, Some(">")),
            (Kind::Assign, Some("=")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_double_char_operators() {
        let input = "+= -= -- ++ *= /= %= ^= != == &= |= << >> <> >= <= && ||";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::AddAssign, Some("+=")),
            (Kind::SubAssign, Some("-=")),
            (Kind::Decrement, Some("--")),
            (Kind::Increment, Some("++")),
            (Kind::MulAssign, Some("*=")),
            (Kind::DivAssign, Some("/=")),
            (Kind::ModAssign, Some("%=")),
            (Kind::XorAssign, Some("^=")),
            (Kind::NotEqual, Some("!=")),
            (Kind::Equal, Some("==")),
            (Kind::AndAssign, Some("&=")),
            (Kind::OrAssign, Some("|=")),
            (Kind::Shl, Some("<<")),
            (Kind::Shr, Some(">>")),
            (Kind::Gte, Some(">=")),
            (Kind::Lte, Some("<=")),
            (Kind::LogicalAnd, Some("&&")),
            (Kind::LogicalOr, Some("||")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_double_char_tokens() {
        let input = ":: -> |> =>";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::Accessor, Some("::")),
            (Kind::Arrow, Some("->")),
            (Kind::Chevron, Some("|>")),
            (Kind::FatArrow, Some("=>")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_triple_char_operators() {
        let input = "<<= >>=";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::ShlAssign, Some("<<=")),
            (Kind::ShrAssign, Some(">>=")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_keywords() {
        let input = "let    if    for    else    pub    new    while    out    func    module    import    def    continue    break    return    sizeof    union    struct    enum";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::LetKw, Some("let")),
            (Kind::IfKw, Some("if")),
            (Kind::ForKw, Some("for")),
            (Kind::ElseKw, Some("else")),
            (Kind::PubKw, Some("pub")),
            (Kind::NewKw, Some("new")),
            (Kind::WhileKw, Some("while")),
            (Kind::OutKw, Some("out")),
            (Kind::FuncKw, Some("func")),
            (Kind::ModuleKw, Some("module")),
            (Kind::ImportKw, Some("import")),
            (Kind::DefKw, Some("def")),
            (Kind::ContinueKw, Some("continue")),
            (Kind::BreakKw, Some("break")),
            (Kind::ReturnKw, Some("return")),
            (Kind::SizeofKw, Some("sizeof")),
            (Kind::UnionKw, Some("union")),
            (Kind::StructKw, Some("struct")),
            (Kind::EnumKw, Some("enum")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_floats() {
        let input = "45.4349 679.39 0.5 0.569 3206.4 .542 7e2 7e-8 3.6e-2";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::FloatLiteral, Some("45.4349")),
            (Kind::FloatLiteral, Some("679.39")),
            (Kind::FloatLiteral, Some("0.5")),
            (Kind::FloatLiteral, Some("0.569")),
            (Kind::FloatLiteral, Some("3206.4")),
            (Kind::FloatLiteral, Some(".542")),
            (Kind::FloatLiteral, Some("7e2")),
            (Kind::FloatLiteral, Some("7e-8")),
            (Kind::FloatLiteral, Some("3.6e-2")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }

    #[test]
    fn test_ints() {
        let input = "45 679 3206 0 1 0 5 459 0";
        let mut lexer = Lexer::new(input);
        let expected = vec![
            (Kind::IntLiteral, Some("45")),
            (Kind::IntLiteral, Some("679")),
            (Kind::IntLiteral, Some("3206")),
            (Kind::IntLiteral, Some("0")),
            (Kind::IntLiteral, Some("1")),
            (Kind::IntLiteral, Some("0")),
            (Kind::IntLiteral, Some("5")),
            (Kind::IntLiteral, Some("459")),
            (Kind::IntLiteral, Some("0")),
        ];

        for (kind, lexeme) in expected {
            let token = lexer.read_next();
            assert_token(token, kind, lexeme);
        }
    }
}
