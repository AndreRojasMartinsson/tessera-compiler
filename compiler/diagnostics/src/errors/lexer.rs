use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum LexerError {
    #[error(transparent)]
    #[diagnostic(transparent, severity(Error))]
    UnexpectedEnd(#[from] UnexpectedEnd),

    #[error(transparent)]
    #[diagnostic(transparent, severity(Error))]
    UnexpectedChar(#[from] UnexpectedCharacter),

    #[error(transparent)]
    #[diagnostic(transparent, severity(Error))]
    InvalidInt(#[from] InvalidInteger),

    #[error(transparent)]
    #[diagnostic(transparent, severity(Error))]
    InvalidFloat(#[from] InvalidFloat),

    #[error(transparent)]
    #[diagnostic(transparent, severity(Error))]
    UnterminatedString(#[from] UnterminatedString),
}

#[derive(Error, Diagnostic, Debug)]
#[error("Unterminated String Literal")]
#[help("Ensure all string literals are properly closed with matching quotation marks.")]
pub struct UnterminatedString {
    #[source_code]
    pub src: String,

    #[label("unclosed string literal here")]
    pub at: SourceSpan,

    #[label("started here")]
    pub started_at: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Invalid Float Format")]
#[help("Ensure that the number follows the correct format for a float.")]
pub struct InvalidFloat {
    #[source_code]
    pub src: String,

    #[label("malformed integer here")]
    pub at: SourceSpan,

    #[label("unexpected symbol here")]
    pub unexpected_char: Option<SourceSpan>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Invalid Integer Format")]
#[help("Ensure that the number follows the correct format for a integer.")]
#[diagnostic()]
pub struct InvalidInteger {
    #[source_code]
    pub src: String,

    #[label("malformed integer here")]
    pub at: SourceSpan,

    #[label("unexpected symbol here")]
    pub unexpected_char: Option<SourceSpan>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Unexpected End")]
#[help("Make sure you finish the literal.")]
pub struct UnexpectedEnd {
    #[source_code]
    pub src: String,

    #[label("unexpected end here")]
    pub at: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Unexpected Character")]
#[help("Make sure the input only contains valid ASCII characters for the current language.")]
pub struct UnexpectedCharacter {
    #[source_code]
    pub src: String,

    #[label("unexpected character here")]
    pub at: SourceSpan,
}
