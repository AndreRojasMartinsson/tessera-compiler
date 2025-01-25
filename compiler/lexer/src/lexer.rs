use std::{ops::Range, str::Chars};

use diagnostics::errors::lexer::{
    InvalidFloat, InvalidInteger, LexerError, UnexpectedCharacter, UnexpectedEnd,
    UnterminatedString,
};
use interner::intern;

use crate::{value::Value, Kind, Token};

pub struct Lexer<'ctx> {
    source: &'ctx str,
    chars: Chars<'ctx>,
}

#[derive(Debug)]
enum KindResult {
    Kind(Kind),
    Ignore,
}

impl<'ctx> Lexer<'ctx> {
    pub fn new(source: &'ctx str) -> Self {
        Self {
            source,
            chars: source.chars(),
        }
    }

    pub fn read_next(&mut self) -> Result<Token, LexerError> {
        let start = self.offset();
        let kind = self.read_next_kind();
        let end = self.offset();

        let raw = &self.source[start..end];

        if let Some(KindResult::Kind(kind)) = kind {
            let value = self.get_token_value(start..end, raw, &kind)?;
            Ok(Token::new(kind, value, start..end, raw))
        } else if let Some(KindResult::Ignore) = kind {
            self.read_next()
        } else {
            Ok(Token::new(Kind::Eof, None, start..end, ""))
        }
    }

    fn read_next_kind(&mut self) -> Option<KindResult> {
        while let Some(c) = self.peek() {
            match c {
                '"' => return Some(self.read_string().unwrap()),
                '#' => {
                    self.chars.next();

                    while let Some(ch) = self.peek() {
                        if ch == '\n' {
                            break;
                        }

                        self.chars.next();
                    }

                    self.chars.next();

                    return Some(KindResult::Ignore);
                }
                ' ' | '\t' | '\r' | '\n' => {
                    self.chars.next();

                    while let Some(' ' | '\t' | '\r' | '\n') = self.peek() {
                        self.chars.next();
                    }

                    return Some(KindResult::Ignore);
                }
                ';' => return self.flush_single(Kind::Semicolon),
                '(' => return self.flush_single(Kind::LParen),
                ')' => return self.flush_single(Kind::RParen),
                '{' => return self.flush_single(Kind::LSquirly),
                '}' => return self.flush_single(Kind::RSquirly),
                '[' => return self.flush_single(Kind::LBracket),
                ']' => return self.flush_single(Kind::RBracket),
                ',' => return self.flush_single(Kind::Comma),
                '~' => return self.flush_single(Kind::Tilde),
                ':' => match self.peek_n(1) {
                    Some(':') => return self.flush(Kind::Accessor, 2),
                    Some(_) | None => return self.flush_single(Kind::Colon),
                },
                '^' => match self.peek_n(1) {
                    Some('=') => return self.flush(Kind::XorAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Xor),
                },
                '&' => match self.peek_n(1) {
                    Some('&') => return self.flush(Kind::LogicalAnd, 2),
                    Some('=') => return self.flush(Kind::AndAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Ampersand),
                },
                '|' => match self.peek_n(1) {
                    Some('|') => return self.flush(Kind::LogicalOr, 2),
                    Some('=') => return self.flush(Kind::OrAssign, 2),
                    Some('>') => return self.flush(Kind::Chevron, 2),
                    Some(_) | None => return self.flush_single(Kind::Pipe),
                },
                '!' => match self.peek_n(1) {
                    Some('=') => return self.flush(Kind::NotEqual, 2),
                    Some(_) | None => return self.flush_single(Kind::Bang),
                },
                '=' => match self.peek_n(1) {
                    Some('>') => return self.flush(Kind::FatArrow, 2),
                    Some('=') => return self.flush(Kind::Equal, 2),
                    Some(_) | None => return self.flush_single(Kind::Assign),
                },
                '>' => match self.peek_n(1) {
                    Some('>') => match self.peek_n(2) {
                        Some('=') => return self.flush(Kind::ShrAssign, 3),
                        Some(_) | None => return self.flush(Kind::Shr, 2),
                    },
                    Some('=') => return self.flush(Kind::Gte, 2),
                    Some(_) | None => return self.flush_single(Kind::Gt),
                },
                '<' => match self.peek_n(1) {
                    Some('=') => return self.flush(Kind::Lte, 2),
                    Some('<') => match self.peek_n(2) {
                        Some('=') => return self.flush(Kind::ShlAssign, 3),
                        Some(_) | None => return self.flush(Kind::Shl, 2),
                    },
                    Some(_) | None => return self.flush_single(Kind::Lt),
                },
                '+' => match self.peek_n(1) {
                    Some('+') => return self.flush(Kind::Increment, 2),
                    Some('=') => return self.flush(Kind::AddAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Plus),
                },
                '-' => match self.peek_n(1) {
                    Some('>') => return self.flush(Kind::Arrow, 2),
                    Some('-') => return self.flush(Kind::Decrement, 2),
                    Some('=') => return self.flush(Kind::SubAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Dash),
                },
                '*' => match self.peek_n(1) {
                    Some('=') => return self.flush(Kind::MulAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Asterisk),
                },
                '/' => match self.peek_n(1) {
                    Some('=') => return self.flush(Kind::DivAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Slash),
                },
                '%' => match self.peek_n(1) {
                    Some('=') => return self.flush(Kind::ModAssign, 2),
                    Some(_) | None => return self.flush_single(Kind::Percent),
                },
                '.' => match self.peek_n(1) {
                    Some('0'..='9') => return Some(self.read_float_after_period().unwrap()),
                    Some(_) | None => return self.flush_single(Kind::Period),
                },
                'a'..='z' | 'A'..='Z' | '_' => return Some(self.read_identifier()),
                '0' => return Some(self.read_number_starting_with_zero().unwrap()),
                '1'..='9' => return Some(self.read_number_after_first_digit().unwrap()),
                _ => {}
            }
        }

        Some(KindResult::Kind(Kind::Eof))
    }

    fn read_string(&mut self) -> Result<KindResult, LexerError> {
        let offset = self.offset();
        self.chars.next();

        while let Some(char) = self.peek() {
            if char == '"' || char == '\n' {
                break;
            }

            self.chars.next();
        }

        if self.peek() == Some('\n') {
            self.chars.next();
            return Err(LexerError::UnterminatedString(UnterminatedString {
                src: self.source.into(),
                at: (offset..self.offset()).into(),
                started_at: (offset..offset + 1).into(),
            }));
        }

        self.chars.next();
        Ok(KindResult::Kind(Kind::StringLiteral))
    }

    fn read_number_after_first_digit(&mut self) -> Result<KindResult, LexerError> {
        self.read_decimal_digits_after_first_digit()?;

        if let Some('.') = self.peek() {
            self.chars.next();
            return self.read_float_after_period_after_digits();
        };

        let has_exponent = self.read_optional_exponent()?;
        if has_exponent {
            Ok(KindResult::Kind(Kind::FloatLiteral))
        } else {
            Ok(KindResult::Kind(Kind::IntLiteral))
        }
    }

    fn read_number_starting_with_zero(&mut self) -> Result<KindResult, LexerError> {
        self.chars.next();

        match self.peek() {
            Some('.') => {
                self.chars.next();
                self.read_float_after_period_after_digits()?;
                Ok(KindResult::Kind(Kind::FloatLiteral))
            }
            Some('e' | 'E') => {
                self.chars.next();
                self.read_decimal_exponent()?;
                Ok(KindResult::Kind(Kind::FloatLiteral))
            }
            _ => Ok(KindResult::Kind(Kind::IntLiteral)),
        }
    }

    fn read_float_after_period_after_digits(&mut self) -> Result<KindResult, LexerError> {
        self.read_optional_decimal_digits()?;
        self.read_optional_exponent()?;

        Ok(KindResult::Kind(Kind::FloatLiteral))
    }

    fn read_optional_decimal_digits(&mut self) -> Result<(), LexerError> {
        if let Some('0'..='9') = self.peek() {
            self.chars.next();
        } else {
            return Ok(());
        }

        self.read_decimal_digits_after_first_digit()?;
        Ok(())
    }

    fn read_float_after_period(&mut self) -> Result<KindResult, LexerError> {
        self.chars.next(); // Consume the initial period

        self.read_decimal_digits()?;
        self.read_optional_exponent()?;

        Ok(KindResult::Kind(Kind::FloatLiteral))
    }

    fn read_decimal_digits(&mut self) -> Result<(), LexerError> {
        if let Some('0'..='9') = self.peek() {
            self.chars.next();
        } else if self.peek().is_some() {
            let start = self.offset();
            self.chars.next();

            return Err(LexerError::UnexpectedChar(UnexpectedCharacter {
                src: self.source.into(),
                at: (start..self.offset()).into(),
            }));
        } else if self.peek().is_none() {
            let start = self.offset();
            self.chars.next();

            return Err(LexerError::UnexpectedEnd(UnexpectedEnd {
                src: self.source.into(),
                at: (start..self.offset()).into(),
            }));
        };

        self.read_decimal_digits_after_first_digit()?;

        Ok(())
    }

    fn read_decimal_digits_after_first_digit(&mut self) -> Result<(), LexerError> {
        while let Some(next) = self.peek() {
            match next {
                '_' => {
                    self.chars.next();

                    if let Some('0'..='9') = self.peek() {
                        self.chars.next();
                    } else if self.peek().is_some() {
                        let start = self.offset();
                        self.chars.next();

                        return Err(LexerError::UnexpectedChar(UnexpectedCharacter {
                            src: self.source.into(),
                            at: (start..self.offset()).into(),
                        }));
                    };
                }
                '0'..='9' => {
                    self.chars.next();
                }
                _ => break,
            }
        }

        Ok(())
    }

    fn read_optional_exponent(&mut self) -> Result<bool, LexerError> {
        if let Some('e' | 'E') = self.peek() {
            self.chars.next();
            self.read_decimal_exponent()?;

            return Ok(true);
        }

        Ok(false)
    }

    fn read_decimal_exponent(&mut self) -> Result<(), LexerError> {
        if let Some('+' | '-') = self.peek() {
            self.chars.next();
        }

        self.read_decimal_digits()?;

        Ok(())
    }

    fn read_identifier(&mut self) -> KindResult {
        let start = self.offset();
        self.chars.next(); // Consume starting character;

        while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = self.peek() {
            self.chars.next();
        }

        let end = self.offset();

        let ident = &self.source[start..end];
        self.match_keyword(ident)
    }

    fn flush_single(&mut self, kind: Kind) -> Option<KindResult> {
        self.chars.next();
        Some(KindResult::Kind(kind))
    }

    /// Helper function to remove n number of characters
    /// from iterator while returning the Option of the kind.
    /// This way we dont have to do this for every case.
    fn flush(&mut self, kind: Kind, advances: usize) -> Option<KindResult> {
        self.chars.nth(advances - 1);
        Some(KindResult::Kind(kind))
    }

    fn match_keyword(&self, ident: &str) -> KindResult {
        // All keywords are between 2 and 8 characters long
        if ident.len() < 2 || ident.len() > 8 {
            return KindResult::Kind(Kind::Identifier);
        }

        match ident {
            "double" | "single" | "u64" | "i64" | "u32" | "i32" => {
                KindResult::Kind(Kind::PrimitiveType)
            }
            "let" => KindResult::Kind(Kind::LetKw),
            "if" => KindResult::Kind(Kind::IfKw),
            "for" => KindResult::Kind(Kind::ForKw),
            "else" => KindResult::Kind(Kind::ElseKw),
            "pub" => KindResult::Kind(Kind::PubKw),
            "new" => KindResult::Kind(Kind::NewKw),
            "while" => KindResult::Kind(Kind::WhileKw),
            "out" => KindResult::Kind(Kind::OutKw),
            "func" => KindResult::Kind(Kind::FuncKw),
            "module" => KindResult::Kind(Kind::ModuleKw),
            "import" => KindResult::Kind(Kind::ImportKw),
            "def" => KindResult::Kind(Kind::DefKw),
            "continue" => KindResult::Kind(Kind::ContinueKw),
            "break" => KindResult::Kind(Kind::BreakKw),
            "return" => KindResult::Kind(Kind::ReturnKw),
            "sizeof" => KindResult::Kind(Kind::SizeofKw),
            "union" => KindResult::Kind(Kind::UnionKw),
            "struct" => KindResult::Kind(Kind::StructKw),
            "enum" => KindResult::Kind(Kind::EnumKw),
            _ => KindResult::Kind(Kind::Identifier),
        }
    }

    fn get_token_value(
        &self,
        span: Range<usize>,
        raw: &str,
        kind: &Kind,
    ) -> Result<Option<Value>, LexerError> {
        let value: Option<Value> = match kind {
            Kind::Identifier => Some(Value::String(intern!(raw))),

            Kind::StringLiteral => Some(Value::String(intern!(raw))),
            Kind::BoolLiteral => Some(Value::Boolean(match raw {
                "true" => true,
                "false" => false,
                _ => unreachable!(),
            })),

            Kind::FloatLiteral => {
                let parsed = raw.parse::<f64>().map_err(|_| {
                    LexerError::InvalidFloat(InvalidFloat {
                        unexpected_char: None,
                        src: self.source.into(),
                        at: span.into(),
                    })
                })?;

                Some(Value::Float(parsed))
            }

            Kind::IntLiteral => {
                let parsed = raw.parse::<i64>().map_err(|_| {
                    LexerError::InvalidInt(InvalidInteger {
                        unexpected_char: None,
                        src: self.source.into(),
                        at: span.into(),
                    })
                })?;

                Some(Value::Integer(parsed))
            }

            _ => None,
        };

        Ok(value)
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.nth(n)
    }

    /// Get the length offset from the source text, in UTF-8 bytes
    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }
}
