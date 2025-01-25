use std::slice::Iter;

use ast::{
    AssignTarget, Block, BlockItem, Expr, ForInit, Identifier, IfAlternate, IfExpr, LetBinding,
    LiteralValue, MemberExpr, MemberTy, Parameter, Program, ProgramItem, Ty, Type,
};
use interner::{lookup, Atom};
use lexer::{
    operator::{AssignOp, BinaryOp, PostfixOp, PrefixOp},
    value::Value,
    Kind, Token,
};
use node::Node;
use symbols::{FunctionSymbol, Mutability, Symbol, SymbolTable, VariableSymbol, Visibility};

pub struct Parser<'ctx> {
    _source: &'ctx str,
    prev_token_end: usize,
    tokens: Iter<'ctx, Token>,
    scopes: Vec<SymbolTable>,
}

trait NodeFinish {
    fn finish(&mut self, p: &Parser) -> Node;
}

impl NodeFinish for Node {
    fn finish(&mut self, p: &Parser) -> Node {
        self.end = p.prev_token_end;
        *self
    }
}

impl<'ctx> Parser<'ctx> {
    pub fn new(tokens: &'ctx mut [Token], source: &'ctx str) -> Self {
        Self {
            tokens: tokens.iter(),
            _source: source,
            prev_token_end: 0,
            scopes: vec![SymbolTable::new()],
        }
    }

    /// Parses the tokens returning the top-level Program Node
    pub fn parse(&mut self) -> Program {
        let mut node = self.start_node();

        self.push_scope();

        let mut items = Vec::new();

        while let Some(token) = self.cur_token() {
            let item = match token.kind {
                Kind::PubKw => match self.lookahead_n(1).map(|token| token.kind) {
                    Some(Kind::FuncKw) => self.parse_function(),
                    Some(Kind::EnumKw) => todo!(),
                    Some(Kind::StructKw) => todo!(),
                    Some(Kind::UnionKw) => todo!(),
                    Some(Kind::ImportKw) => todo!(),
                    _ => unreachable!(),
                },
                Kind::FuncKw => self.parse_function(),
                Kind::EnumKw => todo!(),
                Kind::StructKw => todo!(),
                Kind::UnionKw => todo!(),
                Kind::ImportKw => todo!(),
                Kind::ModuleKw => todo!(),
                Kind::DefKw => todo!(),
                _ => unreachable!(),
            };

            items.push(item)
        }

        Program {
            sid: self.pop_scope(),
            node: node.finish(self),
            items,
        }
    }

    fn parse_function(&mut self) -> ProgramItem {
        let mut node = self.start_node();
        let public = self.eat(Kind::PubKw);
        self.bump(Kind::FuncKw);

        let ty = self.parse_type();

        let mut ident = self.parse_identifier();

        let (mut symbol, id) = FunctionSymbol::new(
            ident.name,
            ty.clone(),
            vec![],
            if public {
                Visibility::Public
            } else {
                Visibility::Private
            },
        );

        ident.sid = Some(id);

        self.bump(Kind::LParen);

        if self.eat(Kind::RParen) {
            let body = self.parse_block();

            self.current_scope()
                .define(&ident.name, Symbol::Function(symbol));

            return ProgramItem::Function {
                node: node.finish(self),
                ty,
                ident,
                public,
                parameters: vec![],
                body,
            };
        }

        self.push_scope();

        let mut parameters = vec![self.parse_parameter()];
        while let Some(Kind::Comma) = self.cur_kind() {
            self.bump(Kind::Comma);
            parameters.push(self.parse_parameter());
        }

        symbol.parameters = parameters
            .iter()
            .map(|param| param.ident.sid.unwrap())
            .collect();

        self.bump(Kind::RParen);
        self.pop_scope();

        self.current_scope()
            .define(&ident.name, Symbol::Function(symbol.clone()));

        let body = self.parse_block();

        ProgramItem::Function {
            node: node.finish(self),
            ty,
            public,
            ident,
            parameters,
            body,
        }
    }

    fn parse_parameter(&mut self) -> Parameter {
        let mut node = self.start_node();

        let ty = self.parse_type();
        self.bump(Kind::Colon);
        let mut ident = self.parse_identifier();

        let (symbol, id) = VariableSymbol::new(
            ident.name,
            ty.clone(),
            Visibility::Private,
            Mutability::Immutable,
        );

        ident.sid = Some(id);

        self.current_scope()
            .define(&ident.name, Symbol::Variable(symbol));

        Parameter {
            node: node.finish(self),
            ty,
            ident,
        }
    }

    fn parse_block(&mut self) -> Block {
        let mut node = self.start_node();
        self.bump(Kind::LSquirly);

        let mut items = vec![];

        while let Some(kind) = self.cur_kind() {
            let item = match kind {
                Kind::RSquirly => break,

                // Statements
                Kind::LetKw => BlockItem::Let(self.parse_let_statement()),
                Kind::OutKw => self.parse_out_statement(),
                Kind::BreakKw => {
                    let mut node = self.start_node();
                    self.advance();
                    self.eat(Kind::Semicolon);
                    BlockItem::Break {
                        node: node.finish(self),
                    }
                }
                Kind::ContinueKw => {
                    let mut node = self.start_node();
                    self.advance();
                    self.eat(Kind::Semicolon);
                    BlockItem::Continue {
                        node: node.finish(self),
                    }
                }

                // Expressions
                Kind::Increment
                | Kind::Decrement
                | Kind::NewKw
                | Kind::SizeofKw
                | Kind::Ampersand
                | Kind::Asterisk
                | Kind::Tilde
                | Kind::Bang
                | Kind::Dash
                | Kind::LBracket
                | Kind::ReturnKw
                | Kind::IntLiteral
                | Kind::FloatLiteral
                | Kind::StringLiteral
                | Kind::BoolLiteral
                | Kind::ForKw
                | Kind::IfKw
                | Kind::WhileKw
                | Kind::LParen
                | Kind::Identifier => {
                    let mut node = self.start_node();
                    let expr = self.parse_expr(None);

                    let semicolon = self.eat(Kind::Semicolon);

                    BlockItem::Expr {
                        node: node.finish(self),
                        semicolon,
                        expr,
                    }
                }
                c => unreachable!("{c:#?}"),
            };

            items.push(item);
        }

        self.bump(Kind::RSquirly);

        Block {
            node: node.finish(self),
            items,
        }
    }

    fn parse_out_statement(&mut self) -> BlockItem {
        let mut node = self.start_node();
        self.bump(Kind::OutKw);

        let format_str = self.parse_expr(None);

        if self.eat(Kind::Semicolon) {
            return BlockItem::Out {
                node: node.finish(self),
                format_str,
                arguments: vec![],
            };
        }

        let mut arguments = vec![self.parse_expr(None)];
        while let Some(Kind::Comma) = self.cur_kind() {
            self.bump(Kind::Comma);

            arguments.push(self.parse_expr(None));
        }

        self.eat(Kind::Semicolon);

        BlockItem::Out {
            node: node.finish(self),
            format_str,
            arguments,
        }
    }

    fn parse_let_statement(&mut self) -> LetBinding {
        let mut node = self.start_node();
        self.bump(Kind::LetKw);

        let mut ident = self.parse_identifier();
        self.bump(Kind::Colon);
        let ty = self.parse_type();

        let (symbol, id) = VariableSymbol::new(
            ident.name,
            ty.clone(),
            Visibility::Private,
            Mutability::Mutable,
        );

        ident.sid = Some(id);

        let mut init: Option<Expr> = None;

        if self.eat(Kind::Assign) {
            init = Some(self.parse_expr(None));
        }

        self.current_scope()
            .define(&ident.name, Symbol::Variable(symbol));

        self.eat(Kind::Semicolon);

        LetBinding {
            node: node.finish(self),
            ident,
            ty,
            init,
        }
    }

    fn parse_identifier(&mut self) -> Identifier {
        let mut node = self.start_node();

        let token = self.bump(Kind::Identifier);
        let value = match token.value {
            Some(Value::String(atom)) => atom,
            _ => unreachable!(),
        };

        Identifier {
            node: node.finish(self),
            sid: None,
            name: value,
        }
    }

    fn parse_expr(&mut self, precedence: Option<u32>) -> Expr {
        let left = self.parse_primary();

        if AssignOp::from_kind(*self.cur_kind().unwrap()).is_some() {
            return self.parse_assign_expr(left);
        }

        if PostfixOp::from_kind(*self.cur_kind().unwrap()).is_some() {
            return self.parse_postfix(left);
        }

        self.parse_binary_expr(left, precedence.unwrap_or(0u32))
    }

    fn parse_binary_expr(&mut self, mut left: Expr, min_precedence: u32) -> Expr {
        while let Some(token) = self.cur_token() {
            let operator = match BinaryOp::from_kind(token.kind) {
                Some(_) if self.get_precedence(token.kind) < min_precedence => break,
                Some(operator) => operator,
                _ => break,
            };

            let op_precedence = self.get_precedence(token.kind);

            self.advance();
            let mut right = self.parse_primary();

            while let Some(token) = self.cur_token() {
                match BinaryOp::from_kind(token.kind) {
                    Some(_) if self.get_precedence(token.kind) <= op_precedence => break,
                    Some(_) => {}
                    _ => break,
                };

                right = self.parse_binary_expr(
                    right,
                    op_precedence
                        + if self.get_precedence(token.kind) > op_precedence {
                            1
                        } else {
                            0
                        },
                )
            }

            left = Expr::Binary {
                node: Node::new(left.node().start, right.node().end),
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
        }

        // if let Some(kind) = self.cur_kind() {
        //     if let Some(operator) = AssignOp::from_kind(*kind) {
        //         self.advance(); // consume operator
        //         left = self.parse_primary();
        //
        //         let right = self.parse_binary_expr(left.clone(), 0);
        //
        //         left = Expr::Assign {
        //             node: Node::new(left.node().start, right.node().end),
        //             target: match left {
        //                 Expr::Member(member) => AssignTarget::Member(member),
        //                 Expr::Identifier(ident) => AssignTarget::Identifier(ident),
        //                 _ => unreachable!(),
        //             },
        //             operator,
        //             expr: Box::new(right),
        //         }
        //     }
        // }
        //
        left
    }

    fn get_precedence(&self, kind: Kind) -> u32 {
        match kind {
            Kind::LogicalOr => 1,
            Kind::LogicalAnd => 2,
            Kind::Pipe => 3,
            Kind::Xor => 4,
            Kind::Ampersand => 5,
            Kind::Equal | Kind::NotEqual => 6,
            Kind::Lt | Kind::Gt | Kind::Gte | Kind::Lte => 7,
            Kind::Shl | Kind::Shr => 8,
            Kind::Plus | Kind::Dash => 9,
            Kind::Asterisk | Kind::Slash | Kind::Percent => 10,
            _ => unreachable!(),
        }
    }

    fn parse_primary(&mut self) -> Expr {
        let expr = match self.cur_kind() {
            Some(Kind::Increment)
            | Some(Kind::Decrement)
            | Some(Kind::NewKw)
            | Some(Kind::SizeofKw)
            | Some(Kind::Ampersand)
            | Some(Kind::Asterisk)
            | Some(Kind::Tilde)
            | Some(Kind::Bang)
            | Some(Kind::Dash) => self.parse_prefix(),
            // TODO: Differentiate between parenthesis expression, tuple expression, and type cast
            Some(Kind::LParen) => match self.lookahead_n(1).map(|x| x.kind) {
                Some(Kind::PrimitiveType) => self.parse_cast_expr(),
                _ => {
                    let mut node = self.start_node();
                    self.advance();
                    let expr = self.parse_expr(None);
                    self.advance();

                    Expr::Paren {
                        node: node.finish(self),
                        expr: Box::new(expr),
                    }
                }
            },
            Some(Kind::Identifier) => match self.lookahead_n(1).map(|x| x.kind) {
                Some(Kind::LParen) => self.parse_call_expr(None),
                Some(Kind::LBracket) => self.parse_index_expr(None),
                Some(Kind::Accessor) => Expr::Member(self.parse_member_expr(MemberTy::Namespace)),
                Some(Kind::Period) => Expr::Member(self.parse_member_expr(MemberTy::Property)),
                _ => Expr::Identifier(self.parse_identifier()),
            },
            // Returns Array Init Expr or Array Expr depending on context
            Some(Kind::LBracket) => self.parse_array(),
            Some(Kind::ReturnKw) => self.parse_return_expr(),
            Some(Kind::ForKw) => self.parse_for_expr(),
            Some(Kind::IfKw) => Expr::If(self.parse_if_expr()),
            Some(Kind::WhileKw) => self.parse_while_expr(),
            Some(Kind::IntLiteral)
            | Some(Kind::FloatLiteral)
            | Some(Kind::StringLiteral)
            | Some(Kind::BoolLiteral) => self.parse_literal(),
            c => unreachable!("{c:#?}"),
        };

        // Handle Call Expression, and index expressions if applicable
        match (&expr, self.cur_kind()) {
            (Expr::Member(member), Some(Kind::LParen)) => {
                self.parse_call_expr(Some(AssignTarget::Member(member.clone())))
            }
            (Expr::Member(member), Some(Kind::LBracket)) => {
                self.parse_index_expr(Some(AssignTarget::Member(member.clone())))
            }
            _ => expr,
        }
    }

    fn parse_postfix(&mut self, left: Expr) -> Expr {
        let mut node = self.start_node();

        if let Some(op) = PostfixOp::from_kind(self.advance().kind) {
            Expr::PostfixUnary {
                node: node.finish(self),
                operand: Box::new(left),
                operator: op,
            }
        } else {
            panic!("Invalid operator for postfix")
        }
    }

    fn parse_assign_expr(&mut self, left: Expr) -> Expr {
        let token = self.advance();
        if let Some(operator) = AssignOp::from_kind(token.kind) {
            let right = self.parse_expr(None);

            return Expr::Assign {
                node: Node::new(left.node().start, right.node().end),
                target: match left {
                    Expr::Member(member) => AssignTarget::Member(member),
                    Expr::Identifier(ident) => AssignTarget::Identifier(ident),
                    _ => unreachable!(),
                },
                operator,
                expr: Box::new(right),
            };
        }

        panic!("Oh no")
    }

    /// ( Identifier | MemberExpr ) ~ ( "<" ~ ...Type ~ ">" ) ~ "(" ~ ( Expr )* ~ ")"
    fn parse_call_expr(&mut self, callee: Option<AssignTarget>) -> Expr {
        let mut node = match &callee {
            Some(expr) => expr.node(),
            None => self.start_node(),
        };

        let callee = callee.unwrap_or_else(|| AssignTarget::Identifier(self.parse_identifier()));

        self.bump(Kind::LParen);

        if self.eat(Kind::RParen) {
            return Expr::Call {
                node: node.finish(self),
                callee,
                args: vec![],
            };
        }

        let mut args = vec![self.parse_expr(None)];

        while let Some(Kind::Comma) = self.cur_kind() {
            self.bump(Kind::Comma);
            args.push(self.parse_expr(None));
        }

        self.bump(Kind::RParen);

        Expr::Call {
            node: node.finish(self),
            callee,
            args,
        }
    }

    /// ( Identifier | MemberExpr ) ~ "[" ~ Expr ~ "]"
    fn parse_index_expr(&mut self, member: Option<AssignTarget>) -> Expr {
        let mut node = match &member {
            Some(expr) => expr.node(),
            None => self.start_node(),
        };

        let member = member.unwrap_or_else(|| AssignTarget::Identifier(self.parse_identifier()));

        self.bump(Kind::LBracket);
        let index = self.parse_expr(None);
        self.bump(Kind::RBracket);

        Expr::IndexExpr {
            node: node.finish(self),
            member,
            index: Box::new(index),
        }
    }

    // /// ( Identifier | MemberExpr ) ~ ("*=" | "+=" | "/=" | "%=" ... ) ~ Expr
    // fn parse_assign_expr(&mut self) -> Expr {}

    /// Identifier ~ ( "::" | "." ) ~ ( Identifier ~ ( "::" | "." )? )*
    fn parse_member_expr(&mut self, member_ty: MemberTy) -> MemberExpr {
        let mut node = self.start_node();

        let mut segments = vec![self.parse_identifier()];

        match member_ty {
            MemberTy::Namespace => {
                while let Some(Kind::Accessor) = self.cur_kind() {
                    self.bump(Kind::Accessor);
                    segments.push(self.parse_identifier());
                }
            }
            MemberTy::Property => {
                while let Some(Kind::Period) = self.cur_kind() {
                    self.bump(Kind::Period);
                    segments.push(self.parse_identifier());
                }
            }
        };

        MemberExpr {
            ty: member_ty,
            node: node.finish(self),
            segments,
        }
    }

    /// ( PrimitiveType ) ~ Expr
    fn parse_cast_expr(&mut self) -> Expr {
        let mut node = self.start_node();
        self.bump(Kind::LParen);

        let ty = self.parse_type();

        self.bump(Kind::RParen);

        let expr = self.parse_expr(None);

        Expr::Cast {
            node: node.finish(self),
            ty,
            expr: Box::new(expr),
        }
    }

    /// Returns Array Init Expr or Array Expr depending on context
    /// "[" ~ Expr* ~ ","? ~ "]" | "[" ~ Type ~ ";" ~ Expr ~ "]"
    fn parse_array(&mut self) -> Expr {
        if let Some(Kind::Semicolon) = self.lookahead_kind_n(2) {
            // Array Init Expr
            return self.parse_array_init();
        }

        self.parse_array_expr()
    }

    /// "[" ~ Type ~ ";" ~ Expr ~ "]"
    fn parse_array_init(&mut self) -> Expr {
        let mut node = self.start_node();
        self.bump(Kind::LBracket);

        let ty = self.parse_type();
        self.bump(Kind::Semicolon);
        let expr = self.parse_expr(None);

        Expr::ArrayInit {
            node: node.finish(self),
            ty,
            size: Box::new(expr),
        }
    }

    /// "[" ~ Expr* ~ ","? ~ "]"
    fn parse_array_expr(&mut self) -> Expr {
        let mut node = self.start_node();
        self.bump(Kind::LBracket);

        if self.eat(Kind::RBracket) {
            return Expr::Array {
                node: node.finish(self),
                elements: vec![],
            };
        }

        let mut elements = vec![self.parse_expr(None)];
        while let Some(Kind::Comma) = self.cur_kind() {
            self.bump(Kind::Comma);
            elements.push(self.parse_expr(None));
        }

        Expr::Array {
            node: node.finish(self),
            elements,
        }
    }

    /// "return" ~ Expr
    fn parse_return_expr(&mut self) -> Expr {
        let mut node = self.start_node();
        self.bump(Kind::ReturnKw);

        let init = self.parse_expr(None);

        Expr::Return {
            node: node.finish(self),
            init: Box::new(init),
        }
    }

    /// "for" ~ Expr ~ "," ~ Expr ~ "," ~ Expr ~ Block
    fn parse_for_expr(&mut self) -> Expr {
        let mut node = self.start_node();
        self.bump(Kind::ForKw);

        let init: ForInit = if self.at(Kind::LetKw) {
            ForInit::Let(Box::new(self.parse_let_statement()))
        } else {
            ForInit::Expr(Box::new(self.parse_expr(None)))
        };

        self.bump(Kind::Comma);

        let condition = self.parse_expr(None);

        self.bump(Kind::Comma);

        let update = self.parse_expr(None);
        let block = self.parse_block();

        Expr::For {
            node: node.finish(self),
            init,
            condition: Box::new(condition),
            update: Box::new(update),
            block,
        }
    }

    /// "if" ~ Expr ~ Block ~ (Block | IfExpr)?
    fn parse_if_expr(&mut self) -> IfExpr {
        let mut node = self.start_node();
        self.bump(Kind::IfKw);

        let condition = self.parse_expr(None);

        let block = self.parse_block();

        let mut alternate: Option<Box<IfAlternate>> = None;

        if self.eat(Kind::ElseKw) {
            if self.at(Kind::IfKw) {
                alternate = Some(Box::new(IfAlternate::If(self.parse_if_expr())));
            } else if self.at(Kind::LSquirly) {
                alternate = Some(Box::new(IfAlternate::Else(self.parse_block())));
            } else {
                panic!("Invalid token")
            }
        }

        IfExpr {
            node: node.finish(self),
            condition: Box::new(condition),
            block,
            alternate,
        }
    }

    /// "while" ~ Expr ~ Block
    fn parse_while_expr(&mut self) -> Expr {
        let mut node = self.start_node();
        self.bump(Kind::WhileKw);

        let condition = self.parse_expr(None);

        let block = self.parse_block();

        Expr::While {
            node: node.finish(self),
            condition: Box::new(condition),
            block,
        }
    }

    fn parse_literal(&mut self) -> Expr {
        let mut node = self.start_node();
        let token = self.advance();

        Expr::Literal {
            node: node.finish(self),
            value: LiteralValue::from_value(token.value.unwrap()),
        }
    }

    fn parse_type(&mut self) -> Type {
        let mut node = self.start_node();

        let token = self.cur_token().unwrap();
        let ty: Ty = match token.kind {
            Kind::Identifier => self.parse_identifier_type(),
            Kind::LBracket => self.parse_array_type(),
            Kind::PrimitiveType => self.parse_primitive_type(),
            c => unreachable!("{c:?}"),
        };

        Type {
            node: node.finish(self),
            ty,
        }
    }

    fn parse_primitive_type(&mut self) -> Ty {
        let token = self.advance();

        match lookup!(token.lexeme).as_str() {
            "u32" => Ty::U32,
            "i32" => Ty::I32,
            "i64" => Ty::I64,
            "u64" => Ty::U64,
            "double" => Ty::Double,
            "single" => Ty::Single,
            "str" => Ty::Str,
            "bool" => Ty::Bool,
            "void" => Ty::Void,
            _ => unreachable!(),
        }
    }

    fn parse_array_type(&mut self) -> Ty {
        self.bump(Kind::LBracket);

        let ty = self.parse_type();
        self.bump(Kind::Semicolon);
        let num = self.parse_expr(None);

        self.bump(Kind::RBracket);

        Ty::Array(Box::new(ty), Box::new(num))
    }

    fn parse_identifier_type(&mut self) -> Ty {
        let ident = self.parse_identifier();

        Ty::Identifier(ident.name)
    }

    fn parse_prefix(&mut self) -> Expr {
        let mut node = self.start_node();
        let token = self.advance();
        let operand = self.parse_expr(Some(12));

        Expr::PrefixUnary {
            node: node.finish(self),
            operator: match token.kind {
                Kind::Dash => PrefixOp::Neg,
                Kind::Increment => PrefixOp::Increment,
                Kind::Decrement => PrefixOp::Decrement,
                Kind::NewKw => PrefixOp::New,
                Kind::SizeofKw => PrefixOp::Sizeof,
                Kind::Ampersand => PrefixOp::Addr,
                Kind::Asterisk => PrefixOp::Deref,
                Kind::Tilde => PrefixOp::Not,
                Kind::Bang => PrefixOp::LogNot,
                _ => unreachable!(),
            },
            operand: Box::new(operand),
        }
    }

    #[allow(dead_code)]
    fn current_scope(&mut self) -> &mut SymbolTable {
        self.scopes.last_mut().unwrap()
    }

    fn lookahead_kind_n(&self, n: usize) -> Option<&Kind> {
        let mut tokens = self.tokens.clone();
        tokens.nth(n).map(|x| &x.kind)
    }

    fn lookahead_n(&self, n: usize) -> Option<&Token> {
        let mut tokens = self.tokens.clone();
        tokens.nth(n)
    }

    fn push_scope(&mut self) {
        self.scopes.push(SymbolTable::new());
    }

    fn pop_scope(&mut self) -> Atom {
        if let Some(scope) = self.scopes.pop() {
            scope.id
        } else {
            panic!("Cannot pop the top-level scope");
        }
    }

    fn start_node(&self) -> Node {
        if let Some(token) = self.cur_token() {
            Node::new(token.start, usize::MAX)
        } else {
            panic!("Tried to read but encountered end of file.")
        }
    }

    fn cur_token(&self) -> Option<&Token> {
        let mut tokens = self.tokens.clone();
        tokens.next()
    }

    fn cur_kind(&self) -> Option<&Kind> {
        if let Some(token) = self.cur_token() {
            Some(&token.kind)
        } else {
            None
        }
    }

    fn at(&self, kind: Kind) -> bool {
        if let Some(current_kind) = self.cur_kind() {
            *current_kind == kind
        } else {
            false
        }
    }

    fn bump(&mut self, kind: Kind) -> Token {
        if self.at(kind) {
            self.advance()
        } else {
            panic!(
                "Unexpected token encountered. Expected {kind:?} got {:?}",
                self.cur_token()
            )
        }
    }

    fn advance(&mut self) -> Token {
        let token = self.tokens.next();
        if let Some(token) = token {
            self.prev_token_end = token.end;
            *token
        } else {
            panic!("Unexpected token encountered, got None")
        }
    }

    fn eat(&mut self, kind: Kind) -> bool {
        if self.at(kind) {
            self.advance();
            return true;
        }
        false
    }
}
