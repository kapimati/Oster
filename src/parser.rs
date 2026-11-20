//! Recursive descent + Pratt parsing.
use std::fmt;
use crate::ast::*;
use crate::lexer::{split_radix, LexError, Lexer, Span, Token, TokenKind as T};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError { pub kind: ParseErrorKind, pub span: Span }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseErrorKind {
    Lex(LexError),
    Expected { expected: &'static str, found: T },
    IntOutOfRange,
    InvalidAssignTarget,
    WildcardBinding,
    TooDeep,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::Lex(e) => e.fmt(f),
            ParseErrorKind::Expected { expected, found } => write!(f, "expected {expected}, found {found}"),
            ParseErrorKind::IntOutOfRange => f.write_str("integer literal does not fit in u64"),
            ParseErrorKind::InvalidAssignTarget => f.write_str("invalid left-hand side of assignment"),
            ParseErrorKind::WildcardBinding => f.write_str("`_` cannot bind a value"),
            ParseErrorKind::TooDeep => f.write_str("nesting too deep"),
        }
    }
}

impl std::error::Error for ParseError {}

type PResult<R> = Result<R, ParseError>;

pub fn parse(src: &str) -> Result<Program<'_>, ParseError> { Parser::new(src).program() }

const MAX_DEPTH: u32 = 128;
const PREFIX_BP: u8 = 25;

enum Infix { Assign, CompoundAssign(BinOp), Binary(BinOp) }

fn infix(kind: T) -> Option<(u8, u8, Infix)> {
    use Infix::Binary;
    Some(match kind {
        T::Eq => (2, 1, Infix::Assign),
        T::PlusEq => (2, 1, Infix::CompoundAssign(BinOp::Add)),
        T::MinusEq => (2, 1, Infix::CompoundAssign(BinOp::Sub)),
        T::StarEq => (2, 1, Infix::CompoundAssign(BinOp::Mul)),
        T::SlashEq => (2, 1, Infix::CompoundAssign(BinOp::Div)),
        T::PercentEq => (2, 1, Infix::CompoundAssign(BinOp::Rem)),
        T::PipePipe => (3, 4, Binary(BinOp::Or)),
        T::AmpAmp => (5, 6, Binary(BinOp::And)),
        T::EqEq => (7, 8, Binary(BinOp::Eq)),
        T::BangEq => (7, 8, Binary(BinOp::Ne)),
        T::Lt => (9, 10, Binary(BinOp::Lt)),
        T::LtEq => (9, 10, Binary(BinOp::Le)),
        T::Gt => (9, 10, Binary(BinOp::Gt)),
        T::GtEq => (9, 10, Binary(BinOp::Ge)),
        T::Plus => (11, 12, Binary(BinOp::Add)),
        T::Minus => (11, 12, Binary(BinOp::Sub)),
        T::Star => (13, 14, Binary(BinOp::Mul)),
        T::Slash => (13, 14, Binary(BinOp::Div)),
        T::Percent => (13, 14, Binary(BinOp::Rem)),
        _ => return None,
    })
}

fn is_assign_target(e: &Expr<'_>) -> bool {
    matches!(e.kind, ExprKind::Ident(_) | ExprKind::Field { .. } | ExprKind::Index { .. } | ExprKind::Deref(_))
}

struct Parser<'src> {
    lexer: Lexer<'src>,
    tok: Token,
    depth: u32,
    no_struct_lit: bool,
}

impl<'src> Parser<'src> {
    fn new(src: &'src str) -> Self {
        let mut lexer = Lexer::new(src);
        let tok = lexer.next_token();
        Self { lexer, tok, depth: 0, no_struct_lit: false }
    }

    fn bump(&mut self) -> Token { std::mem::replace(&mut self.tok, self.lexer.next_token()) }

    fn eat(&mut self, kind: T) -> bool {
        let found = self.tok.kind == kind;
        if found { self.bump(); }
        found
    }

    fn expect(&mut self, kind: T, what: &'static str) -> PResult<Token> {
        if self.tok.kind == kind { Ok(self.bump()) } else { Err(self.unexpected(what)) }
    }

    fn ident(&mut self, what: &'static str) -> PResult<Ident<'src>> {
        let tok = self.expect(T::Ident, what)?;
        Ok(Ident { name: self.text(tok.span), span: tok.span })
    }

    fn text(&self, span: Span) -> &'src str { &self.lexer.source()[span.range()] }

    fn at_wildcard(&self) -> bool {
        self.tok.kind == T::Ident && self.text(self.tok.span) == "_"
    }

    fn unexpected(&self, expected: &'static str) -> ParseError {
        let kind = match self.tok.kind {
            T::Error(e) => ParseErrorKind::Lex(e),
            found => ParseErrorKind::Expected { expected, found },
        };
        ParseError { kind, span: self.tok.span }
    }

    fn nested<R>(&mut self, f: impl FnOnce(&mut Self) -> PResult<R>) -> PResult<R> {
        if self.depth == MAX_DEPTH {
            return Err(ParseError { kind: ParseErrorKind::TooDeep, span: self.tok.span });
        }
        self.depth += 1;
        let r = f(self);
        self.depth -= 1;
        r
    }

    fn program(mut self) -> PResult<Program<'src>> {
        let mut decls = Vec::new();
        while self.tok.kind != T::Eof { decls.push(self.decl()?); }
        Ok(Program { decls })
    }

    fn decl(&mut self) -> PResult<Decl<'src>> {
        match self.tok.kind {
            T::Let | T::Mut => self.let_decl().map(Decl::Let),
            T::Const => self.const_decl().map(Decl::Const),
            T::Fn | T::Extern => self.fn_decl().map(Decl::Fn),
            T::Struct => self.struct_decl().map(Decl::Struct),
            T::Enum => self.enum_decl().map(Decl::Enum),
            _ => Err(self.unexpected("a declaration")),
        }
    }

    fn let_decl(&mut self) -> PResult<LetDecl<'src>> {
        let start = self.tok.span;
        let mutable = self.eat(T::Mut);
        self.expect(T::Let, "`let`")?;
        let name = self.ident("variable name")?;
        let ty = if self.eat(T::Colon) { Some(self.ty()?) } else { None };
        self.expect(T::Eq, "`=`")?;
        let init = self.expr()?;
        let semi = self.expect(T::Semi, "`;`")?;
        Ok(LetDecl { mutable, name, ty, init, span: start.to(semi.span) })
    }

    fn const_decl(&mut self) -> PResult<ConstDecl<'src>> {
        let start = self.bump().span;
        let name = self.ident("constant name")?;
        self.expect(T::Colon, "`:`")?;
        let ty = self.ty()?;
        self.expect(T::Eq, "`=`")?;
        let value = self.expr()?;
        let semi = self.expect(T::Semi, "`;`")?;
        Ok(ConstDecl { name, ty, value, span: start.to(semi.span) })
    }

    fn fn_decl(&mut self) -> PResult<FnDecl<'src>> {
        let start = self.tok.span;
        let is_extern = self.eat(T::Extern);
        self.expect(T::Fn, "`fn`")?;
        let name = self.ident("function name")?;
        self.expect(T::LParen, "`(`")?;
        let mut params = Vec::new();
        while self.tok.kind != T::RParen {
            let pname = self.ident("parameter name")?;
            self.expect(T::Colon, "`:`")?;
            let ty = self.ty()?;
            params.push(Param { name: pname, ty });
            if !self.eat(T::Comma) { break; }
        }
        self.expect(T::RParen, "`,` or `)`")?;
        let ret = if self.eat(T::Arrow) { Some(self.ty()?) } else { None };
        if is_extern {
            let semi = self.expect(T::Semi, "`;`")?;
            let span = start.to(semi.span);
            let body = Block { stmts: Vec::new(), span: semi.span };
            return Ok(FnDecl { name, params, ret, is_extern, span, body });
        }
        let body = self.nested(Self::block)?;
        let span = start.to(body.span);
        Ok(FnDecl { name, params, ret, is_extern, span, body })
    }

    fn struct_decl(&mut self) -> PResult<StructDecl<'src>> {
        let start = self.bump().span;
        let name = self.ident("struct name")?;
        self.expect(T::LBrace, "`{`")?;
        let mut fields = Vec::new();
        while self.tok.kind != T::RBrace {
            let fname = self.ident("field name")?;
            self.expect(T::Colon, "`:`")?;
            let fty = self.ty()?;
            fields.push(FieldDecl { name: fname, ty: fty, span: fname.span });
            if !self.eat(T::Comma) { break; }
        }
        let close = self.expect(T::RBrace, "`}`")?;
        Ok(StructDecl { name, fields, span: start.to(close.span) })
    }

    fn enum_decl(&mut self) -> PResult<EnumDecl<'src>> {
        let start = self.bump().span;
        let name = self.ident("enum name")?;
        self.expect(T::LBrace, "`{`")?;
        let mut variants = Vec::new();
        while self.tok.kind != T::RBrace {
            let vname = self.ident("variant name")?;
            let mut payload = None;
            let mut end = vname.span;
            if self.eat(T::LParen) {
                let t = self.ty()?;
                let close = self.expect(T::RParen, "`)`")?;
                payload = Some(t);
                end = close.span;
            }
            variants.push(VariantDecl { name: vname, payload, span: vname.span.to(end) });
            if !self.eat(T::Comma) { break; }
        }
        let close = self.expect(T::RBrace, "`}`")?;
        Ok(EnumDecl { name, variants, span: start.to(close.span) })
    }

    fn ty(&mut self) -> PResult<Type<'src>> {
        match self.tok.kind {
            T::Amp => {
                self.bump();
                let mutable = self.eat(T::Mut);
                let inner = self.ty()?;
                Ok(Type::Ref { mutable, inner: Box::new(inner) })
            }
            T::LBracket => {
                self.bump();
                let element = self.ty()?;
                self.expect(T::Semi, "`;`")?;
                let len_tok = self.expect(T::Int, "array length")?;
                let size = match parse_int(self.text(len_tok.span)) {
                    Some(v) => v,
                    None => return Err(ParseError { kind: ParseErrorKind::IntOutOfRange, span: len_tok.span }),
                };
                self.expect(T::RBracket, "`]`")?;
                Ok(Type::Array { element: Box::new(element), size })
            }
            _ => self.ident("type name").map(Type::Named),
        }
    }

    fn block(&mut self) -> PResult<Block<'src>> {
        let open = self.expect(T::LBrace, "`{`")?;
        let mut stmts = Vec::new();
        while !matches!(self.tok.kind, T::RBrace | T::Eof) { stmts.push(self.stmt()?); }
        let close = self.expect(T::RBrace, "`}`")?;
        Ok(Block { stmts, span: open.span.to(close.span) })
    }

    fn stmt(&mut self) -> PResult<Stmt<'src>> {
        match self.tok.kind {
            T::Let | T::Mut | T::Const | T::Fn | T::Struct | T::Enum => self.decl().map(Stmt::Decl),
            T::Match => self.match_stmt(),
            T::For => self.for_stmt(),
            T::Return => {
                let start = self.bump().span;
                let value = if self.tok.kind == T::Semi { None } else { Some(self.expr()?) };
                let semi = self.expect(T::Semi, "`;`")?;
                Ok(Stmt::Return { value, span: start.to(semi.span) })
            }
            T::If => {
                let start = self.bump().span;
                self.no_struct_lit = true;
                let condition = self.expr()?;
                self.no_struct_lit = false;
                let then_block = self.nested(Self::block)?;
                let else_block = if self.eat(T::Else) {
                    if self.tok.kind == T::If {
                        let inner = self.nested(Self::stmt)?;
                        let span = match &inner { Stmt::If { span, .. } => *span, _ => unreachable!() };
                        Some(Block { stmts: vec![inner], span })
                    } else {
                        Some(self.nested(Self::block)?)
                    }
                } else {
                    None
                };
                let end = else_block.as_ref().map_or(then_block.span, |b| b.span);
                Ok(Stmt::If { condition, then_block, else_block, span: start.to(end) })
            }
            T::While => {
                let start = self.bump().span;
                self.no_struct_lit = true;
                let condition = self.expr()?;
                self.no_struct_lit = false;
                let body = self.nested(Self::block)?;
                let span = start.to(body.span);
                Ok(Stmt::While { condition, body, span })
            }
            T::Break => {
                let start = self.bump().span;
                let semi = self.expect(T::Semi, "`;`")?;
                Ok(Stmt::Break(start.to(semi.span)))
            }
            T::Continue => {
                let start = self.bump().span;
                let semi = self.expect(T::Semi, "`;`")?;
                Ok(Stmt::Continue(start.to(semi.span)))
            }
            _ => {
                let e = self.expr()?;
                self.expect(T::Semi, "`;`")?;
                Ok(Stmt::Expr(e))
            }
        }
    }

    fn for_stmt(&mut self) -> PResult<Stmt<'src>> {
        let start = self.bump().span;
        let binding = self.ident("loop variable")?;
        self.expect(T::In, "`in`")?;
        self.no_struct_lit = true;
        let iter = self.expr()?;
        self.no_struct_lit = false;
        let body = self.nested(Self::block)?;
        let span = start.to(body.span);
        Ok(Stmt::For { binding, iter, body, span })
    }

    fn match_stmt(&mut self) -> PResult<Stmt<'src>> {
        let start = self.bump().span;
        self.no_struct_lit = true;
        let scrutinee = self.expr()?;
        self.no_struct_lit = false;
        self.expect(T::LBrace, "`{`")?;
        let mut arms = Vec::new();
        while self.tok.kind != T::RBrace {
            let arm_start = self.tok.span;
            let (wildcard, enum_name, variant, binding) = if self.at_wildcard() {
                let tok = self.bump();
                let ph = Ident { name: "_", span: tok.span };
                if self.tok.kind == T::LParen {
                    return Err(ParseError { kind: ParseErrorKind::WildcardBinding, span: tok.span });
                }
                (true, ph, ph, None)
            } else {
                let enum_name = self.ident("enum name")?;
                self.expect(T::ColonColon, "`::`")?;
                let variant = self.ident("variant name")?;
                let binding = if self.eat(T::LParen) {
                    let b = self.ident("binding name")?;
                    self.expect(T::RParen, "`)`")?;
                    Some(b)
                } else {
                    None
                };
                (false, enum_name, variant, binding)
            };
            let guard = if self.eat(T::If) {
                self.no_struct_lit = true;
                let g = self.expr()?;
                self.no_struct_lit = false;
                Some(g)
            } else {
                None
            };
            self.expect(T::FatArrow, "`=>`")?;
            let body = self.nested(Self::block)?;
            arms.push(MatchArm {
                wildcard,
                enum_name,
                variant,
                binding,
                guard,
                span: arm_start.to(body.span),
                body,
            });
            self.eat(T::Comma);
        }
        let close = self.expect(T::RBrace, "`}`")?;
        Ok(Stmt::Match { scrutinee, arms, span: start.to(close.span) })
    }

    fn expr(&mut self) -> PResult<Expr<'src>> {
        let first = self.expr_bp(0)?;
        if !self.eat(T::DotDot) { return Ok(first); }
        // `lo..hi` binds looser than `||` but tighter than assignment.
        let end = self.expr_bp(3)?;
        let span = first.span.to(end.span);
        Ok(Expr { span, kind: ExprKind::Range { start: Box::new(first), end: Box::new(end) } })
    }

    fn expr_bp(&mut self, min_bp: u8) -> PResult<Expr<'src>> {
        self.nested(|p| {
            let mut lhs = p.prefix()?;
            loop {
                if p.tok.kind == T::LParen { lhs = p.call(lhs)?; continue; }
                if p.tok.kind == T::LBracket {
                    p.bump();
                    let saved = p.no_struct_lit;
                    p.no_struct_lit = false;
                    let index = p.expr()?;
                    p.no_struct_lit = saved;
                    let close = p.expect(T::RBracket, "`]`")?;
                    lhs = Expr {
                        span: lhs.span.to(close.span),
                        kind: ExprKind::Index { array: Box::new(lhs), index: Box::new(index) },
                    };
                    continue;
                }
                if p.tok.kind == T::Dot {
                    p.bump();
                    let field = p.ident("field name")?;
                    lhs = Expr {
                        span: lhs.span.to(field.span),
                        kind: ExprKind::Field { object: Box::new(lhs), field },
                    };
                    continue;
                }
                if p.tok.kind == T::Question {
                    p.bump();
                    lhs = Expr { span: lhs.span, kind: ExprKind::Try(Box::new(lhs)) };
                    continue;
                }
                if p.tok.kind == T::ColonColon {
                    let lspan = lhs.span;
                    let enum_name = match lhs.kind {
                        ExprKind::Ident(n) => Ident { name: n, span: lspan },
                        _ => return Err(ParseError {
                            kind: ParseErrorKind::Expected { expected: "enum name before `::`", found: T::ColonColon },
                            span: lspan,
                        }),
                    };
                    p.bump();
                    let variant = p.ident("variant name")?;
                    let mut payload = None;
                    let mut end = variant.span;
                    if p.tok.kind == T::LParen {
                        p.bump();
                        let e = p.expr()?;
                        let close = p.expect(T::RParen, "`)`")?;
                        payload = Some(Box::new(e));
                        end = close.span;
                    }
                    lhs = Expr { span: lspan.to(end), kind: ExprKind::EnumInit { enum_name, variant, payload } };
                    continue;
                }
                if p.tok.kind == T::As {
                    p.bump();
                    let ty = p.ty()?;
                    let span = lhs.span.to(ty.span());
                    lhs = Expr { span, kind: ExprKind::Cast { operand: Box::new(lhs), ty } };
                    continue;
                }
                if p.tok.kind == T::LBrace && !p.no_struct_lit && matches!(lhs.kind, ExprKind::Ident(_)) {
                    let name = match lhs.kind {
                        ExprKind::Ident(n) => Ident { name: n, span: lhs.span },
                        _ => unreachable!(),
                    };
                    p.bump();
                    let mut fields = Vec::new();
                    while p.tok.kind != T::RBrace {
                        let fname = p.ident("field name")?;
                        p.expect(T::Colon, "`:`")?;
                        let v = p.expr()?;
                        fields.push((fname, v));
                        if !p.eat(T::Comma) { break; }
                    }
                    let close = p.expect(T::RBrace, "`}`")?;
                    lhs = Expr { span: lhs.span.to(close.span), kind: ExprKind::StructInit { name, fields } };
                    continue;
                }
                let Some((l_bp, r_bp, op)) = infix(p.tok.kind) else { break };
                if l_bp < min_bp { break; }
                p.bump();
                let rhs = p.expr_bp(r_bp)?;
                let span = lhs.span.to(rhs.span);
                let kind = match op {
                    Infix::Binary(op) => ExprKind::Binary { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
                    Infix::Assign if is_assign_target(&lhs) => ExprKind::Assign {
                        target: Box::new(lhs),
                        value: Box::new(rhs),
                    },
                    Infix::Assign => return Err(ParseError {
                        kind: ParseErrorKind::InvalidAssignTarget,
                        span: lhs.span,
                    }),
                    Infix::CompoundAssign(op) if is_assign_target(&lhs) => {
                        let combined = Expr {
                            span,
                            kind: ExprKind::Binary { op, lhs: Box::new(lhs.clone()), rhs: Box::new(rhs) },
                        };
                        ExprKind::Assign { target: Box::new(lhs), value: Box::new(combined) }
                    }
                    Infix::CompoundAssign(_) => return Err(ParseError {
                        kind: ParseErrorKind::InvalidAssignTarget,
                        span: lhs.span,
                    }),
                };
                lhs = Expr { kind, span };
            }
            Ok(lhs)
        })
    }

    fn prefix(&mut self) -> PResult<Expr<'src>> {
        let tok = self.tok;
        let kind = match tok.kind {
            T::Int => match parse_int(self.text(tok.span)) {
                Some(v) => ExprKind::Int(v),
                None => return Err(ParseError { kind: ParseErrorKind::IntOutOfRange, span: tok.span }),
            },
            T::True => ExprKind::Bool(true),
            T::False => ExprKind::Bool(false),
            T::StringLit => {
                let t = self.text(tok.span);
                ExprKind::Str(&t[1..t.len() - 1])
            }
            T::CharLit => {
                let t = self.text(tok.span);
                let inner = &t[1..t.len() - 1];
                let c = match inner.as_bytes() {
                    [b'\\', b'n'] => '\n',
                    [b'\\', b't'] => '\t',
                    [b'\\', b'\\'] => '\\',
                    [b'\\', b'\''] => '\'',
                    [b'\\', b'"'] => '"',
                    [b'\\', b'0'] => '\0',
                    [b] => *b as char,
                    _ => return Err(ParseError {
                        kind: ParseErrorKind::Expected { expected: "valid char literal", found: tok.kind },
                        span: tok.span,
                    }),
                };
                ExprKind::Char(c)
            }
            T::Ident => ExprKind::Ident(self.text(tok.span)),
            T::Amp => {
                self.bump();
                let mutable = self.eat(T::Mut);
                let operand = self.expr_bp(PREFIX_BP)?;
                return Ok(Expr {
                    span: tok.span.to(operand.span),
                    kind: ExprKind::Borrow { mutable, operand: Box::new(operand) },
                });
            }
            T::Star => {
                self.bump();
                let operand = self.expr_bp(PREFIX_BP)?;
                return Ok(Expr { span: tok.span.to(operand.span), kind: ExprKind::Deref(Box::new(operand)) });
            }
            T::LBracket => {
                self.bump();
                let saved = self.no_struct_lit;
                self.no_struct_lit = false;
                let mut elements = Vec::new();
                while self.tok.kind != T::RBracket {
                    elements.push(self.expr()?);
                    if !self.eat(T::Comma) { break; }
                }
                self.no_struct_lit = saved;
                let close = self.expect(T::RBracket, "`]`")?;
                return Ok(Expr { span: tok.span.to(close.span), kind: ExprKind::ArrayInit { elements } });
            }
            T::Minus | T::Bang => {
                self.bump();
                let op = if tok.kind == T::Minus { UnOp::Neg } else { UnOp::Not };
                let operand = self.expr_bp(PREFIX_BP)?;
                return Ok(Expr {
                    span: tok.span.to(operand.span),
                    kind: ExprKind::Unary { op, operand: Box::new(operand) },
                });
            }
            T::LParen => {
                self.bump();
                let saved = self.no_struct_lit;
                self.no_struct_lit = false;
                let inner = self.expr()?;
                self.no_struct_lit = saved;
                self.expect(T::RParen, "`)`")?;
                return Ok(inner);
            }
            _ => return Err(self.unexpected("an expression")),
        };
        self.bump();
        Ok(Expr { kind, span: tok.span })
    }

    fn call(&mut self, callee: Expr<'src>) -> PResult<Expr<'src>> {
        self.bump();
        let saved = self.no_struct_lit;
        self.no_struct_lit = false;
        let mut args = Vec::new();
        while self.tok.kind != T::RParen {
            args.push(self.expr()?);
            if !self.eat(T::Comma) { break; }
        }
        self.no_struct_lit = saved;
        let close = self.expect(T::RParen, "`,` or `)`")?;
        Ok(Expr { span: callee.span.to(close.span), kind: ExprKind::Call { callee: Box::new(callee), args } })
    }
}

fn parse_int(lexeme: &str) -> Option<u64> {
    let (radix, digits) = split_radix(lexeme);
    let mut value = 0u64;
    for b in digits.bytes().filter(|&b| b != b'_') {
        let d = (b as char).to_digit(radix)?;
        value = value.checked_mul(u64::from(radix))?.checked_add(u64::from(d))?;
    }
    Some(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn program(src: &str) -> String { parse(src).unwrap().to_string() }
    fn expr(src: &str) -> String { Parser::new(src).expr().unwrap().to_string() }
    fn error(src: &str) -> String { parse(src).unwrap_err().to_string() }

    #[test]
    fn basics() {
        assert_eq!(program("let x = 10;"), "(let x 10)");
        assert_eq!(program("mut let y = 20;"), "(let mut y 20)");
        assert_eq!(program("const M: u32 = 1;"), "(const M: u32 1)");
        assert_eq!(program("fn nop() {}"), "(fn nop ())");
        assert_eq!(program("extern fn puts(s: str) -> i32;"), "(extern fn puts (s: str) -> i32)");
    }

    #[test]
    fn precedence() {
        assert_eq!(expr("1 + 2 * 3"), "(+ 1 (* 2 3))");
        assert_eq!(expr("a = b = 1"), "(= a (= b 1))");
        assert_eq!(expr("x as u32 * b"), "(* (as x u32) b)");
        assert_eq!(expr("&pts[i]"), "(& (index pts i))");
        assert_eq!(expr("*p = 5"), "(= (* p) 5)");
    }

    #[test]
    fn compound_assignment_desugars() {
        assert_eq!(expr("x += 1"), "(= x (+ x 1))");
        assert_eq!(expr("p.x += 1"), "(= (. p x) (+ (. p x) 1))");
        assert_eq!(expr("a[0] *= 2"), "(= (index a 0) (* (index a 0) 2))");
        assert_eq!(error("fn f() { 1 += 1; }"), "invalid left-hand side of assignment");
    }

    #[test]
    fn control_flow() {
        assert_eq!(
            program("fn f() { while a { break; continue; } }"),
            "(fn f ()\n  (while a\n    (break)\n    (continue)))"
        );
        assert_eq!(
            program("fn f() { if a { return 1; } else if b { return 2; } else { return 3; } }"),
            "(fn f ()\n  (if a\n    (return 1)\n  else\n    (if b\n      (return 2)\n    else\n      (return 3))))"
        );
    }

    #[test]
    fn for_loops() {
        assert_eq!(
            program("fn f() { for x in a { g(x); } }"),
            "(fn f ()\n  (for x a\n    (call g x)))"
        );
        assert_eq!(
            program("fn f() { for i in 0..10 { g(i); } }"),
            "(fn f ()\n  (for i (range 0 10)\n    (call g i)))"
        );
    }

    #[test]
    fn aggregates() {
        assert_eq!(expr("[1, 2]"), "(array 1 2)");
        assert_eq!(program("struct P { x: i32, }"), "(struct P (x: i32))");
        assert_eq!(expr("P { x: 1 }"), "(new P (x 1))");
        assert_eq!(expr("p.x"), "(. p x)");
    }

    #[test]
    fn enums_and_match() {
        assert_eq!(program("enum Opt { None, Some(i32) }"), "(enum Opt None Some(i32))");
        assert_eq!(expr("Opt::None"), "(variant Opt::None)");
        assert_eq!(expr("Opt::Some(5)"), "(variant Opt::Some 5)");
        assert_eq!(
            program("fn f() { match o { Opt::None => { return; } Opt::Some(v) => { return; } } }"),
            "(fn f ()\n  (match o\n    (arm Opt::None\n      (return))\n    (arm Opt::Some v\n      (return))))"
        );
        assert!(parse("fn f() { match o { Opt::None => { return; }, Opt::Some(v) => { return; }, } }").is_ok());
    }

    #[test]
    fn match_wildcard_and_guards() {
        assert_eq!(
            program("fn f() { match o { E::A(v) if v > 0 => { return; } _ => { return; } } }"),
            "(fn f ()\n  (match o\n    (arm E::A v (guard (> v 0))\n      (return))\n    (arm _\n      (return))))"
        );
        assert_eq!(error("fn f() { match o { _(v) => { return; } } }"), "`_` cannot bind a value");
    }

    #[test]
    fn try_operator() {
        assert_eq!(expr("x?"), "(try x)");
        assert_eq!(expr("f()?"), "(try (call f))");
        assert_eq!(expr("a.b?"), "(try (. a b))");
    }

    #[test]
    fn syntax_errors() {
        assert_eq!(error("let = 1;"), "expected variable name, found `=`");
        assert_eq!(error("fn f() { 1 = 2; }"), "invalid left-hand side of assignment");
        assert_eq!(error("extern fn f() -> i32 {}"), "expected `;`, found `{`");
        assert_eq!(error("fn f() { for x a { } }"), "expected `in`, found identifier");
    }

    #[test]
    fn deep_nesting_is_an_error_not_a_crash() {
        let deep = format!("let x = {}1{};", "(".repeat(400), ")".repeat(400));
        assert_eq!(error(&deep), "nesting too deep");
    }
}