//! Streaming lexer: bytes in, tokens out. Never allocates, never fails.
use std::fmt;
use std::iter::FusedIterator;
use std::ops::Range;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub const fn new(start: u32, end: u32) -> Self { Self { start, end } }
    pub fn to(self, other: Span) -> Span {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }
    pub fn range(self) -> Range<usize> { self.start as usize..self.end as usize }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LexError {
    UnexpectedChar,
    InvalidNumber,
    UnterminatedLiteral,
    UnterminatedComment,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::UnexpectedChar => "unexpected character",
            Self::InvalidNumber => "malformed number literal",
            Self::UnterminatedLiteral => "unterminated string or char literal",
            Self::UnterminatedComment => "unterminated block comment",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Ident, Int, StringLit, CharLit,
    Let, Mut, Const, Fn, Return, True, False,
    If, Else, While, For, In, Break, Continue,
    Struct, Enum, Match, Extern, As,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Colon, ColonColon, Semi, Arrow, FatArrow, Dot, DotDot, Question,
    Eq, EqEq, Bang, BangEq, Lt, LtEq, Gt, GtEq,
    Plus, Minus, Star, Slash, Percent, Amp, AmpAmp, PipePipe,
    PlusEq, MinusEq, StarEq, SlashEq, PercentEq,
    Eof, Error(LexError),
}

impl TokenKind {
    pub const fn describe(self) -> &'static str {
        match self {
            Self::Ident => "identifier",
            Self::Int => "integer literal",
            Self::StringLit => "string literal",
            Self::CharLit => "char literal",
            Self::Let => "`let`",
            Self::Mut => "`mut`",
            Self::Const => "`const`",
            Self::Fn => "`fn`",
            Self::Return => "`return`",
            Self::True => "`true`",
            Self::False => "`false`",
            Self::If => "`if`",
            Self::Else => "`else`",
            Self::While => "`while`",
            Self::For => "`for`",
            Self::In => "`in`",
            Self::Break => "`break`",
            Self::Continue => "`continue`",
            Self::Struct => "`struct`",
            Self::Enum => "`enum`",
            Self::Match => "`match`",
            Self::Extern => "`extern`",
            Self::As => "`as`",
            Self::LParen => "`(`",
            Self::RParen => "`)`",
            Self::LBrace => "`{`",
            Self::RBrace => "`}`",
            Self::LBracket => "`[`",
            Self::RBracket => "`]`",
            Self::Comma => "`,`",
            Self::Colon => "`:`",
            Self::ColonColon => "`::`",
            Self::Semi => "`;`",
            Self::Arrow => "`->`",
            Self::FatArrow => "`=>`",
            Self::Dot => "`.`",
            Self::DotDot => "`..`",
            Self::Question => "`?`",
            Self::Eq => "`=`",
            Self::EqEq => "`==`",
            Self::Bang => "`!`",
            Self::BangEq => "`!=`",
            Self::Lt => "`<`",
            Self::LtEq => "`<=`",
            Self::Gt => "`>`",
            Self::GtEq => "`>=`",
            Self::Plus => "`+`",
            Self::Minus => "`-`",
            Self::Star => "`*`",
            Self::Slash => "`/`",
            Self::Percent => "`%`",
            Self::PlusEq => "`+=`",
            Self::MinusEq => "`-=`",
            Self::StarEq => "`*=`",
            Self::SlashEq => "`/=`",
            Self::PercentEq => "`%=`",
            Self::Amp => "`&`",
            Self::AmpAmp => "`&&`",
            Self::PipePipe => "`||`",
            Self::Eof => "end of input",
            Self::Error(_) => "invalid token",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { f.write_str(self.describe()) }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'src> {
    src: &'src str,
    pos: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        assert!(u32::try_from(src.len()).is_ok(), "source larger than 4 GiB");
        Self { src, pos: 0 }
    }

    pub fn source(&self) -> &'src str { self.src }

    pub fn next_token(&mut self) -> Token {
        let start = loop {
            self.eat_while(|b| matches!(b, b' ' | b'\t' | b'\n' | b'\r'));
            match (self.peek(0), self.peek(1)) {
                (Some(b'/'), Some(b'/')) => self.eat_while(|b| b != b'\n'),
                (Some(b'/'), Some(b'*')) => {
                    let s = self.pos;
                    if !self.skip_block_comment() {
                        return self.token(TokenKind::Error(LexError::UnterminatedComment), s);
                    }
                }
                _ => break self.pos,
            }
        };
        let kind = match self.peek(0) {
            Some(first) => self.scan(first),
            None => TokenKind::Eof,
        };
        self.token(kind, start)
    }

    fn scan(&mut self, first: u8) -> TokenKind {
        use TokenKind::*;
        match first {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.ident_or_keyword(),
            b'0'..=b'9' => self.number(),
            b'"' => self.string_literal(),
            b'\'' => self.char_literal(),
            b'(' => self.single(LParen),
            b')' => self.single(RParen),
            b'{' => self.single(LBrace),
            b'}' => self.single(RBrace),
            b'[' => self.single(LBracket),
            b']' => self.single(RBracket),
            b',' => self.single(Comma),
            b':' => {
                if self.peek(1) == Some(b':') { self.pos += 2; ColonColon } else { self.pos += 1; Colon }
            }
            b';' => self.single(Semi),
            b'.' => {
                if self.peek(1) == Some(b'.') { self.pos += 2; DotDot } else { self.pos += 1; Dot }
            }
            b'?' => self.single(Question),
            b'+' => self.pair(b'=', PlusEq, Plus),
            b'*' => self.pair(b'=', StarEq, Star),
            b'/' => self.pair(b'=', SlashEq, Slash),
            b'%' => self.pair(b'=', PercentEq, Percent),
            b'-' => match self.peek(1) {
                Some(b'>') => { self.pos += 2; Arrow }
                Some(b'=') => { self.pos += 2; MinusEq }
                _ => self.single(Minus),
            },
            b'=' => {
                if self.peek(1) == Some(b'=') { self.pos += 2; EqEq }
                else if self.peek(1) == Some(b'>') { self.pos += 2; FatArrow }
                else { self.pos += 1; Eq }
            }
            b'!' => self.pair(b'=', BangEq, Bang),
            b'<' => self.pair(b'=', LtEq, Lt),
            b'>' => self.pair(b'=', GtEq, Gt),
            b'&' => {
                if self.peek(1) == Some(b'&') { self.pos += 2; AmpAmp } else { self.pos += 1; Amp }
            }
            b'|' if self.peek(1) == Some(b'|') => { self.pos += 2; PipePipe }
            _ => self.invalid_char(),
        }
    }

    fn ident_or_keyword(&mut self) -> TokenKind {
        let start = self.pos;
        self.eat_while(|b| b.is_ascii_alphanumeric() || b == b'_');
        match &self.src[start..self.pos] {
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "const" => TokenKind::Const,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "match" => TokenKind::Match,
            "extern" => TokenKind::Extern,
            "as" => TokenKind::As,
            _ => TokenKind::Ident,
        }
    }

    fn number(&mut self) -> TokenKind {
        let start = self.pos;
        self.eat_while(|b| b.is_ascii_alphanumeric() || b == b'_');
        if is_valid_int(&self.src[start..self.pos]) {
            TokenKind::Int
        } else {
            TokenKind::Error(LexError::InvalidNumber)
        }
    }

    fn string_literal(&mut self) -> TokenKind {
        self.pos += 1;
        loop {
            match self.peek(0) {
                None | Some(b'\n') => return TokenKind::Error(LexError::UnterminatedLiteral),
                Some(b'\\') => {
                    if self.peek(1).is_none() {
                        return TokenKind::Error(LexError::UnterminatedLiteral);
                    }
                    self.pos += 2;
                }
                Some(b'"') => { self.pos += 1; return TokenKind::StringLit; }
                _ => self.pos += 1,
            }
        }
    }

    fn char_literal(&mut self) -> TokenKind {
        self.pos += 1;
        let body_start = self.pos;
        match self.peek(0) {
            None | Some(b'\n') => return TokenKind::Error(LexError::UnterminatedLiteral),
            Some(b'\\') => self.pos += 2,
            Some(b'\'') => {}
            _ => self.pos += 1,
        }
        let len = self.pos - body_start;
        if len >= 1 && self.peek(0) == Some(b'\'') {
            self.pos += 1;
            TokenKind::CharLit
        } else {
            loop {
                match self.peek(0) {
                    None | Some(b'\n') => return TokenKind::Error(LexError::UnterminatedLiteral),
                    Some(b'\'') => { self.pos += 1; return TokenKind::Error(LexError::InvalidNumber); }
                    _ => self.pos += 1,
                }
            }
        }
    }

    fn invalid_char(&mut self) -> TokenKind {
        self.pos += 1;
        self.eat_while(|b| b & 0xC0 == 0x80);
        TokenKind::Error(LexError::UnexpectedChar)
    }

    fn skip_block_comment(&mut self) -> bool {
        let mut depth = 0u32;
        while let Some(b) = self.peek(0) {
            match (b, self.peek(1)) {
                (b'/', Some(b'*')) => { depth += 1; self.pos += 2; }
                (b'*', Some(b'/')) => {
                    depth -= 1;
                    self.pos += 2;
                    if depth == 0 { return true; }
                }
                _ => self.pos += 1,
            }
        }
        false
    }

    #[inline] fn bytes(&self) -> &'src [u8] { self.src.as_bytes() }
    #[inline] fn peek(&self, ahead: usize) -> Option<u8> { self.bytes().get(self.pos + ahead).copied() }
    #[inline] fn eat_while(&mut self, pred: impl Fn(u8) -> bool) {
        let rest = &self.bytes()[self.pos..];
        self.pos += rest.iter().position(|&b| !pred(b)).unwrap_or(rest.len());
    }
    #[inline] fn single(&mut self, kind: TokenKind) -> TokenKind { self.pos += 1; kind }
    #[inline] fn pair(&mut self, second: u8, two: TokenKind, one: TokenKind) -> TokenKind {
        if self.peek(1) == Some(second) { self.pos += 2; two } else { self.pos += 1; one }
    }
    #[inline] fn token(&self, kind: TokenKind, start: usize) -> Token {
        Token { kind, span: Span::new(start as u32, self.pos as u32) }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        match self.next_token() {
            Token { kind: TokenKind::Eof, .. } => None,
            t => Some(t),
        }
    }
}

impl FusedIterator for Lexer<'_> {}

pub fn split_radix(lexeme: &str) -> (u32, &str) {
    match lexeme.as_bytes() {
        [b'0', b'x', ..] => (16, &lexeme[2..]),
        [b'0', b'b', ..] => (2, &lexeme[2..]),
        [b'0', b'o', ..] => (8, &lexeme[2..]),
        _ => (10, lexeme),
    }
}

fn is_valid_int(lexeme: &str) -> bool {
    let (radix, digits) = split_radix(lexeme);
    let mut any = false;
    for b in digits.bytes() {
        match b {
            b'_' => {}
            _ if (b as char).is_digit(radix) => any = true,
            _ => return false,
        }
    }
    any
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    fn kinds(src: &str) -> Vec<TokenKind> { Lexer::new(src).map(|t| t.kind).collect() }

    #[test]
    fn declarations() {
        assert_eq!(kinds("let x = 10;"), [Let, Ident, Eq, Int, Semi]);
        assert_eq!(kinds("mut let y = 20;"), [Mut, Let, Ident, Eq, Int, Semi]);
        assert_eq!(kinds("const M: u32 = 1;"), [Const, Ident, Colon, Ident, Eq, Int, Semi]);
    }

    #[test]
    fn keywords_and_punctuation() {
        assert_eq!(kinds("if else while for in break continue"), [If, Else, While, For, In, Break, Continue]);
        assert_eq!(kinds("struct enum match extern as"), [Struct, Enum, Match, Extern, As]);
        assert_eq!(kinds(":: : => -> . .. ?"), [ColonColon, Colon, FatArrow, Arrow, Dot, DotDot, Question]);
        assert_eq!(kinds("[ ] & &&"), [LBracket, RBracket, Amp, AmpAmp]);
    }

    #[test]
    fn compound_assignment_operators() {
        assert_eq!(kinds("+= -= *= /= %="), [PlusEq, MinusEq, StarEq, SlashEq, PercentEq]);
        assert_eq!(kinds("+ - * / % ->"), [Plus, Minus, Star, Slash, Percent, Arrow]);
    }

    #[test]
    fn operators() {
        assert_eq!(
            kinds("== != <= >= < > = ! - + * / % ||"),
            [EqEq, BangEq, LtEq, GtEq, Lt, Gt, Eq, Bang, Minus, Plus, Star, Slash, Percent, PipePipe]
        );
    }

    #[test]
    fn numbers() {
        assert_eq!(kinds("0 42 1_000 0xFF 0b1010 0o17"), [Int; 6]);
        assert_eq!(kinds("0x"), [Error(LexError::InvalidNumber)]);
        assert_eq!(kinds("12abc"), [Error(LexError::InvalidNumber)]);
    }

    #[test]
    fn literals() {
        assert_eq!(kinds(r#""hello""#), [StringLit]);
        assert_eq!(kinds(r#"'a'"#), [CharLit]);
        assert_eq!(kinds(r#""he\"llo""#), [StringLit]);
        assert_eq!(kinds(r#"'\\'"#), [CharLit]);
        assert_eq!(kinds(r#""open"#), [Error(LexError::UnterminatedLiteral)]);
        assert_eq!(kinds(r#"'ab'"#), [Error(LexError::InvalidNumber)]);
    }

    #[test]
    fn comments_and_errors() {
        let src = "a // line\n/* nested /* x */ still */ b";
        assert_eq!(kinds(src), [Ident, Ident]);
        assert_eq!(kinds("@ x"), [Error(LexError::UnexpectedChar), Ident]);
        assert_eq!(kinds("/* open"), [Error(LexError::UnterminatedComment)]);
    }

    #[test]
    fn spans_and_eof() {
        let src = "ą = 1";
        let first = Lexer::new(src).next().unwrap();
        assert_eq!(first.span, Span::new(0, 2));
        assert_eq!(&src[first.span.range()], "ą");
        let mut lx = Lexer::new("x");
        assert_eq!(lx.next_token().kind, Ident);
        assert_eq!(lx.next_token().kind, Eof);
        assert_eq!(lx.next_token().kind, Eof);
    }
}