//! AST. Names borrow from the source; every node carries a Span.
use std::fmt::{self, Display, Formatter, Write as _};
use crate::lexer::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Program<'src> { pub decls: Vec<Decl<'src>> }

#[derive(Clone, Debug, PartialEq)]
pub enum Decl<'src> {
    Let(LetDecl<'src>),
    Const(ConstDecl<'src>),
    Fn(FnDecl<'src>),
    Struct(StructDecl<'src>),
    Enum(EnumDecl<'src>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ident<'src> { pub name: &'src str, pub span: Span }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<'src> {
    Named(Ident<'src>),
    Array { element: Box<Type<'src>>, size: u64 },
    Ref { mutable: bool, inner: Box<Type<'src>> },
}

impl<'src> Type<'src> {
    pub fn span(&self) -> Span {
        match self {
            Type::Named(id) => id.span,
            Type::Array { element, .. } => element.span(),
            Type::Ref { inner, .. } => inner.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetDecl<'src> {
    pub mutable: bool,
    pub name: Ident<'src>,
    pub ty: Option<Type<'src>>,
    pub init: Expr<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstDecl<'src> {
    pub name: Ident<'src>,
    pub ty: Type<'src>,
    pub value: Expr<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl<'src> {
    pub name: Ident<'src>,
    pub params: Vec<Param<'src>>,
    pub ret: Option<Type<'src>>,
    pub is_extern: bool,
    pub body: Block<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl<'src> {
    pub name: Ident<'src>,
    pub fields: Vec<FieldDecl<'src>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldDecl<'src> {
    pub name: Ident<'src>,
    pub ty: Type<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDecl<'src> {
    pub name: Ident<'src>,
    pub variants: Vec<VariantDecl<'src>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariantDecl<'src> {
    pub name: Ident<'src>,
    pub payload: Option<Type<'src>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm<'src> {
    pub wildcard: bool,
    pub enum_name: Ident<'src>,
    pub variant: Ident<'src>,
    pub binding: Option<Ident<'src>>,
    pub guard: Option<Expr<'src>>,
    pub body: Block<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param<'src> { pub name: Ident<'src>, pub ty: Type<'src> }

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'src> { pub stmts: Vec<Stmt<'src>>, pub span: Span }

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    Decl(Decl<'src>),
    Return { value: Option<Expr<'src>>, span: Span },
    Expr(Expr<'src>),
    If {
        condition: Expr<'src>,
        then_block: Block<'src>,
        else_block: Option<Block<'src>>,
        span: Span,
    },
    While { condition: Expr<'src>, body: Block<'src>, span: Span },
    For { binding: Ident<'src>, iter: Expr<'src>, body: Block<'src>, span: Span },
    Break(Span),
    Continue(Span),
    Match { scrutinee: Expr<'src>, arms: Vec<MatchArm<'src>>, span: Span },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'src> { pub kind: ExprKind<'src>, pub span: Span }

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'src> {
    Int(u64),
    Bool(bool),
    Str(&'src str),
    Char(char),
    Ident(&'src str),
    Unary { op: UnOp, operand: Box<Expr<'src>> },
    Binary { op: BinOp, lhs: Box<Expr<'src>>, rhs: Box<Expr<'src>> },
    Assign { target: Box<Expr<'src>>, value: Box<Expr<'src>> },
    Call { callee: Box<Expr<'src>>, args: Vec<Expr<'src>> },
    ArrayInit { elements: Vec<Expr<'src>> },
    Index { array: Box<Expr<'src>>, index: Box<Expr<'src>> },
    StructInit { name: Ident<'src>, fields: Vec<(Ident<'src>, Expr<'src>)> },
    Field { object: Box<Expr<'src>>, field: Ident<'src> },
    Borrow { mutable: bool, operand: Box<Expr<'src>> },
    Deref(Box<Expr<'src>>),
    Cast { operand: Box<Expr<'src>>, ty: Type<'src> },
    EnumInit {
        enum_name: Ident<'src>,
        variant: Ident<'src>,
        payload: Option<Box<Expr<'src>>>,
    },
    Try(Box<Expr<'src>>),
    Range { start: Box<Expr<'src>>, end: Box<Expr<'src>> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp { Neg, Not }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp { Add, Sub, Mul, Div, Rem, Eq, Ne, Lt, Le, Gt, Ge, And, Or }

impl UnOp {
    pub const fn symbol(self) -> &'static str {
        match self { Self::Neg => "-", Self::Not => "!" }
    }
}

impl BinOp {
    pub const fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+", Self::Sub => "-", Self::Mul => "*", Self::Div => "/",
            Self::Rem => "%", Self::Eq => "==", Self::Ne => "!=", Self::Lt => "<",
            Self::Le => "<=", Self::Gt => ">", Self::Ge => ">=", Self::And => "&&",
            Self::Or => "||",
        }
    }
}

impl Display for Program<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, d) in self.decls.iter().enumerate() {
            if i > 0 { f.write_char('\n')?; }
            write_decl(f, d, 0)?;
        }
        Ok(())
    }
}

impl Display for Decl<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { write_decl(f, self, 0) }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Named(id) => f.write_str(id.name),
            Type::Array { element, size } => write!(f, "[{element}; {size}]"),
            Type::Ref { mutable, inner } => {
                f.write_str(if *mutable { "&mut " } else { "&" })?;
                write!(f, "{inner}")
            }
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::Int(n) => write!(f, "{n}"),
            ExprKind::Bool(b) => write!(f, "{b}"),
            ExprKind::Str(s) => write!(f, "\"{s}\""),
            ExprKind::Char(c) => write!(f, "'{c}'"),
            ExprKind::Ident(n) => f.write_str(n),
            ExprKind::Unary { op, operand } => write!(f, "({} {operand})", op.symbol()),
            ExprKind::Binary { op, lhs, rhs } => write!(f, "({} {lhs} {rhs})", op.symbol()),
            ExprKind::Assign { target, value } => write!(f, "(= {target} {value})"),
            ExprKind::Call { callee, args } => {
                write!(f, "(call {callee}")?;
                for a in args { write!(f, " {a}")?; }
                f.write_char(')')
            }
            ExprKind::ArrayInit { elements } => {
                f.write_str("(array")?;
                for e in elements { write!(f, " {e}")?; }
                f.write_char(')')
            }
            ExprKind::Index { array, index } => write!(f, "(index {array} {index})"),
            ExprKind::StructInit { name, fields } => {
                write!(f, "(new {}", name.name)?;
                for (n, v) in fields { write!(f, " ({} {v})", n.name)?; }
                f.write_char(')')
            }
            ExprKind::Field { object, field } => write!(f, "(. {object} {})", field.name),
            ExprKind::Borrow { mutable, operand } => {
                write!(f, "({} {operand})", if *mutable { "&mut" } else { "&" })
            }
            ExprKind::Deref(e) => write!(f, "(* {e})"),
            ExprKind::Cast { operand, ty } => write!(f, "(as {operand} {ty})"),
            ExprKind::EnumInit { enum_name, variant, payload } => {
                write!(f, "(variant {}::{}", enum_name.name, variant.name)?;
                if let Some(p) = payload { write!(f, " {p}")?; }
                f.write_char(')')
            }
            ExprKind::Try(e) => write!(f, "(try {e})"),
            ExprKind::Range { start, end } => write!(f, "(range {start} {end})"),
        }
    }
}

fn newline(f: &mut Formatter<'_>, depth: usize) -> fmt::Result {
    write!(f, "\n{:width$}", "", width = depth * 2)
}

fn write_decl(f: &mut Formatter<'_>, decl: &Decl<'_>, depth: usize) -> fmt::Result {
    match decl {
        Decl::Let(d) => {
            f.write_str(if d.mutable { "(let mut " } else { "(let " })?;
            f.write_str(d.name.name)?;
            if let Some(t) = &d.ty { write!(f, ": {t}")?; }
            write!(f, " {})", d.init)
        }
        Decl::Const(d) => write!(f, "(const {}: {} {})", d.name.name, d.ty, d.value),
        Decl::Fn(d) => {
            write!(f, "({}fn {} (", if d.is_extern { "extern " } else { "" }, d.name.name)?;
            for (i, p) in d.params.iter().enumerate() {
                if i > 0 { f.write_str(", ")?; }
                write!(f, "{}: {}", p.name.name, p.ty)?;
            }
            f.write_char(')')?;
            if let Some(r) = &d.ret { write!(f, " -> {r}")?; }
            for s in &d.body.stmts {
                newline(f, depth + 1)?;
                write_stmt(f, s, depth + 1)?;
            }
            f.write_char(')')
        }
        Decl::Struct(d) => {
            write!(f, "(struct {} (", d.name.name)?;
            for (i, fl) in d.fields.iter().enumerate() {
                if i > 0 { f.write_str(", ")?; }
                write!(f, "{}: {}", fl.name.name, fl.ty)?;
            }
            f.write_str("))")
        }
        Decl::Enum(d) => {
            write!(f, "(enum {}", d.name.name)?;
            for v in &d.variants {
                write!(f, " {}", v.name.name)?;
                if let Some(t) = &v.payload { write!(f, "({t})")?; }
            }
            f.write_char(')')
        }
    }
}

fn write_stmt(f: &mut Formatter<'_>, stmt: &Stmt<'_>, depth: usize) -> fmt::Result {
    match stmt {
        Stmt::Decl(d) => write_decl(f, d, depth),
        Stmt::Return { value: Some(v), .. } => write!(f, "(return {v})"),
        Stmt::Return { value: None, .. } => f.write_str("(return)"),
        Stmt::Expr(e) => write!(f, "{e}"),
        Stmt::If { condition, then_block, else_block, .. } => {
            write!(f, "(if {condition}")?;
            for s in &then_block.stmts {
                newline(f, depth + 1)?;
                write_stmt(f, s, depth + 1)?;
            }
            if let Some(eb) = else_block {
                newline(f, depth)?;
                f.write_str("else")?;
                for s in &eb.stmts {
                    newline(f, depth + 1)?;
                    write_stmt(f, s, depth + 1)?;
                }
            }
            f.write_char(')')
        }
        Stmt::While { condition, body, .. } => {
            write!(f, "(while {condition}")?;
            for s in &body.stmts {
                newline(f, depth + 1)?;
                write_stmt(f, s, depth + 1)?;
            }
            f.write_char(')')
        }
        Stmt::For { binding, iter, body, .. } => {
            write!(f, "(for {} {iter}", binding.name)?;
            for s in &body.stmts {
                newline(f, depth + 1)?;
                write_stmt(f, s, depth + 1)?;
            }
            f.write_char(')')
        }
        Stmt::Break(_) => f.write_str("(break)"),
        Stmt::Continue(_) => f.write_str("(continue)"),
        Stmt::Match { scrutinee, arms, .. } => {
            write!(f, "(match {scrutinee}")?;
            for a in arms {
                newline(f, depth + 1)?;
                if a.wildcard {
                    f.write_str("(arm _")?;
                } else {
                    write!(f, "(arm {}::{}", a.enum_name.name, a.variant.name)?;
                }
                if let Some(b) = &a.binding { write!(f, " {}", b.name)?; }
                if let Some(g) = &a.guard { write!(f, " (guard {g})")?; }
                for s in &a.body.stmts {
                    newline(f, depth + 2)?;
                    write_stmt(f, s, depth + 2)?;
                }
                f.write_char(')')?;
            }
            f.write_char(')')
        }
    }
}
