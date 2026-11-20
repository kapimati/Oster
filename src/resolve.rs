//! Name resolution: every identifier gets a DefId.
use std::collections::HashMap;
use std::fmt;
use crate::ast::*;
use crate::lexer::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DefId(u32);

impl DefId {
    pub const fn index(self) -> usize { self.0 as usize }
    pub const fn from_index(i: usize) -> Self { Self(i as u32) }
}

#[derive(Clone, Debug)]
pub struct Def<'src> {
    pub kind: DefKind,
    pub name: &'src str,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefKind { Variable, Constant, Function, Parameter }

#[derive(Clone, Debug)]
pub struct ResolvedProgram<'src> {
    pub defs: Vec<Def<'src>>,
    pub uses: Vec<(Span, DefId)>,
}

#[derive(Clone, Debug)]
pub struct ResolveError { pub kind: ResolveErrorKind, pub span: Span }

#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    UndefinedVariable(String),
    AlreadyDeclared(String),
    MutableRequired(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ResolveErrorKind::*;
        match &self.kind {
            UndefinedVariable(n) => write!(f, "undefined variable `{n}`"),
            AlreadyDeclared(n) => write!(f, "variable `{n}` already declared in this scope"),
            MutableRequired(n) => write!(f, "cannot assign to immutable variable `{n}`"),
            BreakOutsideLoop => f.write_str("`break` outside of a loop"),
            ContinueOutsideLoop => f.write_str("`continue` outside of a loop"),
        }
    }
}

impl std::error::Error for ResolveError {}

pub fn resolve<'src>(p: &Program<'src>) -> Result<ResolvedProgram<'src>, ResolveError> {
    let mut r = Resolver::new();
    r.hoist_fns(&p.decls)?;
    for d in &p.decls { r.decl(d)?; }
    Ok(ResolvedProgram { defs: r.defs, uses: r.uses })
}

struct Resolver<'src> {
    defs: Vec<Def<'src>>,
    uses: Vec<(Span, DefId)>,
    scopes: Vec<HashMap<&'src str, DefId>>,
    loop_depth: u32,
}

impl<'src> Resolver<'src> {
    fn new() -> Self {
        Self { defs: Vec::new(), uses: Vec::new(), scopes: vec![HashMap::new()], loop_depth: 0 }
    }

    fn define(&mut self, name: &'src str, kind: DefKind, mutable: bool, span: Span) -> Result<DefId, ResolveError> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(name) {
            return Err(ResolveError { kind: ResolveErrorKind::AlreadyDeclared(name.to_string()), span });
        }
        let id = DefId::from_index(self.defs.len());
        self.defs.push(Def { kind, name, mutable, span });
        scope.insert(name, id);
        Ok(id)
    }

    /// Pre-declares every `fn` in the current scope so call order doesn't matter.
    fn hoist_fns(&mut self, decls: &[Decl<'src>]) -> Result<(), ResolveError> {
        for d in decls {
            if let Decl::Fn(f) = d {
                self.define(f.name.name, DefKind::Function, false, f.name.span)?;
            }
        }
        Ok(())
    }

    fn lookup(&self, name: &str) -> Option<DefId> {
        for s in self.scopes.iter().rev() {
            if let Some(&id) = s.get(name) { return Some(id); }
        }
        None
    }

    fn use_ident(&mut self, name: &'src str, span: Span) -> Result<DefId, ResolveError> {
        match self.lookup(name) {
            Some(id) => { self.uses.push((span, id)); Ok(id) }
            None => Err(ResolveError { kind: ResolveErrorKind::UndefinedVariable(name.to_string()), span }),
        }
    }

    fn decl(&mut self, d: &Decl<'src>) -> Result<(), ResolveError> {
        match d {
            Decl::Let(l) => {
                self.expr(&l.init)?;
                self.define(l.name.name, DefKind::Variable, l.mutable, l.name.span)?;
                Ok(())
            }
            Decl::Const(c) => {
                self.expr(&c.value)?;
                self.define(c.name.name, DefKind::Constant, false, c.name.span)?;
                Ok(())
            }
            Decl::Fn(f) => {
                // Name already bound by the enclosing hoist_fns() pass.
                self.scopes.push(HashMap::new());
                let saved = self.loop_depth;
                self.loop_depth = 0;
                for p in &f.params {
                    self.define(p.name.name, DefKind::Parameter, false, p.name.span)?;
                }
                self.block(&f.body)?;
                self.loop_depth = saved;
                self.scopes.pop();
                Ok(())
            }
            Decl::Struct(_) | Decl::Enum(_) => Ok(()),
        }
    }

    fn block(&mut self, b: &Block<'src>) -> Result<(), ResolveError> {
        self.scopes.push(HashMap::new());
        let nested_fns: Vec<Decl<'src>> = b.stmts.iter()
            .filter_map(|s| match s {
                Stmt::Decl(d @ Decl::Fn(_)) => Some(d.clone()),
                _ => None,
            })
            .collect();
        let result = (|| {
            self.hoist_fns(&nested_fns)?;
            for s in &b.stmts { self.stmt(s)?; }
            Ok(())
        })();
        self.scopes.pop();
        result
    }

    /// Loop body: its own scope, with `break`/`continue` enabled.
    fn loop_body(&mut self, b: &Block<'src>) -> Result<(), ResolveError> {
        self.loop_depth += 1;
        let r = self.block(b);
        self.loop_depth -= 1;
        r
    }

    fn stmt(&mut self, s: &Stmt<'src>) -> Result<(), ResolveError> {
        match s {
            Stmt::Decl(d) => self.decl(d),
            Stmt::Return { value, .. } => {
                if let Some(e) = value { self.expr(e)?; }
                Ok(())
            }
            Stmt::Expr(e) => self.expr(e),
            Stmt::If { condition, then_block, else_block, .. } => {
                self.expr(condition)?;
                self.block(then_block)?;
                if let Some(eb) = else_block { self.block(eb)?; }
                Ok(())
            }
            Stmt::While { condition, body, .. } => {
                self.expr(condition)?;
                self.loop_body(body)
            }
            Stmt::For { binding, iter, body, .. } => {
                self.expr(iter)?;
                self.scopes.push(HashMap::new());
                let r = (|| {
                    self.define(binding.name, DefKind::Variable, false, binding.span)?;
                    self.loop_body(body)
                })();
                self.scopes.pop();
                r
            }
            Stmt::Break(span) => {
                if self.loop_depth == 0 {
                    return Err(ResolveError { kind: ResolveErrorKind::BreakOutsideLoop, span: *span });
                }
                Ok(())
            }
            Stmt::Continue(span) => {
                if self.loop_depth == 0 {
                    return Err(ResolveError { kind: ResolveErrorKind::ContinueOutsideLoop, span: *span });
                }
                Ok(())
            }
            Stmt::Match { scrutinee, arms, .. } => {
                self.expr(scrutinee)?;
                for a in arms {
                    self.scopes.push(HashMap::new());
                    let r = (|| {
                        if let Some(b) = &a.binding {
                            self.define(b.name, DefKind::Variable, false, b.span)?;
                        }
                        // The guard sees the arm's binding.
                        if let Some(g) = &a.guard { self.expr(g)?; }
                        self.block(&a.body)
                    })();
                    self.scopes.pop();
                    r?;
                }
                Ok(())
            }
        }
    }

    fn target(&mut self, e: &Expr<'src>) -> Result<(), ResolveError> {
        match &e.kind {
            ExprKind::Ident(name) => {
                let id = self.lookup(name).ok_or_else(|| ResolveError {
                    kind: ResolveErrorKind::UndefinedVariable(name.to_string()),
                    span: e.span,
                })?;
                if !self.defs[id.index()].mutable {
                    return Err(ResolveError {
                        kind: ResolveErrorKind::MutableRequired(name.to_string()),
                        span: e.span,
                    });
                }
                self.uses.push((e.span, id));
                Ok(())
            }
            ExprKind::Field { object, .. } => self.target(object),
            ExprKind::Index { array, .. } => self.target(array),
            ExprKind::Deref(inner) => self.expr(inner),
            _ => self.expr(e),
        }
    }

    fn expr(&mut self, e: &Expr<'src>) -> Result<(), ResolveError> {
        match &e.kind {
            ExprKind::Int(_) | ExprKind::Bool(_) | ExprKind::Str(_) | ExprKind::Char(_) => Ok(()),
            ExprKind::Ident(n) => { self.use_ident(n, e.span)?; Ok(()) }
            ExprKind::Unary { operand, .. } => self.expr(operand),
            ExprKind::Binary { lhs, rhs, .. } => { self.expr(lhs)?; self.expr(rhs) }
            ExprKind::Assign { target, value } => { self.target(target)?; self.expr(value) }
            ExprKind::Call { callee, args } => {
                self.expr(callee)?;
                for a in args { self.expr(a)?; }
                Ok(())
            }
            ExprKind::ArrayInit { elements } => {
                for x in elements { self.expr(x)?; }
                Ok(())
            }
            ExprKind::Index { array, index } => { self.expr(array)?; self.expr(index) }
            ExprKind::StructInit { fields, .. } => {
                for (_, v) in fields { self.expr(v)?; }
                Ok(())
            }
            ExprKind::Field { object, .. } => self.expr(object),
            ExprKind::Borrow { operand, .. } => self.expr(operand),
            ExprKind::Deref(inner) => self.expr(inner),
            ExprKind::Cast { operand, .. } => self.expr(operand),
            ExprKind::EnumInit { payload, .. } => {
                if let Some(p) = payload { self.expr(p)?; }
                Ok(())
            }
            ExprKind::Try(inner) => self.expr(inner),
            ExprKind::Range { start, end } => { self.expr(start)?; self.expr(end) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    fn ok(src: &str) -> ResolvedProgram<'_> {
        let a = parser::parse(src).unwrap();
        resolve(&a).unwrap()
    }
    fn err(src: &str) -> ResolveErrorKind {
        let a = parser::parse(src).unwrap();
        resolve(&a).unwrap_err().kind
    }

    #[test]
    fn basics() {
        let r = ok("let x = 1; fn f() { return x; }");
        assert_eq!(r.defs.len(), 2);
        assert!(matches!(err("fn f() { return y; }"), ResolveErrorKind::UndefinedVariable(_)));
        assert!(matches!(err("fn f() { let x = 1; let x = 2; }"), ResolveErrorKind::AlreadyDeclared(_)));
    }

    #[test]
    fn forward_references() {
        ok("fn f() -> i32 { return g(); } fn g() -> i32 { return 1; }");
        ok("fn f() -> i32 { fn a() -> i32 { return b(); } fn b() -> i32 { return 2; } return a(); }");
    }

    #[test]
    fn mutation_rules() {
        assert!(matches!(err("fn f() { let x = 1; x = 2; }"), ResolveErrorKind::MutableRequired(_)));
        assert!(matches!(err("fn f() { let a = [1]; a[0] = 2; }"), ResolveErrorKind::MutableRequired(_)));
        ok("fn f() { mut let a = [1]; a[0] = 2; }");
        ok("fn f(r: &mut i32) { *r = 5; }");
    }

    #[test]
    fn loops_and_scopes() {
        assert!(matches!(err("fn f() { break; }"), ResolveErrorKind::BreakOutsideLoop));
        ok("fn f() { while true { break; continue; } }");
        let r = ok("fn f() { let x = 1; fn g() { let y = 2; } }");
        assert_eq!(r.defs.len(), 4);
    }

    #[test]
    fn for_binding_is_scoped() {
        ok("fn g(x: i32) { } fn f() { for i in 0..3 { g(i); } }");
        ok("fn g(x: i32) { } fn f() { let a = [1, 2]; for x in a { g(x); } }");
        assert!(matches!(
            err("fn f() { for i in 0..3 { } return i; }"),
            ResolveErrorKind::UndefinedVariable(_)
        ));
        ok("fn f() { for i in 0..3 { break; continue; } }");
        assert!(matches!(err("fn f() { for i in 0..3 { } break; }"), ResolveErrorKind::BreakOutsideLoop));
    }

    #[test]
    fn match_binding_and_guard_are_scoped() {
        ok("enum E { A, B(i32) } fn f(e: E) -> i32 { match e { E::A => { return 0; } E::B(v) => { return v; } } }");
        ok("enum E { A(i32) } fn f(e: E) -> i32 { match e { E::A(v) if v > 0 => { return v; } _ => { return 0; } } }");
        assert!(matches!(
            err("enum E { A(i32) } fn f(e: E) -> i32 { match e { E::A(v) => { return v; } } return v; }"),
            ResolveErrorKind::UndefinedVariable(_)
        ));
    }
}