//! Ownership + borrows (NLL-lite) and reference escape analysis.
use std::collections::{HashMap, HashSet};
use std::fmt;
use crate::ast::*;
use crate::lexer::Span;
use crate::resolve::{DefId, DefKind, ResolvedProgram};
use crate::typeck::{CheckedProgram, Ty};

#[derive(Clone, Debug)]
pub struct OwnershipError { pub kind: OwnershipErrorKind, pub span: Span }

#[derive(Clone, Debug)]
pub enum OwnershipErrorKind {
    UseAfterMove { name: String },
    MoveOutOfPlace { place: &'static str },
    ConflictingBorrow { name: String },
    MoveWhileBorrowed { name: String },
    WriteThroughSharedRef,
    ReturnLocalReference { name: String },
    Unresolved,
}

impl fmt::Display for OwnershipError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OwnershipErrorKind::*;
        match &self.kind {
            UseAfterMove { name } => write!(f, "use of moved value `{name}`"),
            MoveOutOfPlace { place } => write!(f, "cannot move out of {place} (partial moves are not supported yet)"),
            ConflictingBorrow { name } => write!(f, "cannot borrow `{name}` twice: `&mut` requires exclusive access"),
            MoveWhileBorrowed { name } => write!(f, "cannot move or reassign `{name}` while it is borrowed"),
            WriteThroughSharedRef => f.write_str("cannot write through a shared reference `&` (use `&mut`)"),
            ReturnLocalReference { name } => write!(f, "cannot return a reference to local `{name}`: it would dangle"),
            Unresolved => f.write_str("internal error: unresolved identifier"),
        }
    }
}

impl std::error::Error for OwnershipError {}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Demand { Move, Read }

#[derive(Clone, Debug)]
struct Loan {
    owner: DefId,
    mutable: bool,
    depth: usize,
    temporary: bool,
    holder: Option<DefId>,
}

pub fn check_ownership(
    program: &Program<'_>,
    resolved: &ResolvedProgram<'_>,
    checked: &CheckedProgram<'_>,
) -> Result<(), OwnershipError> {
    let uses: HashMap<Span, DefId> = resolved.uses.iter().copied().collect();
    let def_at: HashMap<Span, DefId> = resolved.defs.iter().enumerate()
        .map(|(i, d)| (d.span, DefId::from_index(i))).collect();
    let mut globals = HashSet::new();
    for d in &program.decls {
        let span = match d {
            Decl::Let(l) => l.name.span,
            Decl::Const(c) => c.name.span,
            _ => continue,
        };
        if let Some(id) = def_at.get(&span) { globals.insert(*id); }
    }
    let mut own = Own {
        resolved, checked, uses, def_at, globals,
        moved: HashSet::new(), loans: Vec::new(),
        last_use: HashMap::new(), ref_origin: HashMap::new(),
        ret_is_ref: false, depth: 0, tick: 0,
    };
    for f in collect_fns(&program.decls) { own.check_fn(f)?; }
    Ok(())
}

fn collect_fns<'a, 'src>(decls: &'a [Decl<'src>]) -> Vec<&'a FnDecl<'src>> {
    let mut out = Vec::new();
    for d in decls {
        if let Decl::Fn(f) = d {
            out.push(f);
            out.extend(collect_fns_block(&f.body));
        }
    }
    out
}

fn collect_fns_block<'a, 'src>(b: &'a Block<'src>) -> Vec<&'a FnDecl<'src>> {
    let mut out = Vec::new();
    for s in &b.stmts {
        match s {
            Stmt::Decl(Decl::Fn(f)) => {
                out.push(f);
                out.extend(collect_fns_block(&f.body));
            }
            Stmt::Match { arms, .. } => {
                for a in arms { out.extend(collect_fns_block(&a.body)); }
            }
            Stmt::For { body, .. } => out.extend(collect_fns_block(body)),
            _ => {}
        }
    }
    out
}

struct Liveness<'a, 'src> {
    uses: &'a HashMap<Span, DefId>,
    def_at: &'a HashMap<Span, DefId>,
    last_use: HashMap<DefId, usize>,
    tick: usize,
    loops: Vec<Vec<DefId>>,
    _src: std::marker::PhantomData<&'src ()>,
}

impl<'a, 'src> Liveness<'a, 'src> {
    fn new(uses: &'a HashMap<Span, DefId>, def_at: &'a HashMap<Span, DefId>) -> Self {
        Self { uses, def_at, last_use: HashMap::new(), tick: 0, loops: Vec::new(), _src: std::marker::PhantomData }
    }

    fn touch(&mut self, id: DefId, t: usize) {
        let e = self.last_use.entry(id).or_insert(t);
        if *e < t { *e = t; }
        for l in &mut self.loops { l.push(id); }
    }

    fn block(&mut self, b: &Block<'src>) {
        for s in &b.stmts {
            self.tick += 1;
            let t = self.tick;
            self.stmt(s, t);
            if matches!(s, Stmt::Return { .. }) { break; }
        }
    }

    /// Loop body: uses inside extend to the loop's end (it may run again).
    fn loop_body(&mut self, b: &Block<'src>) {
        self.loops.push(Vec::new());
        self.block(b);
        let seen = self.loops.pop().unwrap_or_default();
        let end = self.tick;
        for id in seen { self.touch(id, end); }
    }

    fn stmt(&mut self, s: &Stmt<'src>, t: usize) {
        match s {
            Stmt::Decl(Decl::Let(d)) => {
                if let Some(id) = self.def_at.get(&d.name.span) { self.touch(*id, t); }
                self.expr(&d.init, t);
            }
            Stmt::Decl(Decl::Const(d)) => {
                if let Some(id) = self.def_at.get(&d.name.span) { self.touch(*id, t); }
                self.expr(&d.value, t);
            }
            Stmt::Decl(_) => {}
            Stmt::Return { value, .. } => if let Some(v) = value { self.expr(v, t); },
            Stmt::Expr(e) => self.expr(e, t),
            Stmt::If { condition, then_block, else_block, .. } => {
                self.expr(condition, t);
                self.block(then_block);
                if let Some(eb) = else_block { self.block(eb); }
            }
            Stmt::While { condition, body, .. } => {
                self.expr(condition, t);
                self.loop_body(body);
                self.expr(condition, self.tick);
            }
            Stmt::For { binding, iter, body, .. } => {
                self.expr(iter, t);
                if let Some(id) = self.def_at.get(&binding.span) { self.touch(*id, t); }
                self.loop_body(body);
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
            Stmt::Match { scrutinee, arms, .. } => {
                self.expr(scrutinee, t);
                for a in arms {
                    if let Some(b) = &a.binding {
                        if let Some(id) = self.def_at.get(&b.span) { self.touch(*id, t); }
                    }
                    if let Some(g) = &a.guard { self.expr(g, t); }
                    self.block(&a.body);
                }
            }
        }
    }

    fn expr(&mut self, e: &Expr<'src>, t: usize) {
        match &e.kind {
            ExprKind::Int(_) | ExprKind::Bool(_) | ExprKind::Str(_) | ExprKind::Char(_) => {}
            ExprKind::Ident(_) => if let Some(id) = self.uses.get(&e.span) { self.touch(*id, t); },
            ExprKind::Unary { operand, .. } => self.expr(operand, t),
            ExprKind::Binary { lhs, rhs, .. } => { self.expr(lhs, t); self.expr(rhs, t); }
            ExprKind::Assign { target, value } => { self.expr(target, t); self.expr(value, t); }
            ExprKind::Call { callee, args } => {
                self.expr(callee, t);
                for a in args { self.expr(a, t); }
            }
            ExprKind::ArrayInit { elements } => for x in elements { self.expr(x, t); },
            ExprKind::StructInit { fields, .. } => for (_, v) in fields { self.expr(v, t); },
            ExprKind::Index { array, index } => { self.expr(array, t); self.expr(index, t); }
            ExprKind::Field { object, .. } => self.expr(object, t),
            ExprKind::Borrow { operand, .. } => self.expr(operand, t),
            ExprKind::Deref(i) => self.expr(i, t),
            ExprKind::Cast { operand, .. } => self.expr(operand, t),
            ExprKind::EnumInit { payload, .. } => if let Some(p) = payload { self.expr(p, t); },
            ExprKind::Try(inner) => self.expr(inner, t),
            ExprKind::Range { start, end } => { self.expr(start, t); self.expr(end, t); }
        }
    }
}

struct Own<'a, 'src> {
    resolved: &'a ResolvedProgram<'src>,
    checked: &'a CheckedProgram<'src>,
    uses: HashMap<Span, DefId>,
    def_at: HashMap<Span, DefId>,
    globals: HashSet<DefId>,
    moved: HashSet<DefId>,
    loans: Vec<Loan>,
    last_use: HashMap<DefId, usize>,
    ref_origin: HashMap<DefId, DefId>,
    ret_is_ref: bool,
    depth: usize,
    tick: usize,
}

impl<'a, 'src> Own<'a, 'src> {
    fn check_fn(&mut self, f: &FnDecl<'src>) -> Result<(), OwnershipError> {
        let mut lv = Liveness::new(&self.uses, &self.def_at);
        lv.block(&f.body);
        self.last_use = lv.last_use;
        self.ret_is_ref = self.def_at.get(&f.name.span)
            .and_then(|id| self.checked.fn_sigs.get(id))
            .map(|s| matches!(s.ret, Ty::Ref(..)))
            .unwrap_or(false);
        self.ref_origin.clear();
        self.moved.clear();
        self.loans.clear();
        self.depth = 0;
        self.tick = 0;
        self.walk_block(&f.body)
    }

    fn walk_block(&mut self, b: &Block<'src>) -> Result<(), OwnershipError> {
        self.depth += 1;
        let block_depth = self.depth;
        for s in &b.stmts {
            self.tick += 1;
            let tick = self.tick;
            let mark = self.loans.len();
            self.walk_stmt(s)?;
            self.bind_loans(s, mark);
            self.loans.retain(|l| !l.temporary);
            let lu = &self.last_use;
            self.loans.retain(|l| match l.holder {
                Some(h) => lu.get(&h).copied().unwrap_or(usize::MAX) > tick,
                None => true,
            });
            if matches!(s, Stmt::Return { .. }) { break; }
        }
        self.loans.retain(|l| l.depth < block_depth);
        self.depth -= 1;
        Ok(())
    }

    fn bind_loans(&mut self, s: &Stmt<'src>, mark: usize) {
        let holder = match s {
            Stmt::Decl(Decl::Let(d)) => self.def_at.get(&d.name.span).copied()
                .filter(|id| matches!(self.checked.def_tys[id.index()], Ty::Ref(..))),
            Stmt::Expr(e) => match &e.kind {
                ExprKind::Assign { target, .. }
                    if matches!(self.checked.expr_tys.get(&target.span), Some(Ty::Ref(..))) =>
                {
                    self.uses.get(&target.span).copied()
                }
                _ => None,
            },
            _ => None,
        };
        if let Some(h) = holder {
            for l in &mut self.loans[mark..] { l.temporary = false; l.holder = Some(h); }
        }
    }

    fn walk_stmt(&mut self, s: &Stmt<'src>) -> Result<(), OwnershipError> {
        match s {
            Stmt::Decl(Decl::Let(d)) => {
                self.walk_expr(&d.init, Demand::Move)?;
                self.record_origin(d);
                Ok(())
            }
            Stmt::Decl(Decl::Const(d)) => self.walk_expr(&d.value, Demand::Move),
            Stmt::Decl(_) => Ok(()),
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.walk_expr(v, Demand::Move)?;
                    if self.ret_is_ref { self.check_escape(v)?; }
                }
                Ok(())
            }
            Stmt::Expr(e) => self.walk_expr(e, Demand::Read),
            Stmt::If { condition, then_block, else_block, .. } => {
                self.walk_expr(condition, Demand::Read)?;
                let snap = self.moved.clone();
                self.walk_block(then_block)?;
                let after_then = std::mem::replace(&mut self.moved, snap.clone());
                let mut after_else = snap.clone();
                if let Some(eb) = else_block {
                    self.moved = snap.clone();
                    self.walk_block(eb)?;
                    after_else = std::mem::replace(&mut self.moved, snap.clone());
                }
                self.moved = snap.union(&after_then).chain(&after_else).copied().collect();
                Ok(())
            }
            Stmt::While { condition, body, .. } => {
                self.walk_expr(condition, Demand::Read)?;
                let snap = self.moved.clone();
                self.walk_block(body)?;
                let after = self.moved.clone();
                self.moved = snap.union(&after).copied().collect();
                self.walk_expr(condition, Demand::Read)
            }
            Stmt::For { iter, body, .. } => {
                self.walk_expr(iter, Demand::Read)?;
                let snap = self.moved.clone();
                self.walk_block(body)?;
                let after = self.moved.clone();
                self.moved = snap.union(&after).copied().collect();
                Ok(())
            }
            Stmt::Break(_) | Stmt::Continue(_) => Ok(()),
            Stmt::Match { scrutinee, arms, .. } => {
                self.walk_expr(scrutinee, Demand::Read)?;
                let snap = self.moved.clone();
                let mut acc = snap.clone();
                for a in arms {
                    self.moved = snap.clone();
                    if let Some(g) = &a.guard { self.walk_expr(g, Demand::Read)?; }
                    self.walk_block(&a.body)?;
                    let after = std::mem::replace(&mut self.moved, snap.clone());
                    acc = acc.union(&after).copied().collect();
                }
                self.moved = acc;
                Ok(())
            }
        }
    }

    fn record_origin(&mut self, d: &LetDecl<'src>) {
        let Some(id) = self.def_at.get(&d.name.span).copied() else { return };
        if !matches!(self.checked.def_tys[id.index()], Ty::Ref(..)) { return; }
        if let Some(o) = self.origin_of(&d.init) { self.ref_origin.insert(id, o); }
    }

    fn origin_of(&self, e: &Expr<'src>) -> Option<DefId> {
        match &e.kind {
            ExprKind::Borrow { operand, .. } => self.place_root(operand),
            ExprKind::Ident(_) => {
                let id = self.uses.get(&e.span).copied()?;
                if matches!(self.checked.def_tys[id.index()], Ty::Ref(..)) {
                    Some(self.ref_origin.get(&id).copied().unwrap_or(id))
                } else { None }
            }
            _ => self.place_root(e),
        }
    }

    fn check_escape(&self, e: &Expr<'src>) -> Result<(), OwnershipError> {
        let Some(root) = self.origin_of(e) else { return Ok(()) };
        if self.globals.contains(&root) { return Ok(()); }
        let def = &self.resolved.defs[root.index()];
        if def.kind == DefKind::Constant { return Ok(()); }
        if def.kind == DefKind::Parameter && matches!(self.checked.def_tys[root.index()], Ty::Ref(..)) {
            return Ok(());
        }
        Err(OwnershipError {
            kind: OwnershipErrorKind::ReturnLocalReference { name: def.name.to_string() },
            span: e.span,
        })
    }

    fn walk_expr(&mut self, e: &Expr<'src>, demand: Demand) -> Result<(), OwnershipError> {
        match &e.kind {
            ExprKind::Int(_) | ExprKind::Bool(_) | ExprKind::Str(_) | ExprKind::Char(_) => Ok(()),
            ExprKind::Ident(_) => {
                let id = self.use_id(e.span)?;
                self.use_def(id, e.span, demand)
            }
            ExprKind::Unary { operand, .. } => self.walk_expr(operand, Demand::Read),
            ExprKind::Binary { lhs, rhs, .. } => {
                self.walk_expr(lhs, Demand::Read)?;
                self.walk_expr(rhs, Demand::Read)
            }
            ExprKind::Assign { target, value } => {
                self.walk_expr(value, Demand::Move)?;
                self.walk_target(target)
            }
            ExprKind::Call { callee, args } => {
                self.walk_expr(callee, Demand::Read)?;
                for a in args { self.walk_expr(a, Demand::Move)?; }
                Ok(())
            }
            ExprKind::ArrayInit { elements } => {
                for x in elements { self.walk_expr(x, Demand::Move)?; }
                Ok(())
            }
            ExprKind::StructInit { fields, .. } => {
                for (_, v) in fields { self.walk_expr(v, Demand::Move)?; }
                Ok(())
            }
            ExprKind::Index { array, index } => {
                self.walk_expr(index, Demand::Read)?;
                self.walk_expr(array, Demand::Read)?;
                if demand == Demand::Move && self.is_moveable(e.span) {
                    return Err(OwnershipError {
                        kind: OwnershipErrorKind::MoveOutOfPlace { place: "an index" },
                        span: e.span,
                    });
                }
                Ok(())
            }
            ExprKind::Field { object, .. } => {
                self.walk_expr(object, Demand::Read)?;
                if demand == Demand::Move && self.is_moveable(e.span) {
                    return Err(OwnershipError {
                        kind: OwnershipErrorKind::MoveOutOfPlace { place: "a field" },
                        span: e.span,
                    });
                }
                Ok(())
            }
            ExprKind::Borrow { mutable, operand } => {
                self.walk_expr(operand, Demand::Read)?;
                if let Some(root) = self.place_root(operand) { self.add_loan(root, *mutable, e.span)?; }
                Ok(())
            }
            ExprKind::Deref(i) => self.walk_expr(i, Demand::Read),
            ExprKind::Cast { operand, .. } => self.walk_expr(operand, Demand::Read),
            ExprKind::EnumInit { payload, .. } => {
                if let Some(p) = payload { self.walk_expr(p, Demand::Move)?; }
                Ok(())
            }
            ExprKind::Try(inner) => self.walk_expr(inner, Demand::Read),
            ExprKind::Range { start, end } => {
                self.walk_expr(start, Demand::Read)?;
                self.walk_expr(end, Demand::Read)
            }
        }
    }

    fn walk_target(&mut self, t: &Expr<'src>) -> Result<(), OwnershipError> {
        match &t.kind {
            ExprKind::Ident(_) => {
                let id = self.use_id(t.span)?;
                self.check_not_borrowed(id, t.span)?;
                self.moved.remove(&id);
                Ok(())
            }
            ExprKind::Index { array, .. } => self.walk_expr(array, Demand::Read),
            ExprKind::Field { object, .. } => self.walk_expr(object, Demand::Read),
            ExprKind::Deref(inner) => {
                self.walk_expr(inner, Demand::Read)?;
                match self.checked.expr_tys.get(&inner.span).cloned() {
                    Some(Ty::Ref(_, false)) => Err(OwnershipError {
                        kind: OwnershipErrorKind::WriteThroughSharedRef,
                        span: t.span,
                    }),
                    Some(Ty::Ref(_, true)) => {
                        if let Some(root) = self.place_root(inner) {
                            if self.loans.iter().filter(|l| l.owner == root).count() > 1 {
                                return Err(OwnershipError {
                                    kind: OwnershipErrorKind::ConflictingBorrow {
                                        name: self.resolved.defs[root.index()].name.to_string(),
                                    },
                                    span: t.span,
                                });
                            }
                        }
                        Ok(())
                    }
                    _ => Ok(()),
                }
            }
            _ => self.walk_expr(t, Demand::Read),
        }
    }

    fn place_root(&self, e: &Expr<'src>) -> Option<DefId> {
        match &e.kind {
            ExprKind::Ident(_) => self.uses.get(&e.span).copied(),
            ExprKind::Field { object, .. } => self.place_root(object),
            ExprKind::Index { array, .. } => self.place_root(array),
            ExprKind::Deref(i) => self.place_root(i),
            _ => None,
        }
    }

    fn add_loan(&mut self, owner: DefId, mutable: bool, span: Span) -> Result<(), OwnershipError> {
        if self.globals.contains(&owner) { return Ok(()); }
        if self.loans.iter().any(|l| l.owner == owner && (mutable || l.mutable)) {
            return Err(OwnershipError {
                kind: OwnershipErrorKind::ConflictingBorrow {
                    name: self.resolved.defs[owner.index()].name.to_string(),
                },
                span,
            });
        }
        self.loans.push(Loan { owner, mutable, depth: self.depth, temporary: true, holder: None });
        Ok(())
    }

    fn check_not_borrowed(&self, owner: DefId, span: Span) -> Result<(), OwnershipError> {
        if self.loans.iter().any(|l| l.owner == owner) {
            return Err(OwnershipError {
                kind: OwnershipErrorKind::MoveWhileBorrowed {
                    name: self.resolved.defs[owner.index()].name.to_string(),
                },
                span,
            });
        }
        Ok(())
    }

    fn use_def(&mut self, id: DefId, span: Span, demand: Demand) -> Result<(), OwnershipError> {
        let def = &self.resolved.defs[id.index()];
        if self.moved.contains(&id) {
            return Err(OwnershipError {
                kind: OwnershipErrorKind::UseAfterMove { name: def.name.to_string() },
                span,
            });
        }
        let shared = self.globals.contains(&id) || def.kind == DefKind::Constant;
        if demand == Demand::Move && !shared && self.is_moveable(span) {
            self.check_not_borrowed(id, span)?;
            self.moved.insert(id);
        }
        Ok(())
    }

    fn is_moveable(&self, span: Span) -> bool {
        matches!(self.checked.expr_tys.get(&span), Some(Ty::Array(_, _)) | Some(Ty::Struct(_)))
    }

    fn use_id(&self, span: Span) -> Result<DefId, OwnershipError> {
        self.uses.get(&span).copied().ok_or(OwnershipError { kind: OwnershipErrorKind::Unresolved, span })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::resolve::resolve;
    use crate::typeck::type_check;

    fn ok(src: &str) {
        let p = parse(src).unwrap();
        let r = resolve(&p).unwrap();
        let c = type_check(&p, &r).unwrap();
        check_ownership(&p, &r, &c).unwrap();
    }
    fn err(src: &str) -> OwnershipErrorKind {
        let p = parse(src).unwrap();
        let r = resolve(&p).unwrap();
        let c = type_check(&p, &r).unwrap();
        check_ownership(&p, &r, &c).unwrap_err().kind
    }

    #[test]
    fn moves() {
        assert!(matches!(
            err("fn f() { let a = [1, 2]; let b = a; let c = a[0]; }"),
            OwnershipErrorKind::UseAfterMove { .. }
        ));
        ok("fn f() { let a = 1; let b = a; let c = a + b; }");
        ok("struct P { x: i32 } fn f() { mut let a = P { x: 1 }; let b = a; a = P { x: 2 }; let c = a.x; }");
        assert!(matches!(
            err("fn f() { let a = [[1], [2]]; let b = a[0]; }"),
            OwnershipErrorKind::MoveOutOfPlace { .. }
        ));
    }

    #[test]
    fn borrows_and_nll() {
        ok("fn f() { let x = 1; let a = &x; let b = &x; let c = *a + *b; }");
        assert!(matches!(
            err("fn f() { mut let x = 1; let a = &x; let b = &mut x; let c = *a; }"),
            OwnershipErrorKind::ConflictingBorrow { .. }
        ));
        ok("fn f() { mut let x = 1; let r = &mut x; *r = 5; let s = &mut x; *s = 6; }");
        ok("fn bump(r: &mut i32) { *r = *r + 1; } fn f() { mut let x = 1; bump(&mut x); bump(&mut x); }");
        assert!(matches!(
            err("fn f() { let x = 1; let r = &x; *r = 2; }"),
            OwnershipErrorKind::WriteThroughSharedRef
        ));
    }

    #[test]
    fn escapes() {
        assert!(matches!(
            err("fn f() -> &i32 { let x = 1; return &x; }"),
            OwnershipErrorKind::ReturnLocalReference { .. }
        ));
        ok("fn id(r: &i32) -> &i32 { return r; }");
        ok("struct P { x: i32 } fn f(p: &P) -> &i32 { return &p.x; }");
        ok("let G = 1; fn f() -> &i32 { return &G; }");
    }

    #[test]
    fn enums_and_try() {
        ok("enum E { A, B(i32) } fn f() -> i32 { let e = E::B(1); let g = e; match g { E::A => { return 0; } E::B(v) => { return v; } } }");
        ok("enum Result { Err(i32), Ok(i32) } fn f() -> Result { let x = g()?; return Result::Ok(x); } fn g() -> Result { return Result::Ok(42); }");
    }

    #[test]
    fn for_loops_do_not_move_the_array() {
        ok("fn f() -> i32 { let a = [1, 2, 3]; mut let s = 0; for x in a { s += x; } return s + a[0]; }");
        ok("fn f() -> i32 { mut let s = 0; for i in 0..3 { s += i; } return s; }");
    }

    #[test]
    fn guards_read_bindings() {
        ok("enum E { A(i32) } fn f(e: E) -> i32 { match e { E::A(v) if v > 0 => { return v; } _ => { return 0; } } }");
    }

    #[test]
    fn borrow_of_array_element() {
        ok("struct P { x: i32 } fn get(p: &P) -> i32 { return p.x; } fn main() -> i32 { let pts = [P { x: 1 }, P { x: 2 }]; mut let s = 0; for i in 0..2 { s += get(&pts[i]); } return s; }");
    }
}