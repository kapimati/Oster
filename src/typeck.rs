//! Type checking: literals, arrays, structs, enums, references, casts, try, for.
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use crate::ast::*;
use crate::lexer::Span;
use crate::resolve::{DefId, ResolvedProgram};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    Bool, Str, Char, Unit,
    Array(Box<Ty>, u64),
    Struct(String),
    Enum(String),
    Ref(Box<Ty>, bool),
    Unknown(String),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::I8 => f.write_str("i8"), Ty::I16 => f.write_str("i16"),
            Ty::I32 => f.write_str("i32"), Ty::I64 => f.write_str("i64"),
            Ty::U8 => f.write_str("u8"), Ty::U16 => f.write_str("u16"),
            Ty::U32 => f.write_str("u32"), Ty::U64 => f.write_str("u64"),
            Ty::Bool => f.write_str("bool"), Ty::Str => f.write_str("str"),
            Ty::Char => f.write_str("char"), Ty::Unit => f.write_str("()"),
            Ty::Array(e, n) => write!(f, "[{e}; {n}]"),
            Ty::Struct(n) | Ty::Enum(n) => f.write_str(n),
            Ty::Ref(t, m) => write!(f, "&{}{t}", if *m { "mut " } else { "" }),
            Ty::Unknown(n) => write!(f, "?{n}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FnSig { pub params: Vec<Ty>, pub ret: Ty }

#[derive(Clone, Debug)]
pub struct TryInfo {
    pub enum_name: String,
    pub err_variant: String,
    pub ok_variant: String,
}

#[derive(Clone, Debug)]
pub struct CheckedProgram<'src> {
    pub def_tys: Vec<Ty>,
    pub expr_tys: HashMap<Span, Ty>,
    pub fn_sigs: HashMap<DefId, FnSig>,
    pub struct_fields: HashMap<String, Vec<(String, Ty)>>,
    pub enum_variants: HashMap<String, Vec<(String, Option<Ty>)>>,
    pub try_info: HashMap<Span, TryInfo>,
    _phantom: PhantomData<&'src ()>,
}

#[derive(Clone, Debug)]
pub struct TypeError { pub kind: TypeErrorKind, pub span: Span }

#[derive(Clone, Debug)]
pub enum TypeErrorKind {
    TypeMismatch { expected: Ty, found: Ty },
    ReturnTypeMismatch { expected: Ty, found: Ty },
    LiteralOutOfRange { value: u64, ty: Ty },
    CannotNegate { ty: Ty },
    ArgCount { expected: usize, found: usize },
    DuplicateDefinition(String),
    UnknownStruct(String),
    MissingField(String),
    UnexpectedField(String),
    DuplicateField(String),
    NotAStruct(Ty),
    NotAnArray(Ty),
    IndexNotInteger(Ty),
    IndexOutOfRange { index: u64, len: u64 },
    Unassignable(Ty),
    NotAReference(Ty),
    BorrowOfTemporary,
    ImmutableBorrow,
    InvalidCast { from: Ty, to: Ty },
    UnknownEnum(String),
    UnknownVariant(String),
    NonExhaustiveMatch { missing: String },
    DuplicateArm(String),
    ArmBinding(String),
    NonCopyPayload(String),
    NotAnEnum(Ty),
    TryRequiresTwoVariants { found: usize },
    TryReturnTypeMismatch { expected: Ty, found: Ty },
    NotIterable(Ty),
    ForElementNotCopy(Ty),
    RangeBounds { start: Ty, end: Ty },
    RangeOutsideFor,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeErrorKind::*;
        match &self.kind {
            TypeMismatch { expected, found } => write!(f, "type mismatch: expected {expected}, found {found}"),
            ReturnTypeMismatch { expected, found } => write!(f, "return type mismatch: expected {expected}, found {found}"),
            LiteralOutOfRange { value, ty } => write!(f, "integer literal `{value}` does not fit in `{ty}`"),
            CannotNegate { ty } => write!(f, "cannot apply unary `-` to type `{ty}`"),
            ArgCount { expected, found } => write!(f, "expected {expected} argument(s), found {found}"),
            DuplicateDefinition(n) => write!(f, "duplicate definition of `{n}`"),
            UnknownStruct(n) => write!(f, "unknown struct `{n}`"),
            MissingField(n) => write!(f, "missing field `{n}` in initializer"),
            UnexpectedField(n) => write!(f, "struct has no field `{n}`"),
            DuplicateField(n) => write!(f, "field `{n}` initialized twice"),
            NotAStruct(t) => write!(f, "type `{t}` has no fields"),
            NotAnArray(t) => write!(f, "type `{t}` cannot be indexed"),
            IndexNotInteger(t) => write!(f, "index must be an integer, found `{t}`"),
            IndexOutOfRange { index, len } => write!(f, "index {index} out of range for length {len}"),
            Unassignable(t) => write!(f, "cannot assign to value of type `{t}`"),
            NotAReference(t) => write!(f, "type `{t}` is not a reference"),
            BorrowOfTemporary => f.write_str("cannot borrow a temporary value (bind it with `let` first)"),
            ImmutableBorrow => f.write_str("cannot borrow an immutable binding as `&mut`"),
            InvalidCast { from, to } => write!(f, "invalid cast from `{from}` to `{to}`"),
            UnknownEnum(n) => write!(f, "unknown enum `{n}`"),
            UnknownVariant(n) => write!(f, "unknown variant `{n}`"),
            NonExhaustiveMatch { missing } => write!(f, "match is not exhaustive: missing `{missing}` (add an arm or `_`)"),
            DuplicateArm(n) => write!(f, "variant `{n}` matched twice"),
            ArmBinding(n) => write!(f, "wrong binding for `{n}`: payload needs `(name)`, unit variant needs none"),
            NonCopyPayload(n) => write!(f, "enum payload `{n}` must be a copy type (scalar, char, str or reference)"),
            NotAnEnum(t) => write!(f, "type `{t}` is not an enum"),
            TryRequiresTwoVariants { found } => write!(f, "`?` requires an enum with exactly 2 variants, found {found}"),
            TryReturnTypeMismatch { expected, found } => write!(f, "`?` needs the function to return `{expected}`, found `{found}`"),
            NotIterable(t) => write!(f, "cannot iterate over `{t}`: expected an array or `lo..hi`"),
            ForElementNotCopy(t) => write!(f, "`for` by value needs a copy element type, `{t}` would move out of the array"),
            RangeBounds { start, end } => write!(f, "range bounds must be matching integers, found `{start}` and `{end}`"),
            RangeOutsideFor => f.write_str("`lo..hi` is only allowed as a `for` iterator"),
        }
    }
}

impl std::error::Error for TypeError {}

pub fn type_check<'src>(
    program: &Program<'src>,
    resolved: &ResolvedProgram<'src>,
) -> Result<CheckedProgram<'src>, TypeError> {
    let mut ck = Checker::new(resolved);
    ck.collect_types(&program.decls)?;
    ck.finish_types()?;
    ck.collect_sigs(&program.decls);
    for d in &program.decls { ck.decl(d)?; }
    Ok(CheckedProgram {
        def_tys: ck.def_tys,
        expr_tys: ck.expr_tys,
        fn_sigs: ck.fn_sigs,
        struct_fields: ck.struct_fields,
        enum_variants: ck.enum_variants,
        try_info: ck.try_info,
        _phantom: PhantomData,
    })
}

struct Checker<'src> {
    resolved: &'src ResolvedProgram<'src>,
    def_tys: Vec<Ty>,
    expr_tys: HashMap<Span, Ty>,
    fn_sigs: HashMap<DefId, FnSig>,
    struct_names: Vec<String>,
    enum_names: Vec<String>,
    struct_raw: Vec<(String, Vec<(String, Type<'src>)>)>,
    enum_raw: Vec<(String, Vec<(String, Option<Type<'src>>, Span)>)>,
    struct_fields: HashMap<String, Vec<(String, Ty)>>,
    enum_variants: HashMap<String, Vec<(String, Option<Ty>)>>,
    try_info: HashMap<Span, TryInfo>,
    current_ret_ty: Option<Ty>,
}

impl<'src> Checker<'src> {
    fn new(resolved: &'src ResolvedProgram<'src>) -> Self {
        Self {
            resolved,
            def_tys: vec![Ty::Unknown(String::new()); resolved.defs.len()],
            expr_tys: HashMap::new(),
            fn_sigs: HashMap::new(),
            struct_names: Vec::new(),
            enum_names: Vec::new(),
            struct_raw: Vec::new(),
            enum_raw: Vec::new(),
            struct_fields: HashMap::new(),
            enum_variants: HashMap::new(),
            try_info: HashMap::new(),
            current_ret_ty: None,
        }
    }

    fn prim(name: &str) -> Option<Ty> {
        Some(match name {
            "i8" => Ty::I8, "i16" => Ty::I16, "i32" => Ty::I32, "i64" => Ty::I64,
            "u8" => Ty::U8, "u16" => Ty::U16, "u32" => Ty::U32, "u64" => Ty::U64,
            "bool" => Ty::Bool, "str" => Ty::Str, "char" => Ty::Char, "()" => Ty::Unit,
            _ => return None,
        })
    }

    fn ty_of(&self, t: &Type<'src>) -> Ty {
        match t {
            Type::Named(id) => {
                if let Some(p) = Self::prim(id.name) { p }
                else if self.struct_names.iter().any(|n| n == id.name) { Ty::Struct(id.name.to_string()) }
                else if self.enum_names.iter().any(|n| n == id.name) { Ty::Enum(id.name.to_string()) }
                else { Ty::Unknown(id.name.to_string()) }
            }
            Type::Array { element, size } => Ty::Array(Box::new(self.ty_of(element)), *size),
            Type::Ref { mutable, inner } => Ty::Ref(Box::new(self.ty_of(inner)), *mutable),
        }
    }

    fn field_ty(&self, s: &str, f: &str) -> Option<Ty> {
        self.struct_fields.get(s)
            .and_then(|fs| fs.iter().find(|(n, _)| n == f))
            .map(|(_, t)| t.clone())
    }

    fn peel(t: &Ty) -> Ty {
        match t { Ty::Ref(i, _) => (**i).clone(), other => other.clone() }
    }

    fn collect_types(&mut self, decls: &[Decl<'src>]) -> Result<(), TypeError> {
        for d in decls {
            match d {
                Decl::Struct(s) => self.register_struct(s)?,
                Decl::Enum(e) => self.register_enum(e)?,
                Decl::Fn(f) => self.collect_types_block(&f.body)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn collect_types_block(&mut self, b: &Block<'src>) -> Result<(), TypeError> {
        for s in &b.stmts {
            match s {
                Stmt::Decl(Decl::Struct(sd)) => self.register_struct(sd)?,
                Stmt::Decl(Decl::Enum(e)) => self.register_enum(e)?,
                Stmt::Decl(Decl::Fn(f)) => self.collect_types_block(&f.body)?,
                Stmt::Match { arms, .. } => {
                    for a in arms { self.collect_types_block(&a.body)?; }
                }
                Stmt::For { body, .. } => self.collect_types_block(body)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn register_struct(&mut self, s: &StructDecl<'src>) -> Result<(), TypeError> {
        if self.struct_names.iter().any(|n| n == &s.name.name)
            || self.enum_names.iter().any(|n| n == &s.name.name)
        {
            return Err(TypeError {
                kind: TypeErrorKind::DuplicateDefinition(s.name.name.to_string()),
                span: s.name.span,
            });
        }
        self.struct_names.push(s.name.name.to_string());
        self.struct_raw.push((
            s.name.name.to_string(),
            s.fields.iter().map(|f| (f.name.name.to_string(), f.ty.clone())).collect(),
        ));
        Ok(())
    }

    fn register_enum(&mut self, e: &EnumDecl<'src>) -> Result<(), TypeError> {
        if self.enum_names.iter().any(|n| n == &e.name.name)
            || self.struct_names.iter().any(|n| n == &e.name.name)
        {
            return Err(TypeError {
                kind: TypeErrorKind::DuplicateDefinition(e.name.name.to_string()),
                span: e.name.span,
            });
        }
        self.enum_names.push(e.name.name.to_string());
        self.enum_raw.push((
            e.name.name.to_string(),
            e.variants.iter().map(|v| (v.name.name.to_string(), v.payload.clone(), v.span)).collect(),
        ));
        Ok(())
    }

    fn finish_types(&mut self) -> Result<(), TypeError> {
        for i in 0..self.struct_raw.len() {
            let (name, raw) = self.struct_raw[i].clone();
            let fields = raw.into_iter().map(|(f, t)| (f, self.ty_of(&t))).collect();
            self.struct_fields.insert(name, fields);
        }
        for i in 0..self.enum_raw.len() {
            let (name, raw) = self.enum_raw[i].clone();
            let mut out = Vec::new();
            for (v, payload, span) in raw {
                let ty = payload.as_ref().map(|t| self.ty_of(t));
                if let Some(t) = &ty {
                    if !is_copy(t) {
                        return Err(TypeError { kind: TypeErrorKind::NonCopyPayload(v), span });
                    }
                }
                out.push((v, ty));
            }
            self.enum_variants.insert(name, out);
        }
        Ok(())
    }

    fn collect_sigs(&mut self, decls: &[Decl<'src>]) {
        for d in decls {
            if let Decl::Fn(f) = d { self.sig_from_fn(f); }
        }
    }

    fn sigs_block(&mut self, b: &Block<'src>) {
        for s in &b.stmts {
            match s {
                Stmt::Decl(Decl::Fn(f)) => self.sig_from_fn(f),
                Stmt::Match { arms, .. } => {
                    for a in arms { self.sigs_block(&a.body); }
                }
                Stmt::For { body, .. } => self.sigs_block(body),
                _ => {}
            }
        }
    }

    fn sig_from_fn(&mut self, f: &FnDecl<'src>) {
        if let Some(id) = self.find_def(f.name.name, f.name.span) {
            let params = f.params.iter().map(|p| self.ty_of(&p.ty)).collect();
            let ret = f.ret.as_ref().map(|t| self.ty_of(t)).unwrap_or(Ty::Unit);
            self.fn_sigs.insert(id, FnSig { params, ret });
        }
        self.sigs_block(&f.body);
    }

    fn decl(&mut self, d: &Decl<'src>) -> Result<(), TypeError> {
        match d {
            Decl::Let(l) => {
                let want = l.ty.as_ref().map(|t| self.ty_of(t));
                let got = self.expr(&l.init, want.as_ref())?;
                let ty = match &want {
                    Some(w) => {
                        if !compatible(w, &got) {
                            return Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch { expected: w.clone(), found: got },
                                span: l.init.span,
                            });
                        }
                        w.clone()
                    }
                    None => got.clone(),
                };
                if let Some(id) = self.find_def(l.name.name, l.name.span) {
                    self.def_tys[id.index()] = ty;
                }
                self.expr_tys.insert(l.init.span, got);
                Ok(())
            }
            Decl::Const(c) => {
                let want = self.ty_of(&c.ty);
                let got = self.expr(&c.value, Some(&want))?;
                if !compatible(&want, &got) {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch { expected: want, found: got },
                        span: c.value.span,
                    });
                }
                if let Some(id) = self.find_def(c.name.name, c.name.span) {
                    self.def_tys[id.index()] = want;
                }
                Ok(())
            }
            Decl::Fn(f) => {
                let ret = f.ret.as_ref().map(|t| self.ty_of(t));
                for p in &f.params {
                    let t = self.ty_of(&p.ty);
                    if let Some(id) = self.find_def(p.name.name, p.name.span) {
                        self.def_tys[id.index()] = t;
                    }
                }
                let saved_ret = self.current_ret_ty.clone();
                self.current_ret_ty = ret.clone();
                let body_ret = self.block(&f.body, ret.as_ref())?;
                self.current_ret_ty = saved_ret;
                if let (Some(w), Some(g)) = (&ret, &body_ret) {
                    if !compatible(w, g) {
                        return Err(TypeError {
                            kind: TypeErrorKind::ReturnTypeMismatch { expected: w.clone(), found: g.clone() },
                            span: f.body.span,
                        });
                    }
                }
                if let Some(id) = self.find_def(f.name.name, f.name.span) {
                    self.def_tys[id.index()] = Ty::Unit;
                }
                Ok(())
            }
            Decl::Struct(_) | Decl::Enum(_) => Ok(()),
        }
    }

    fn block(&mut self, b: &Block<'src>, ret: Option<&Ty>) -> Result<Option<Ty>, TypeError> {
        let mut last = None;
        for s in &b.stmts {
            if let Some(t) = self.stmt(s, ret)? { last = Some(t); }
        }
        Ok(last)
    }

    fn stmt(&mut self, s: &Stmt<'src>, ret: Option<&Ty>) -> Result<Option<Ty>, TypeError> {
        match s {
            Stmt::Decl(d) => { self.decl(d)?; Ok(None) }
            Stmt::Return { value, span } => {
                let ty = match value { Some(e) => self.expr(e, ret)?, None => Ty::Unit };
                if let Some(w) = ret {
                    if !compatible(w, &ty) {
                        return Err(TypeError {
                            kind: TypeErrorKind::ReturnTypeMismatch { expected: w.clone(), found: ty },
                            span: *span,
                        });
                    }
                }
                Ok(Some(ty))
            }
            Stmt::Expr(e) => { self.expr(e, None)?; Ok(None) }
            Stmt::If { condition, then_block, else_block, .. } => {
                self.cond(condition)?;
                self.block(then_block, ret)?;
                if let Some(eb) = else_block { self.block(eb, ret)?; }
                Ok(None)
            }
            Stmt::While { condition, body, .. } => {
                self.cond(condition)?;
                self.block(body, ret)?;
                Ok(None)
            }
            Stmt::For { binding, iter, body, .. } => {
                let elem = self.for_iter(iter)?;
                if let Some(id) = self.find_def(binding.name, binding.span) {
                    self.def_tys[id.index()] = elem;
                }
                self.block(body, ret)?;
                Ok(None)
            }
            Stmt::Break(_) | Stmt::Continue(_) => Ok(None),
            Stmt::Match { scrutinee, arms, span } => {
                let st = self.expr(scrutinee, None)?;
                let ename = match &st {
                    Ty::Enum(n) => n.clone(),
                    Ty::Unknown(_) => String::new(),
                    other => return Err(TypeError {
                        kind: TypeErrorKind::NotAnEnum(other.clone()),
                        span: scrutinee.span,
                    }),
                };
                let spec = self.enum_variants.get(&ename).cloned().unwrap_or_default();
                let mut covered: Vec<String> = Vec::new();
                let mut fallback = false;
                for arm in arms {
                    if arm.wildcard {
                        if arm.guard.is_none() { fallback = true; }
                    } else {
                        if !ename.is_empty() && arm.enum_name.name != ename {
                            return Err(TypeError {
                                kind: TypeErrorKind::UnknownEnum(arm.enum_name.name.to_string()),
                                span: arm.enum_name.span,
                            });
                        }
                        let Some((_, payload)) = spec.iter().find(|(v, _)| v == arm.variant.name) else {
                            return Err(TypeError {
                                kind: TypeErrorKind::UnknownVariant(arm.variant.name.to_string()),
                                span: arm.variant.span,
                            });
                        };
                        if arm.guard.is_none() {
                            if covered.iter().any(|x| x == arm.variant.name) {
                                return Err(TypeError {
                                    kind: TypeErrorKind::DuplicateArm(arm.variant.name.to_string()),
                                    span: arm.variant.span,
                                });
                            }
                            covered.push(arm.variant.name.to_string());
                        }
                        match (&arm.binding, payload) {
                            (Some(b), Some(t)) => {
                                if let Some(id) = self.find_def(b.name, b.span) {
                                    self.def_tys[id.index()] = t.clone();
                                }
                            }
                            (None, None) => {}
                            _ => return Err(TypeError {
                                kind: TypeErrorKind::ArmBinding(arm.variant.name.to_string()),
                                span: arm.variant.span,
                            }),
                        }
                    }
                    if let Some(g) = &arm.guard { self.cond(g)?; }
                    self.block(&arm.body, ret)?;
                }
                if !fallback {
                    for (v, _) in &spec {
                        if !covered.iter().any(|x| x == v) {
                            return Err(TypeError {
                                kind: TypeErrorKind::NonExhaustiveMatch { missing: v.clone() },
                                span: *span,
                            });
                        }
                    }
                }
                Ok(None)
            }
        }
    }

    /// Returns the per-iteration binding type of a `for` iterator.
    fn for_iter(&mut self, iter: &Expr<'src>) -> Result<Ty, TypeError> {
        if let ExprKind::Range { start, end } = &iter.kind {
            let st = self.expr(start, None)?;
            let en = self.expr(end, Some(&st))?;
            if !is_integer(&st) || !compatible(&st, &en) {
                return Err(TypeError {
                    kind: TypeErrorKind::RangeBounds { start: st, end: en },
                    span: iter.span,
                });
            }
            self.expr_tys.insert(iter.span, st.clone());
            return Ok(st);
        }
        let it = self.expr(iter, None)?;
        match Self::peel(&it) {
            Ty::Array(el, _) => {
                if !is_copy(&el) {
                    return Err(TypeError { kind: TypeErrorKind::ForElementNotCopy(*el), span: iter.span });
                }
                Ok(*el)
            }
            Ty::Unknown(_) => Ok(Ty::Unknown(String::new())),
            other => Err(TypeError { kind: TypeErrorKind::NotIterable(other), span: iter.span }),
        }
    }

    fn cond(&mut self, e: &Expr<'src>) -> Result<(), TypeError> {
        let t = self.expr(e, Some(&Ty::Bool))?;
        if !compatible(&Ty::Bool, &t) {
            return Err(TypeError {
                kind: TypeErrorKind::TypeMismatch { expected: Ty::Bool, found: t },
                span: e.span,
            });
        }
        Ok(())
    }

    fn expr(&mut self, e: &Expr<'src>, expected: Option<&Ty>) -> Result<Ty, TypeError> {
        let ty = match &e.kind {
            ExprKind::Int(n) => {
                let t = match expected { Some(t) if is_integer(t) => t.clone(), _ => Ty::I32 };
                if !int_fits(*n, &t) {
                    return Err(TypeError {
                        kind: TypeErrorKind::LiteralOutOfRange { value: *n, ty: t },
                        span: e.span,
                    });
                }
                t
            }
            ExprKind::Bool(_) => Ty::Bool,
            ExprKind::Str(_) => Ty::Str,
            ExprKind::Char(_) => Ty::Char,
            ExprKind::Ident(_) => match self.find_use(e.span) {
                Some(id) => self.def_tys[id.index()].clone(),
                None => Ty::Unknown(String::new()),
            },
            ExprKind::Unary { op, operand } => match op {
                UnOp::Neg => {
                    let inner = match (&operand.kind, expected) {
                        (ExprKind::Int(n), Some(t)) if signed_neg_fits(*n, t) => {
                            self.expr_tys.insert(operand.span, t.clone());
                            t.clone()
                        }
                        _ => self.expr(operand, expected)?,
                    };
                    if !is_numeric(&inner) {
                        return Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch { expected: Ty::I32, found: inner },
                            span: e.span,
                        });
                    }
                    if is_unsigned(&inner) {
                        return Err(TypeError { kind: TypeErrorKind::CannotNegate { ty: inner }, span: e.span });
                    }
                    inner
                }
                UnOp::Not => {
                    let inner = self.expr(operand, Some(&Ty::Bool))?;
                    if !compatible(&Ty::Bool, &inner) {
                        return Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch { expected: Ty::Bool, found: inner },
                            span: e.span,
                        });
                    }
                    Ty::Bool
                }
            },
            ExprKind::Binary { op, lhs, rhs } => self.binary(*op, lhs, rhs, expected, e.span)?,
            ExprKind::Assign { target, value } => {
                let t = self.expr(target, None)?;
                if matches!(t, Ty::Array(_, _)) {
                    return Err(TypeError { kind: TypeErrorKind::Unassignable(t), span: e.span });
                }
                let v = self.expr(value, Some(&t))?;
                if !compatible(&t, &v) {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch { expected: t, found: v },
                        span: e.span,
                    });
                }
                Ty::Unit
            }
            ExprKind::Call { callee, args } => self.call(callee, args, e.span)?,
            ExprKind::ArrayInit { elements } => {
                let exp = match expected {
                    Some(Ty::Array(el, n)) => {
                        if *n != elements.len() as u64 {
                            return Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch {
                                    expected: expected.unwrap().clone(),
                                    found: Ty::Array(el.clone(), elements.len() as u64),
                                },
                                span: e.span,
                            });
                        }
                        Some((**el).clone())
                    }
                    _ => None,
                };
                let mut elem = exp.clone().unwrap_or(Ty::Unknown(String::new()));
                for x in elements {
                    let t = self.expr(x, exp.as_ref())?;
                    if matches!(elem, Ty::Unknown(_)) { elem = t; }
                    else if !compatible(&elem, &t) {
                        return Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch { expected: elem, found: t },
                            span: x.span,
                        });
                    }
                }
                Ty::Array(Box::new(elem), elements.len() as u64)
            }
            ExprKind::Index { array, index } => {
                let at = Self::peel(&self.expr(array, None)?);
                let it = self.expr(index, None)?;
                if !is_integer(&it) {
                    return Err(TypeError { kind: TypeErrorKind::IndexNotInteger(it), span: index.span });
                }
                match at {
                    Ty::Array(el, n) => {
                        if let ExprKind::Int(k) = index.kind {
                            if k >= n {
                                return Err(TypeError {
                                    kind: TypeErrorKind::IndexOutOfRange { index: k, len: n },
                                    span: index.span,
                                });
                            }
                        }
                        *el
                    }
                    Ty::Unknown(_) => Ty::Unknown(String::new()),
                    other => return Err(TypeError { kind: TypeErrorKind::NotAnArray(other), span: array.span }),
                }
            }
            ExprKind::StructInit { name, fields } => {
                let spec = match self.struct_fields.get(&name.name.to_string()) {
                    Some(s) => s.clone(),
                    None => return Err(TypeError {
                        kind: TypeErrorKind::UnknownStruct(name.name.to_string()),
                        span: name.span,
                    }),
                };
                let mut seen: Vec<&str> = Vec::new();
                for (f, _) in fields {
                    if !spec.iter().any(|(n, _)| n == &f.name) {
                        return Err(TypeError { kind: TypeErrorKind::UnexpectedField(f.name.to_string()), span: f.span });
                    }
                    if seen.contains(&f.name) {
                        return Err(TypeError { kind: TypeErrorKind::DuplicateField(f.name.to_string()), span: f.span });
                    }
                    seen.push(f.name);
                }
                for (fname, fty) in &spec {
                    if !seen.contains(&fname.as_str()) {
                        return Err(TypeError { kind: TypeErrorKind::MissingField(fname.clone()), span: e.span });
                    }
                    let (_, v) = fields.iter().find(|(f, _)| f.name == fname).unwrap();
                    let got = self.expr(v, Some(fty))?;
                    if !compatible(fty, &got) {
                        return Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch { expected: fty.clone(), found: got },
                            span: v.span,
                        });
                    }
                }
                Ty::Struct(name.name.to_string())
            }
            ExprKind::Field { object, field } => {
                let ot = Self::peel(&self.expr(object, None)?);
                match ot {
                    Ty::Struct(s) => match self.field_ty(&s, field.name) {
                        Some(t) => t,
                        None => return Err(TypeError {
                            kind: TypeErrorKind::MissingField(field.name.to_string()),
                            span: field.span,
                        }),
                    },
                    Ty::Unknown(_) => Ty::Unknown(String::new()),
                    other => return Err(TypeError { kind: TypeErrorKind::NotAStruct(other), span: object.span }),
                }
            }
            ExprKind::Borrow { mutable, operand } => {
                if !is_place(operand) {
                    return Err(TypeError { kind: TypeErrorKind::BorrowOfTemporary, span: operand.span });
                }
                if *mutable {
                    if let ExprKind::Ident(_) = operand.kind {
                        if let Some(id) = self.find_use(operand.span) {
                            if !self.resolved.defs[id.index()].mutable {
                                return Err(TypeError { kind: TypeErrorKind::ImmutableBorrow, span: e.span });
                            }
                        }
                    }
                }
                let inner = self.expr(operand, None)?;
                Ty::Ref(Box::new(inner), *mutable)
            }
            ExprKind::Deref(inner) => {
                let rt = self.expr(inner, None)?;
                match rt {
                    Ty::Ref(t, _) => *t,
                    Ty::Unknown(_) => Ty::Unknown(String::new()),
                    other => return Err(TypeError { kind: TypeErrorKind::NotAReference(other), span: inner.span }),
                }
            }
            ExprKind::Cast { operand, ty } => {
                let from = self.expr(operand, None)?;
                let to = self.ty_of(ty);
                let ok = |t: &Ty| is_integer(t) || matches!(t, Ty::Char | Ty::Unknown(_));
                if !ok(&from) || !ok(&to) {
                    return Err(TypeError { kind: TypeErrorKind::InvalidCast { from, to }, span: e.span });
                }
                to
            }
            ExprKind::EnumInit { enum_name, variant, payload } => {
                let spec = match self.enum_variants.get(&enum_name.name.to_string()) {
                    Some(s) => s.clone(),
                    None => return Err(TypeError {
                        kind: TypeErrorKind::UnknownEnum(enum_name.name.to_string()),
                        span: enum_name.span,
                    }),
                };
                let Some((_, want)) = spec.iter().find(|(v, _)| v == variant.name) else {
                    return Err(TypeError {
                        kind: TypeErrorKind::UnknownVariant(variant.name.to_string()),
                        span: variant.span,
                    });
                };
                match (payload, want) {
                    (Some(p), Some(t)) => {
                        let got = self.expr(p, Some(t))?;
                        if !compatible(t, &got) {
                            return Err(TypeError {
                                kind: TypeErrorKind::TypeMismatch { expected: t.clone(), found: got },
                                span: p.span,
                            });
                        }
                    }
                    (None, None) => {}
                    _ => return Err(TypeError {
                        kind: TypeErrorKind::ArmBinding(variant.name.to_string()),
                        span: variant.span,
                    }),
                }
                Ty::Enum(enum_name.name.to_string())
            }
            ExprKind::Try(inner) => {
                let inner_ty = self.expr(inner, None)?;
                let enum_name = match &inner_ty {
                    Ty::Enum(n) => n.clone(),
                    Ty::Unknown(_) => String::new(),
                    other => return Err(TypeError {
                        kind: TypeErrorKind::NotAnEnum(other.clone()),
                        span: inner.span,
                    }),
                };
                let spec = match self.enum_variants.get(&enum_name) {
                    Some(s) => s.clone(),
                    None => return Err(TypeError { kind: TypeErrorKind::UnknownEnum(enum_name), span: inner.span }),
                };
                if spec.len() != 2 {
                    return Err(TypeError {
                        kind: TypeErrorKind::TryRequiresTwoVariants { found: spec.len() },
                        span: e.span,
                    });
                }
                let (err_name, _) = &spec[0];
                let (ok_name, ok_payload) = &spec[1];
                if let Some(ret_ty) = self.current_ret_ty.as_ref() {
                    match ret_ty {
                        Ty::Enum(re) if re == &enum_name => {}
                        _ => return Err(TypeError {
                            kind: TypeErrorKind::TryReturnTypeMismatch {
                                expected: ret_ty.clone(),
                                found: inner_ty.clone(),
                            },
                            span: e.span,
                        }),
                    }
                }
                self.try_info.insert(e.span, TryInfo {
                    enum_name: enum_name.clone(),
                    err_variant: err_name.clone(),
                    ok_variant: ok_name.clone(),
                });
                match ok_payload { Some(t) => t.clone(), None => Ty::Unit }
            }
            ExprKind::Range { .. } => {
                return Err(TypeError { kind: TypeErrorKind::RangeOutsideFor, span: e.span });
            }
        };
        self.expr_tys.insert(e.span, ty.clone());
        Ok(ty)
    }

    fn binary(&mut self, op: BinOp, lhs: &Expr<'src>, rhs: &Expr<'src>, expected: Option<&Ty>, span: Span) -> Result<Ty, TypeError> {
        let arith = matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem);
        let logical = matches!(op, BinOp::And | BinOp::Or);
        let hint = if logical { Some(Ty::Bool) } else if arith { expected.cloned() } else { None };
        let (l, r) = if arith && is_int_literal_tree(lhs) && !is_int_literal_tree(rhs) {
            let r = self.expr(rhs, hint.as_ref())?;
            let h = hint.clone().or_else(|| if is_integer(&r) { Some(r.clone()) } else { None });
            let l = self.expr(lhs, h.as_ref())?;
            (l, r)
        } else {
            let l = self.expr(lhs, hint.as_ref())?;
            let h = hint.clone().or_else(|| if is_integer(&l) { Some(l.clone()) } else { None });
            let r = self.expr(rhs, h.as_ref())?;
            (l, r)
        };
        if logical {
            if !compatible(&Ty::Bool, &l) {
                return Err(TypeError { kind: TypeErrorKind::TypeMismatch { expected: Ty::Bool, found: l }, span: lhs.span });
            }
            if !compatible(&Ty::Bool, &r) {
                return Err(TypeError { kind: TypeErrorKind::TypeMismatch { expected: Ty::Bool, found: r }, span: rhs.span });
            }
            Ok(Ty::Bool)
        } else if arith {
            if !is_numeric(&l) || !is_numeric(&r) || !compatible(&l, &r) {
                return Err(TypeError { kind: TypeErrorKind::TypeMismatch { expected: l, found: r }, span });
            }
            Ok(l)
        } else {
            if !compatible(&l, &r) {
                return Err(TypeError { kind: TypeErrorKind::TypeMismatch { expected: l, found: r }, span });
            }
            Ok(Ty::Bool)
        }
    }

    fn call(&mut self, callee: &Expr<'src>, args: &[Expr<'src>], span: Span) -> Result<Ty, TypeError> {
        let sig = match &callee.kind {
            ExprKind::Ident(_) => self.find_use(callee.span).and_then(|id| self.fn_sigs.get(&id).cloned()),
            _ => None,
        };
        match &sig {
            Some(s) => {
                if s.params.len() != args.len() {
                    return Err(TypeError {
                        kind: TypeErrorKind::ArgCount { expected: s.params.len(), found: args.len() },
                        span,
                    });
                }
                for (a, p) in args.iter().zip(&s.params) {
                    let at = self.expr(a, Some(p))?;
                    if !compatible(p, &at) {
                        return Err(TypeError {
                            kind: TypeErrorKind::TypeMismatch { expected: p.clone(), found: at },
                            span: a.span,
                        });
                    }
                }
            }
            None => { for a in args { self.expr(a, None)?; } }
        }
        self.expr(callee, None)?;
        Ok(sig.map(|s| s.ret).unwrap_or(Ty::I32))
    }

    fn find_def(&self, name: &str, span: Span) -> Option<DefId> {
        self.resolved.defs.iter().enumerate()
            .find_map(|(i, d)| if d.name == name && d.span == span { Some(DefId::from_index(i)) } else { None })
    }

    fn find_use(&self, span: Span) -> Option<DefId> {
        self.resolved.uses.iter().find_map(|(s, id)| if *s == span { Some(*id) } else { None })
    }
}

fn is_place(e: &Expr<'_>) -> bool {
    match &e.kind {
        ExprKind::Ident(_) | ExprKind::Deref(_) => true,
        ExprKind::Field { object, .. } => is_place(object),
        ExprKind::Index { array, .. } => is_place(array),
        _ => false,
    }
}

fn compatible(a: &Ty, b: &Ty) -> bool {
    a == b || matches!(a, Ty::Unknown(_)) || matches!(b, Ty::Unknown(_))
}

fn is_integer(t: &Ty) -> bool {
    matches!(t, Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64)
}

fn is_unsigned(t: &Ty) -> bool { matches!(t, Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64) }

fn is_numeric(t: &Ty) -> bool { is_integer(t) || matches!(t, Ty::Unknown(_)) }

pub fn is_copy(t: &Ty) -> bool {
    is_integer(t) || matches!(t, Ty::Bool | Ty::Str | Ty::Char | Ty::Unit | Ty::Ref(..) | Ty::Enum(_) | Ty::Unknown(_))
}

fn int_fits(v: u64, t: &Ty) -> bool {
    match t {
        Ty::I8 => v <= i8::MAX as u64,
        Ty::I16 => v <= i16::MAX as u64,
        Ty::I32 => v <= i32::MAX as u64,
        Ty::I64 => v <= i64::MAX as u64,
        Ty::U8 => v <= u8::MAX as u64,
        Ty::U16 => v <= u16::MAX as u64,
        Ty::U32 => v <= u32::MAX as u64,
        Ty::U64 => true,
        _ => false,
    }
}

fn signed_neg_fits(v: u64, t: &Ty) -> bool {
    match t {
        Ty::I8 => v <= 128,
        Ty::I16 => v <= 32_768,
        Ty::I32 => v <= 2_147_483_648,
        Ty::I64 => v <= 9_223_372_036_854_775_808,
        _ => false,
    }
}

fn is_int_literal_tree(e: &Expr<'_>) -> bool {
    match &e.kind {
        ExprKind::Int(_) => true,
        ExprKind::Unary { operand, .. } => is_int_literal_tree(operand),
        ExprKind::Binary { lhs, rhs, .. } => is_int_literal_tree(lhs) && is_int_literal_tree(rhs),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::resolve::resolve;

    fn check(src: &str) -> CheckedProgram<'_> {
        let a = parser::parse(src).unwrap();
        let r = resolve(&a).unwrap();
        type_check(&a, &r).unwrap()
    }
    fn err(src: &str) -> TypeErrorKind {
        let a = parser::parse(src).unwrap();
        let r = resolve(&a).unwrap();
        type_check(&a, &r).unwrap_err().kind
    }

    #[test]
    fn literals_and_numerics() {
        assert_eq!(check("let x = 10;").def_tys[0], Ty::I32);
        assert_eq!(check("let x: u32 = 10;").def_tys[0], Ty::U32);
        assert!(matches!(err("let x: u8 = 300;"), TypeErrorKind::LiteralOutOfRange { .. }));
        assert!(matches!(
            err("fn f() { let a: u32 = 1; let b: i32 = 2; let c = a + b; }"),
            TypeErrorKind::TypeMismatch { .. }
        ));
    }

    #[test]
    fn aggregates_and_refs() {
        assert_eq!(check("let a = [1, 2];").def_tys[0], Ty::Array(Box::new(Ty::I32), 2));
        check("struct P { x: i32 } fn f(p: &P) -> i32 { return p.x; }");
        check("fn f(a: &[i32; 2]) -> i32 { return a[0]; }");
        assert!(matches!(err("fn f() { let r = &(1 + 2); }"), TypeErrorKind::BorrowOfTemporary));
        assert!(matches!(err("fn f() { let x = 1; let r = &mut x; }"), TypeErrorKind::ImmutableBorrow));
    }

    #[test]
    fn casts_and_ffi() {
        check("fn f() -> i32 { return '!' as i32; }");
        assert!(matches!(err("fn f() { let x = 1; let y = x as &i32; }"), TypeErrorKind::InvalidCast { .. }));
        check("extern fn puts(s: str) -> i32; fn main() -> i32 { puts(\"hi\"); return 0; }");
    }

    #[test]
    fn enums() {
        check("enum E { A, B(i32) } fn f(e: E) -> i32 { match e { E::A => { return 0; } E::B(v) => { return v; } } }");
        assert_eq!(check("enum E { A } fn f() { let x = E::A; }").def_tys[1], Ty::Enum("E".into()));
        assert!(matches!(
            err("enum E { A, B(i32) } fn f(e: E) -> i32 { match e { E::A => { return 0; } } }"),
            TypeErrorKind::NonExhaustiveMatch { .. }
        ));
        assert!(matches!(err("enum E { A } fn f() { let x = E::Z; }"), TypeErrorKind::UnknownVariant(_)));
        assert!(matches!(err("struct P { x: i32 } enum E { A(P) }"), TypeErrorKind::NonCopyPayload(_)));
    }

    #[test]
    fn wildcard_and_guards() {
        check("enum E { A, B(i32) } fn f(e: E) -> i32 { match e { _ => { return 0; } } }");
        check("enum E { A(i32) } fn f(e: E) -> i32 { match e { E::A(v) if v > 0 => { return v; } _ => { return 0; } } }");
        // Guarded arms do not cover the variant.
        assert!(matches!(
            err("enum E { A(i32) } fn f(e: E) -> i32 { match e { E::A(v) if v > 0 => { return v; } } }"),
            TypeErrorKind::NonExhaustiveMatch { .. }
        ));
        // Guarded arms may repeat a variant.
        check("enum E { A(i32) } fn f(e: E) -> i32 { match e { E::A(v) if v > 0 => { return 1; } E::A(v) if v < 0 => { return 2; } E::A(v) => { return 0; } } }");
        // Guard must be bool.
        assert!(matches!(
            err("enum E { A } fn f(e: E) -> i32 { match e { E::A if 1 => { return 0; } _ => { return 1; } } }"),
            TypeErrorKind::TypeMismatch { .. }
        ));
    }

    #[test]
    fn for_loops() {
        check("fn f() -> i32 { mut let s = 0; for x in [1, 2, 3] { s += x; } return s; }");
        check("fn f() -> i32 { mut let s = 0; for i in 0..10 { s += i; } return s; }");
        check("fn f(a: &[i32; 2]) -> i32 { mut let s = 0; for x in a { s += x; } return s; }");
        assert!(matches!(err("fn f() { for x in 1 { } }"), TypeErrorKind::NotIterable(_)));
        assert!(matches!(
            err("struct P { x: i32 } fn f() { for p in [P { x: 1 }] { } }"),
            TypeErrorKind::ForElementNotCopy(_)
        ));
        assert!(matches!(err("fn f() { for i in 0..true { } }"), TypeErrorKind::RangeBounds { .. }));
        assert!(matches!(err("fn f() { let r = 0..3; }"), TypeErrorKind::RangeOutsideFor));
    }

    #[test]
    fn try_operator() {
        check("enum Result { Err(i32), Ok(i32) } fn f() -> Result { let x = g()?; return Result::Ok(x); } fn g() -> Result { return Result::Ok(42); }");
        assert!(matches!(
            err("enum Opt { None, Some(i32), Other } fn f() -> Opt { let x = g()?; return Opt::Some(x); } fn g() -> Opt { return Opt::Some(1); }"),
            TypeErrorKind::TryRequiresTwoVariants { .. }
        ));
        assert!(matches!(err("fn f() -> i32 { let x = 42?; return x; }"), TypeErrorKind::NotAnEnum(_)));
    }
}