//! Backend: transpilation to portable C11.
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write as _};
use crate::ast::*;
use crate::lexer::Span;
use crate::resolve::{DefId, DefKind, ResolvedProgram};
use crate::typeck::{CheckedProgram, Ty};

#[derive(Clone, Debug)]
pub struct CodegenError { pub kind: CodegenErrorKind, pub span: Span }

#[derive(Clone, Debug)]
pub enum CodegenErrorKind {
    UnknownType(String),
    NonConstantGlobalInit,
    Capture { name: String },
    IndirectCall,
    MainSignature(&'static str),
    MissingReturn,
    Unresolved,
    ArrayReturn,
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CodegenErrorKind::*;
        match &self.kind {
            UnknownType(n) => write!(f, "cannot generate C for unknown type `{n}`"),
            NonConstantGlobalInit => f.write_str("global initializer must be a constant expression"),
            Capture { name } => write!(f, "nested function cannot capture local `{name}`"),
            IndirectCall => f.write_str("only direct calls to named functions are supported"),
            MainSignature(m) => f.write_str(m),
            MissingReturn => f.write_str("function with a return type must end with `return`"),
            Unresolved => f.write_str("internal error: unresolved identifier"),
            ArrayReturn => f.write_str("arrays cannot be returned by value"),
        }
    }
}

impl std::error::Error for CodegenError {}

const RESERVED: &[&str] = &[
    "auto", "break", "case", "char", "const", "continue", "default", "do", "double",
    "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long",
    "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct",
    "switch", "typedef", "union", "unsigned", "void", "volatile", "while",
    "bool", "true", "false", "main", "NULL", "size_t",
    "int8_t", "int16_t", "int32_t", "int64_t",
    "uint8_t", "uint16_t", "uint32_t", "uint64_t",
];

pub fn compile_to_c<'a, 'src>(
    program: &'a Program<'src>,
    resolved: &'a ResolvedProgram<'src>,
    checked: &'a CheckedProgram<'src>,
) -> Result<String, CodegenError> {
    let mut g = Cgen::new(resolved, checked);
    g.collect(program)?;
    g.emit()
}

struct FnInfo<'a, 'src> {
    c_name: String,
    decl: &'a FnDecl<'src>,
    locals: HashSet<DefId>,
    ret: Option<Ty>,
}

struct StructInfo {
    orig: String,
    c_name: String,
    fields: Vec<(String, String, Ty)>,
}

struct EnumInfo {
    orig: String,
    c_name: String,
    variants: Vec<(String, String, Option<Ty>)>,
}

struct Cgen<'a, 'src> {
    resolved: &'a ResolvedProgram<'src>,
    checked: &'a CheckedProgram<'src>,
    uses: HashMap<Span, DefId>,
    def_at: HashMap<Span, DefId>,
    names: Vec<String>,
    used: HashSet<String>,
    globals: HashSet<DefId>,
    global_lets: Vec<(&'a LetDecl<'src>, DefId)>,
    global_consts: Vec<(&'a ConstDecl<'src>, DefId)>,
    structs: Vec<StructInfo>,
    enums: Vec<EnumInfo>,
    fns: Vec<FnInfo<'a, 'src>>,
    entry: Option<usize>,
}

impl<'a, 'src> Cgen<'a, 'src> {
    fn new(resolved: &'a ResolvedProgram<'src>, checked: &'a CheckedProgram<'src>) -> Self {
        Self {
            resolved, checked,
            uses: resolved.uses.iter().copied().collect(),
            def_at: resolved.defs.iter().enumerate().map(|(i, d)| (d.span, DefId::from_index(i))).collect(),
            names: vec![String::new(); resolved.defs.len()],
            used: RESERVED.iter().map(|s| s.to_string()).collect(),
            globals: HashSet::new(),
            global_lets: Vec::new(),
            global_consts: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            fns: Vec::new(),
            entry: None,
        }
    }

    fn collect(&mut self, program: &'a Program<'src>) -> Result<(), CodegenError> {
        let mut ev: Vec<(String, Vec<(String, Option<Ty>)>)> =
            self.checked.enum_variants.clone().into_iter().collect();
        ev.sort_by(|a, b| a.0.cmp(&b.0));
        for (orig, variants) in ev {
            let c_name = self.fresh_name(&orig);
            let variants = variants.into_iter().map(|(v, t)| (v.clone(), sanitize(&v), t)).collect();
            self.enums.push(EnumInfo { orig, c_name, variants });
        }
        let mut sf: Vec<(String, Vec<(String, Ty)>)> =
            self.checked.struct_fields.clone().into_iter().collect();
        sf.sort_by(|a, b| a.0.cmp(&b.0));
        for (orig, fields) in sf {
            let c_name = self.fresh_name(&orig);
            let fields = fields.into_iter().map(|(f, t)| (f.clone(), sanitize(&f), t)).collect();
            self.structs.push(StructInfo { orig, c_name, fields });
        }
        for decl in &program.decls {
            match decl {
                Decl::Let(d) => {
                    let id = self.def_id(d.name.span);
                    self.names[id.index()] = self.fresh(d.name.name, id);
                    self.globals.insert(id);
                    self.global_lets.push((d, id));
                }
                Decl::Const(d) => {
                    let id = self.def_id(d.name.span);
                    self.names[id.index()] = self.fresh(d.name.name, id);
                    self.globals.insert(id);
                    self.global_consts.push((d, id));
                }
                Decl::Fn(d) => self.collect_fn(d, None, true)?,
                Decl::Struct(_) | Decl::Enum(_) => {}
            }
        }
        Ok(())
    }

    fn collect_fn(&mut self, d: &'a FnDecl<'src>, parent: Option<&str>, top: bool) -> Result<(), CodegenError> {
        let id = self.def_id(d.name.span);
        let ret = self.checked.fn_sigs.get(&id)
            .map(|s| s.ret.clone())
            .filter(|t| !matches!(t, Ty::Unit));
        if d.is_extern {
            let name = d.name.name.to_string();
            self.used.insert(name.clone());
            self.names[id.index()] = name.clone();
            let mut locals = HashSet::new();
            for p in &d.params {
                let pid = self.def_id(p.name.span);
                self.names[pid.index()] = self.fresh(p.name.name, pid);
                locals.insert(pid);
            }
            self.fns.push(FnInfo { c_name: name, decl: d, locals, ret });
            return Ok(());
        }
        let is_entry = top && d.name.name == "main";
        let base = if is_entry {
            "oster_main".to_string()
        } else {
            match parent {
                Some(p) => format!("{p}__{}", d.name.name),
                None => d.name.name.to_string(),
            }
        };
        let c_name = self.fresh(&base, id);
        self.names[id.index()] = c_name.clone();
        if is_entry {
            if !d.params.is_empty() {
                return Err(self.err(CodegenErrorKind::MainSignature("main must take no parameters"), d.name.span));
            }
            if !matches!(ret, None | Some(Ty::I32)) {
                return Err(self.err(CodegenErrorKind::MainSignature("main must return i32 or nothing"), d.name.span));
            }
        }
        let mut locals = HashSet::new();
        for p in &d.params {
            let pid = self.def_id(p.name.span);
            self.names[pid.index()] = self.fresh(p.name.name, pid);
            locals.insert(pid);
        }
        let idx = self.fns.len();
        self.fns.push(FnInfo { c_name, decl: d, locals, ret });
        if is_entry { self.entry = Some(idx); }
        self.collect_block(&d.body, idx)
    }

    fn collect_block(&mut self, b: &'a Block<'src>, fn_idx: usize) -> Result<(), CodegenError> {
        for stmt in &b.stmts {
            match stmt {
                Stmt::Decl(d) => match d {
                    Decl::Let(ld) => {
                        let id = self.def_id(ld.name.span);
                        self.names[id.index()] = self.fresh(ld.name.name, id);
                        self.fns[fn_idx].locals.insert(id);
                    }
                    Decl::Const(cd) => {
                        let id = self.def_id(cd.name.span);
                        self.names[id.index()] = self.fresh(cd.name.name, id);
                        self.fns[fn_idx].locals.insert(id);
                    }
                    Decl::Fn(nd) => {
                        let parent = self.fns[fn_idx].c_name.clone();
                        self.collect_fn(nd, Some(&parent), false)?;
                    }
                    Decl::Struct(_) | Decl::Enum(_) => {}
                },
                Stmt::Match { arms, .. } => {
                    for a in arms {
                        if let Some(bnd) = &a.binding {
                            let id = self.def_id(bnd.span);
                            self.names[id.index()] = self.fresh(bnd.name, id);
                            self.fns[fn_idx].locals.insert(id);
                        }
                        self.collect_block(&a.body, fn_idx)?;
                    }
                }
                Stmt::For { binding, body, .. } => {
                    let id = self.def_id(binding.span);
                    self.names[id.index()] = self.fresh(binding.name, id);
                    self.fns[fn_idx].locals.insert(id);
                    self.collect_block(body, fn_idx)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn emit(&self) -> Result<String, CodegenError> {
        let mut out = String::from(
            "/* generated by Oster — do not edit */\n#include <stdint.h>\n#include <stdbool.h>\n\n",
        );
        let no_locals = HashSet::new();

        for (d, id) in &self.global_consts {
            if !self.is_const(&d.value) {
                return Err(self.err(CodegenErrorKind::NonConstantGlobalInit, d.value.span));
            }
            let ty = self.def_ty(*id, d.name.span)?;
            if matches!(ty, Ty::Array(_, _)) && !matches!(d.value.kind, ExprKind::ArrayInit { .. }) {
                return Err(self.err(CodegenErrorKind::NonConstantGlobalInit, d.value.span));
            }
            let mut val = String::new();
            self.emit_init(&d.value, &ty, &no_locals, &mut val)?;
            if matches!(ty, Ty::Array(_, _) | Ty::Struct(_) | Ty::Enum(_)) {
                out.push_str("static ");
                let ds = self.decl_string(&ty, &self.names[id.index()], false, d.name.span)?;
                writeln!(out, "{ds} = {val};").unwrap();
            } else {
                let t = self.c_ty(&ty, d.name.span)?;
                writeln!(out, "#define {} (({t})({val}))", self.names[id.index()]).unwrap();
            }
        }
        if !self.global_consts.is_empty() { out.push('\n'); }

        for e in &self.enums {
            writeln!(out, "struct {} {{", e.c_name).unwrap();
            out.push_str("    int32_t tag;\n");
            if e.variants.iter().any(|(_, _, t)| t.is_some()) {
                out.push_str("    union {\n");
                for (_, member, t) in &e.variants {
                    if let Some(t) = t {
                        out.push_str("        ");
                        self.declare(t, member.clone(), &mut out, Span::new(0, 0))?;
                        writeln!(out, ";").unwrap();
                    }
                }
                out.push_str("    } data;\n");
            }
            writeln!(out, "}};").unwrap();
        }
        if !self.enums.is_empty() { out.push('\n'); }

        for s in &self.structs {
            writeln!(out, "struct {} {{", s.c_name).unwrap();
            for (_, cf, fty) in &s.fields {
                out.push_str("    ");
                self.declare(fty, cf.clone(), &mut out, Span::new(0, 0))?;
                writeln!(out, ";").unwrap();
            }
            writeln!(out, "}};").unwrap();
        }
        if !self.structs.is_empty() { out.push('\n'); }

        for (d, id) in &self.global_lets {
            if !self.is_const(&d.init) {
                return Err(self.err(CodegenErrorKind::NonConstantGlobalInit, d.init.span));
            }
            let ty = self.def_ty(*id, d.name.span)?;
            if matches!(ty, Ty::Array(_, _)) && !matches!(d.init.kind, ExprKind::ArrayInit { .. }) {
                return Err(self.err(CodegenErrorKind::NonConstantGlobalInit, d.init.span));
            }
            let mut val = String::new();
            self.emit_init(&d.init, &ty, &no_locals, &mut val)?;
            out.push_str("static ");
            let ds = self.decl_string(&ty, &self.names[id.index()], d.mutable, d.name.span)?;
            writeln!(out, "{ds} = {val};").unwrap();
        }
        if !self.global_lets.is_empty() { out.push('\n'); }

        if !self.fns.is_empty() { out.push('\n'); }
        for f in &self.fns {
            self.write_sig(f, &mut out)?;
            out.push_str(";\n");
        }
        out.push('\n');
        for f in &self.fns {
            if f.decl.is_extern { continue; }
            self.emit_fn(f, &mut out)?;
            out.push('\n');
        }

        if let Some(i) = self.entry {
            let f = &self.fns[i];
            if f.ret.is_some() {
                writeln!(out, "int main(void) {{\n    return (int){}();\n}}", f.c_name).unwrap();
            } else {
                writeln!(out, "int main(void) {{\n    {}();\n    return 0;\n}}", f.c_name).unwrap();
            }
        }
        Ok(out)
    }

    fn def_ty(&self, id: DefId, span: Span) -> Result<Ty, CodegenError> {
        let ty = self.checked.def_tys[id.index()].clone();
        if matches!(ty, Ty::Unknown(_)) {
            return Err(self.err(CodegenErrorKind::UnknownType(ty.to_string()), span));
        }
        Ok(ty)
    }

    fn c_decl(&self, ty: &Ty, name: &str, span: Span) -> Result<(String, String), CodegenError> {
        let (base, lead, tail) = self.c_decl_parts(ty, name, span)?;
        let decl = if !tail.is_empty() && lead.starts_with('*') {
            format!("({lead}){tail}")
        } else {
            format!("{lead}{tail}")
        };
        Ok((base, decl))
    }

    fn c_decl_parts(&self, ty: &Ty, name: &str, span: Span) -> Result<(String, String, String), CodegenError> {
        match ty {
            Ty::Array(el, n) => {
                let (base, lead, mut tail) = self.c_decl_parts(el, name, span)?;
                let lead = if lead.starts_with('*') { format!("({lead})") } else { lead };
                tail.push_str(&format!("[{n}]"));
                Ok((base, lead, tail))
            }
            Ty::Ref(inner, mutable) => {
                let (base, lead, tail) = self.c_decl_parts(inner, name, span)?;
                let base = if *mutable { base } else { format!("const {base}") };
                Ok((base, format!("*{lead}"), tail))
            }
            other => Ok((self.c_ty(other, span)?, name.to_string(), String::new())),
        }
    }

    fn decl_string(&self, ty: &Ty, name: &str, binding_mutable: bool, span: Span) -> Result<String, CodegenError> {
        let (base, decl) = self.c_decl(ty, name, span)?;
        if binding_mutable || matches!(ty, Ty::Ref(..)) {
            Ok(format!("{base} {decl}"))
        } else {
            Ok(format!("const {base} {decl}"))
        }
    }

    fn declare(&self, ty: &Ty, name: String, out: &mut String, span: Span) -> Result<(), CodegenError> {
        let s = self.decl_string(ty, &name, true, span)?;
        out.push_str(&s);
        Ok(())
    }

    fn write_sig(&self, f: &FnInfo<'a, 'src>, out: &mut String) -> Result<(), CodegenError> {
        let ret = match &f.ret {
            Some(Ty::Array(_, _)) => return Err(self.err(CodegenErrorKind::ArrayReturn, f.decl.name.span)),
            Some(t) => self.c_ty(t, f.decl.name.span)?,
            None => "void".to_string(),
        };
        let linkage = if f.decl.is_extern { "extern" } else { "static" };
        write!(out, "{linkage} {ret} {}(", f.c_name).unwrap();
        if f.decl.params.is_empty() {
            out.push_str("void");
        } else {
            for (i, p) in f.decl.params.iter().enumerate() {
                if i > 0 { out.push_str(", "); }
                let pid = self.def_id(p.name.span);
                let pt = self.def_ty(pid, p.name.span)?;
                self.declare(&pt, self.names[pid.index()].clone(), out, p.name.span)?;
            }
        }
        out.push(')');
        Ok(())
    }

    fn emit_fn(&self, f: &FnInfo<'a, 'src>, out: &mut String) -> Result<(), CodegenError> {
        if f.ret.is_some() && !ends_with_return(&f.decl.body) {
            return Err(self.err(CodegenErrorKind::MissingReturn, f.decl.name.span));
        }
        self.write_sig(f, out)?;
        out.push_str(" {\n");
        for s in &f.decl.body.stmts { self.emit_stmt(s, &f.locals, out, 1)?; }
        out.push_str("}\n");
        Ok(())
    }

    fn emit_stmt(&self, s: &Stmt<'src>, locals: &HashSet<DefId>, out: &mut String, depth: usize) -> Result<(), CodegenError> {
        match s {
            Stmt::Decl(Decl::Fn(_)) | Stmt::Decl(Decl::Struct(_)) | Stmt::Decl(Decl::Enum(_)) => Ok(()),
            Stmt::Decl(Decl::Let(d)) => {
                let id = self.def_id(d.name.span);
                let ty = self.def_ty(id, d.name.span)?;
                let name = self.names[id.index()].clone();
                match &ty {
                    Ty::Array(el, n) if !matches!(d.init.kind, ExprKind::ArrayInit { .. }) => {
                        let et = self.c_ty(el, d.name.span)?;
                        let i = id.index();
                        indent(out, depth);
                        self.declare(&ty, name.clone(), out, d.name.span)?;
                        writeln!(out, ";").unwrap();
                        let mut src = String::new();
                        self.emit_expr(&d.init, locals, &mut src)?;
                        indent(out, depth);
                        writeln!(out, "const {et} *os_src_{i} = {src};").unwrap();
                        indent(out, depth);
                        writeln!(out, "for (uint64_t os_i = 0; os_i < {n}; os_i++) {{").unwrap();
                        indent(out, depth + 1);
                        writeln!(out, "{name}[os_i] = os_src_{i}[os_i];").unwrap();
                        indent(out, depth);
                        out.push_str("}\n");
                        Ok(())
                    }
                    _ => {
                        let mut val = String::new();
                        self.emit_init(&d.init, &ty, locals, &mut val)?;
                        indent(out, depth);
                        let ds = self.decl_string(&ty, &name, d.mutable, d.name.span)?;
                        writeln!(out, "{ds} = {val};").unwrap();
                        Ok(())
                    }
                }
            }
            Stmt::Decl(Decl::Const(d)) => {
                let id = self.def_id(d.name.span);
                let ty = self.def_ty(id, d.name.span)?;
                let mut val = String::new();
                self.emit_init(&d.value, &ty, locals, &mut val)?;
                indent(out, depth);
                let ds = self.decl_string(&ty, &self.names[id.index()], false, d.name.span)?;
                writeln!(out, "{ds} = {val};").unwrap();
                Ok(())
            }
            Stmt::Return { value, .. } => {
                indent(out, depth);
                match value {
                    Some(e) => {
                        let mut s = String::new();
                        self.emit_expr(e, locals, &mut s)?;
                        writeln!(out, "return {s};").unwrap();
                    }
                    None => out.push_str("return;\n"),
                }
                Ok(())
            }
            Stmt::Expr(e) => {
                let mut s = String::new();
                self.emit_expr(e, locals, &mut s)?;
                indent(out, depth);
                writeln!(out, "{s};").unwrap();
                Ok(())
            }
            Stmt::If { condition, then_block, else_block, .. } => {
                let mut cond = String::new();
                self.emit_expr(condition, locals, &mut cond)?;
                indent(out, depth);
                writeln!(out, "if ({cond}) {{").unwrap();
                for x in &then_block.stmts { self.emit_stmt(x, locals, out, depth + 1)?; }
                indent(out, depth);
                match else_block {
                    Some(eb) => {
                        out.push_str("} else {\n");
                        for x in &eb.stmts { self.emit_stmt(x, locals, out, depth + 1)?; }
                        indent(out, depth);
                        out.push_str("}\n");
                    }
                    None => out.push_str("}\n"),
                }
                Ok(())
            }
            Stmt::While { condition, body, .. } => {
                let mut cond = String::new();
                self.emit_expr(condition, locals, &mut cond)?;
                indent(out, depth);
                writeln!(out, "while ({cond}) {{").unwrap();
                for x in &body.stmts { self.emit_stmt(x, locals, out, depth + 1)?; }
                indent(out, depth);
                out.push_str("}\n");
                Ok(())
            }
            Stmt::For { binding, iter, body, span } => {
                self.emit_for(binding, iter, body, *span, locals, out, depth)
            }
            Stmt::Break(_) => { indent(out, depth); out.push_str("break;\n"); Ok(()) }
            Stmt::Continue(_) => { indent(out, depth); out.push_str("continue;\n"); Ok(()) }
            Stmt::Match { scrutinee, arms, .. } => {
                self.emit_match(scrutinee, arms, locals, out, depth)
            }
        }
    }

    fn emit_for(
        &self,
        binding: &Ident<'src>,
        iter: &Expr<'src>,
        body: &Block<'src>,
        span: Span,
        locals: &HashSet<DefId>,
        out: &mut String,
        depth: usize,
    ) -> Result<(), CodegenError> {
        let bid = self.def_id(binding.span);
        let bty = self.def_ty(bid, binding.span)?;
        let bname = self.names[bid.index()].clone();
        let tag = span.start;

        if let ExprKind::Range { start, end } = &iter.kind {
            let mut lo = String::new();
            let mut hi = String::new();
            self.emit_expr(start, locals, &mut lo)?;
            self.emit_expr(end, locals, &mut hi)?;
            let ct = self.c_ty(&bty, binding.span)?;
            let idx = format!("os_f_{tag}");
            indent(out, depth);
            writeln!(out, "for ({ct} {idx} = {lo}; {idx} < {hi}; {idx}++) {{").unwrap();
            indent(out, depth + 1);
            let ds = self.decl_string(&bty, &bname, false, binding.span)?;
            writeln!(out, "{ds} = {idx};").unwrap();
            for x in &body.stmts { self.emit_stmt(x, locals, out, depth + 1)?; }
            indent(out, depth);
            out.push_str("}\n");
            return Ok(());
        }

        let raw = self.checked.expr_tys.get(&iter.span).cloned().unwrap_or(Ty::I32);
        let (elem, len) = match peel_ref(&raw) {
            Ty::Array(el, n) => (*el, n),
            _ => return Err(self.err(CodegenErrorKind::UnknownType(raw.to_string()), iter.span)),
        };
        let et = self.c_ty(&elem, iter.span)?;
        let mut src = String::new();
        self.emit_expr(iter, locals, &mut src)?;
        if matches!(raw, Ty::Ref(..)) {
            src = format!("(*({src}))");
        }
        let arr = format!("os_a_{tag}");
        let idx = format!("os_f_{tag}");
        indent(out, depth);
        writeln!(out, "const {et} *{arr} = {src};").unwrap();
        indent(out, depth);
        writeln!(out, "for (uint64_t {idx} = 0; {idx} < {len}; {idx}++) {{").unwrap();
        indent(out, depth + 1);
        let ds = self.decl_string(&bty, &bname, false, binding.span)?;
        writeln!(out, "{ds} = {arr}[{idx}];").unwrap();
        for x in &body.stmts { self.emit_stmt(x, locals, out, depth + 1)?; }
        indent(out, depth);
        out.push_str("}\n");
        Ok(())
    }

    fn emit_match(
        &self,
        scrutinee: &Expr<'src>,
        arms: &[MatchArm<'src>],
        locals: &HashSet<DefId>,
        out: &mut String,
        depth: usize,
    ) -> Result<(), CodegenError> {
        let ty = self.checked.expr_tys.get(&scrutinee.span).cloned().unwrap_or(Ty::I32);
        let ename = match &ty { Ty::Enum(n) => n.clone(), _ => String::new() };
        let info = self.einfo(&ename);
        let cname = info.map(|i| i.c_name.clone()).unwrap_or_else(|| sanitize(&ename));
        let tmp = format!("os_m_{}", scrutinee.span.start);
        let mut scr = String::new();
        self.emit_expr(scrutinee, locals, &mut scr)?;
        indent(out, depth);
        writeln!(out, "struct {cname} {tmp} = {scr};").unwrap();

        let needs_chain = arms.iter().any(|a| a.wildcard || a.guard.is_some());
        if !needs_chain {
            indent(out, depth);
            writeln!(out, "switch ({tmp}.tag) {{").unwrap();
            for arm in arms {
                let idx = self.tag_of(info, arm.variant.name);
                indent(out, depth);
                writeln!(out, "case {idx}: {{").unwrap();
                self.emit_arm_binding(arm, info, &tmp, out, depth + 1)?;
                for x in &arm.body.stmts { self.emit_stmt(x, locals, out, depth + 1)?; }
                if !ends_with_return(&arm.body) {
                    indent(out, depth + 1);
                    out.push_str("break;\n");
                }
                indent(out, depth);
                out.push_str("}\n");
            }
            indent(out, depth);
            out.push_str("}\n");
            return Ok(());
        }

        // Guards / wildcard: one `if` per arm, gated on a "matched" flag,
        // with the binding declared BEFORE the guard is tested.
        let hit = format!("os_hit_{}", scrutinee.span.start);
        indent(out, depth);
        writeln!(out, "bool {hit} = false;").unwrap();
        for arm in arms {
            let mut outer = format!("!{hit}");
            if !arm.wildcard {
                outer = format!("{outer} && {tmp}.tag == {}", self.tag_of(info, arm.variant.name));
            }
            indent(out, depth);
            writeln!(out, "if ({outer}) {{").unwrap();
            self.emit_arm_binding(arm, info, &tmp, out, depth + 1)?;
            let nested = if let Some(g) = &arm.guard {
                let mut gs = String::new();
                self.emit_expr(g, locals, &mut gs)?;
                indent(out, depth + 1);
                writeln!(out, "if ({gs}) {{").unwrap();
                true
            } else {
                false
            };
            let body_depth = if nested { depth + 2 } else { depth + 1 };
            for x in &arm.body.stmts { self.emit_stmt(x, locals, out, body_depth)?; }
            if !ends_with_return(&arm.body) {
                indent(out, body_depth);
                writeln!(out, "{hit} = true;").unwrap();
            }
            if nested {
                indent(out, depth + 1);
                out.push_str("}\n");
            }
            indent(out, depth);
            out.push_str("}\n");
            if arm.wildcard && arm.guard.is_none() { break; }
        }
        Ok(())
    }

    fn emit_arm_binding(
        &self,
        arm: &MatchArm<'src>,
        info: Option<&EnumInfo>,
        tmp: &str,
        out: &mut String,
        depth: usize,
    ) -> Result<(), CodegenError> {
        let Some(b) = &arm.binding else { return Ok(()) };
        let Some(i) = info else { return Ok(()) };
        if let Some((_, member, Some(t))) = i.variants.iter().find(|(v, _, _)| v == arm.variant.name) {
            let bid = self.def_id(b.span);
            let ds = self.decl_string(t, &self.names[bid.index()], true, b.span)?;
            indent(out, depth);
            writeln!(out, "{ds} = {tmp}.data.{member};").unwrap();
        }
        Ok(())
    }

    fn tag_of(&self, info: Option<&EnumInfo>, variant: &str) -> usize {
        info.and_then(|i| i.variants.iter().position(|(v, _, _)| v == variant)).unwrap_or(0)
    }

    fn emit_init(&self, e: &Expr<'src>, ty: &Ty, locals: &HashSet<DefId>, out: &mut String) -> Result<(), CodegenError> {
        match (&e.kind, ty) {
            (ExprKind::ArrayInit { elements }, Ty::Array(el, _)) => {
                out.push('{');
                for (i, x) in elements.iter().enumerate() {
                    if i > 0 { out.push_str(", "); }
                    self.emit_init(x, el, locals, out)?;
                }
                out.push('}');
                Ok(())
            }
            (ExprKind::StructInit { fields, .. }, Ty::Struct(s)) => {
                let info = self.info(s);
                out.push('{');
                for (i, (fname, v)) in fields.iter().enumerate() {
                    if i > 0 { out.push_str(", "); }
                    let (cf, fty) = match info {
                        Some(si) => si.fields.iter().find(|(f, _, _)| f == &fname.name)
                            .map(|(_, cf, t)| (cf.clone(), t.clone()))
                            .unwrap_or_else(|| (sanitize(fname.name), Ty::I32)),
                        None => (sanitize(fname.name), Ty::I32),
                    };
                    write!(out, ".{cf} = ").unwrap();
                    self.emit_init(v, &fty, locals, out)?;
                }
                out.push('}');
                Ok(())
            }
            _ => self.emit_expr(e, locals, out),
        }
    }

    fn emit_expr(&self, e: &Expr<'src>, locals: &HashSet<DefId>, out: &mut String) -> Result<(), CodegenError> {
        match &e.kind {
            ExprKind::Int(n) => {
                let suffix = match self.checked.expr_tys.get(&e.span) {
                    Some(Ty::U64) => "ULL",
                    Some(Ty::I64) => "LL",
                    Some(Ty::U32 | Ty::U16 | Ty::U8) => "U",
                    _ => "",
                };
                write!(out, "{n}{suffix}").unwrap();
                Ok(())
            }
            ExprKind::Bool(b) => { out.push_str(if *b { "true" } else { "false" }); Ok(()) }
            ExprKind::Str(s) => {
                out.push('"');
                for ch in s.chars() {
                    match ch {
                        '"' => out.push_str("\\\""),
                        '\\' => out.push_str("\\\\"),
                        '\n' => out.push_str("\\n"),
                        '\t' => out.push_str("\\t"),
                        '\0' => out.push_str("\\0"),
                        c => out.push(c),
                    }
                }
                out.push('"');
                Ok(())
            }
            ExprKind::Char(c) => {
                let c = *c;
                out.push('\'');
                match c {
                    '\'' => out.push_str("\\'"),
                    '\\' => out.push_str("\\\\"),
                    '\n' => out.push_str("\\n"),
                    '\t' => out.push_str("\\t"),
                    '\0' => out.push_str("\\0"),
                    ch => out.push(ch),
                }
                out.push('\'');
                Ok(())
            }
            ExprKind::Ident(_) => {
                let id = self.use_id(e.span)?;
                self.check_capture(id, locals, e.span)?;
                out.push_str(&self.names[id.index()]);
                Ok(())
            }
            ExprKind::Unary { op, operand } => {
                out.push('(');
                out.push_str(op.symbol());
                self.emit_expr(operand, locals, out)?;
                out.push(')');
                Ok(())
            }
            ExprKind::Binary { op, lhs, rhs } => {
                out.push('(');
                self.emit_expr(lhs, locals, out)?;
                out.push(' ');
                out.push_str(op.symbol());
                out.push(' ');
                self.emit_expr(rhs, locals, out)?;
                out.push(')');
                Ok(())
            }
            ExprKind::Assign { target, value } => {
                out.push('(');
                self.emit_expr(target, locals, out)?;
                out.push_str(" = ");
                self.emit_expr(value, locals, out)?;
                out.push(')');
                Ok(())
            }
            ExprKind::Call { callee, args } => {
                let id = match &callee.kind {
                    ExprKind::Ident(_) => self.use_id(callee.span)?,
                    _ => return Err(self.err(CodegenErrorKind::IndirectCall, callee.span)),
                };
                if self.resolved.defs[id.index()].kind != DefKind::Function {
                    return Err(self.err(CodegenErrorKind::IndirectCall, callee.span));
                }
                out.push_str(&self.names[id.index()]);
                out.push('(');
                for (i, a) in args.iter().enumerate() {
                    if i > 0 { out.push_str(", "); }
                    self.emit_expr(a, locals, out)?;
                }
                out.push(')');
                Ok(())
            }
            ExprKind::ArrayInit { .. } => {
                let ty = self.checked.expr_tys.get(&e.span).cloned().unwrap_or(Ty::I32);
                match &ty {
                    Ty::Array(el, n) => {
                        let et = self.c_ty(el, e.span)?;
                        write!(out, "({et}[{n}])").unwrap();
                        self.emit_init(e, &ty, locals, out)?;
                        Ok(())
                    }
                    _ => self.emit_expr(e, locals, out),
                }
            }
            ExprKind::StructInit { name, .. } => {
                let ty = self.checked.expr_tys.get(&e.span).cloned()
                    .unwrap_or_else(|| Ty::Struct(name.name.to_string()));
                let cn = match &ty {
                    Ty::Struct(s) => self.info(s).map(|i| i.c_name.clone()).unwrap_or_else(|| sanitize(s)),
                    _ => sanitize(name.name),
                };
                write!(out, "(struct {cn})").unwrap();
                self.emit_init(e, &ty, locals, out)?;
                Ok(())
            }
            ExprKind::Index { array, index } => {
                out.push('(');
                self.emit_expr(array, locals, out)?;
                out.push('[');
                self.emit_expr(index, locals, out)?;
                out.push_str("])");
                Ok(())
            }
            ExprKind::Field { object, field } => {
                out.push('(');
                self.emit_expr(object, locals, out)?;
                let ot = self.checked.expr_tys.get(&object.span).cloned();
                let (arrow, sname) = match &ot {
                    Some(Ty::Ref(inner, _)) => (true, match &**inner {
                        Ty::Struct(s) => Some(s.clone()),
                        _ => None,
                    }),
                    Some(Ty::Struct(s)) => (false, Some(s.clone())),
                    _ => (false, None),
                };
                let cf = sname.as_deref().and_then(|s| self.info(s))
                    .and_then(|si| si.fields.iter().find(|(f, _, _)| f == &field.name).map(|(_, cf, _)| cf.clone()))
                    .unwrap_or_else(|| sanitize(field.name));
                out.push_str(if arrow { "->" } else { "." });
                out.push_str(&cf);
                out.push(')');
                Ok(())
            }
            ExprKind::Borrow { operand, .. } => {
                out.push_str("(&(");
                self.emit_expr(operand, locals, out)?;
                out.push_str("))");
                Ok(())
            }
            ExprKind::Deref(inner) => {
                out.push_str("(*(");
                self.emit_expr(inner, locals, out)?;
                out.push_str("))");
                Ok(())
            }
            ExprKind::Cast { operand, ty } => {
                let t = self.c_ty(&named_ty(ty), e.span)?;
                out.push_str("((");
                out.push_str(&t);
                out.push_str(")(");
                self.emit_expr(operand, locals, out)?;
                out.push_str("))");
                Ok(())
            }
            ExprKind::EnumInit { enum_name, variant, payload } => {
                let info = self.einfo(enum_name.name);
                let cname = info.map(|i| i.c_name.clone()).unwrap_or_else(|| sanitize(enum_name.name));
                let idx = self.tag_of(info, variant.name);
                write!(out, "((struct {cname}){{ .tag = {idx}").unwrap();
                if let (Some(p), Some(i)) = (payload, info) {
                    if let Some((_, member, Some(_))) = i.variants.iter().find(|(v, _, _)| v == variant.name) {
                        write!(out, ", .data.{member} = ").unwrap();
                        self.emit_expr(p, locals, out)?;
                    }
                }
                out.push_str(" })");
                Ok(())
            }
            ExprKind::Try(inner) => {
                let Some(info) = self.checked.try_info.get(&e.span) else {
                    return Err(self.err(CodegenErrorKind::Unresolved, e.span));
                };
                let ei = self.einfo(&info.enum_name);
                let cname = ei.map(|i| i.c_name.clone()).unwrap_or_else(|| sanitize(&info.enum_name));
                let member = |name: &str| -> String {
                    ei.and_then(|i| i.variants.iter().find(|(v, _, _)| v == name))
                        .map(|(_, m, _)| m.clone())
                        .unwrap_or_else(|| sanitize(name))
                };
                let err_m = member(&info.err_variant);
                let ok_m = member(&info.ok_variant);
                let err_has_payload = ei
                    .and_then(|i| i.variants.iter().find(|(v, _, _)| v == &info.err_variant))
                    .map(|(_, _, t)| t.is_some())
                    .unwrap_or(false);
                let tmp = format!("os_try_{}", e.span.start);
                out.push_str("({\n");
                out.push_str(&format!("    struct {cname} {tmp} = "));
                self.emit_expr(inner, locals, out)?;
                out.push_str(";\n");
                if err_has_payload {
                    out.push_str(&format!(
                        "    if ({tmp}.tag == 0) return ((struct {cname}){{ .tag = 0, .data.{err_m} = {tmp}.data.{err_m} }});\n"
                    ));
                } else {
                    out.push_str(&format!(
                        "    if ({tmp}.tag == 0) return ((struct {cname}){{ .tag = 0 }});\n"
                    ));
                }
                out.push_str(&format!("    {tmp}.data.{ok_m};\n"));
                out.push_str("})");
                Ok(())
            }
            ExprKind::Range { .. } => Err(self.err(CodegenErrorKind::Unresolved, e.span)),
        }
    }

    fn info(&self, orig: &str) -> Option<&StructInfo> { self.structs.iter().find(|s| s.orig == orig) }
    fn einfo(&self, orig: &str) -> Option<&EnumInfo> { self.enums.iter().find(|e| e.orig == orig) }

    fn check_capture(&self, id: DefId, locals: &HashSet<DefId>, span: Span) -> Result<(), CodegenError> {
        let def = &self.resolved.defs[id.index()];
        if def.kind == DefKind::Function || self.globals.contains(&id) || locals.contains(&id) {
            Ok(())
        } else {
            Err(self.err(CodegenErrorKind::Capture { name: def.name.to_string() }, span))
        }
    }

    fn is_const(&self, e: &Expr<'src>) -> bool {
        match &e.kind {
            ExprKind::Int(_) | ExprKind::Bool(_) | ExprKind::Str(_) | ExprKind::Char(_) => true,
            ExprKind::Unary { operand, .. } => self.is_const(operand),
            ExprKind::Binary { lhs, rhs, .. } => self.is_const(lhs) && self.is_const(rhs),
            ExprKind::ArrayInit { elements } => elements.iter().all(|x| self.is_const(x)),
            ExprKind::StructInit { fields, .. } => fields.iter().all(|(_, v)| self.is_const(v)),
            ExprKind::EnumInit { payload, .. } => payload.as_ref().map_or(true, |p| self.is_const(p)),
            ExprKind::Cast { operand, .. } => self.is_const(operand),
            ExprKind::Ident(_) => self.uses.get(&e.span).is_some_and(|id| {
                self.resolved.defs[id.index()].kind == DefKind::Constant && self.globals.contains(id)
            }),
            _ => false,
        }
    }

    fn fresh_name(&mut self, base: &str) -> String {
        let mut cand = sanitize(base);
        if self.used.contains(&cand) { cand = format!("{cand}_{}", self.used.len()); }
        self.used.insert(cand.clone());
        cand
    }

    fn fresh(&mut self, base: &str, id: DefId) -> String {
        let b = sanitize(base);
        let mut cand = b.clone();
        if self.used.contains(&cand) { cand = format!("{b}_{}", id.index()); }
        while self.used.contains(&cand) { cand = format!("v{}_{}", id.index(), self.used.len()); }
        self.used.insert(cand.clone());
        cand
    }

    fn def_id(&self, span: Span) -> DefId {
        *self.def_at.get(&span).expect("definition span recorded by resolver")
    }

    fn use_id(&self, span: Span) -> Result<DefId, CodegenError> {
        self.uses.get(&span).copied().ok_or_else(|| self.err(CodegenErrorKind::Unresolved, span))
    }

    fn err(&self, kind: CodegenErrorKind, span: Span) -> CodegenError { CodegenError { kind, span } }

    fn c_ty(&self, ty: &Ty, span: Span) -> Result<String, CodegenError> {
        Ok(match ty {
            Ty::I8 => "int8_t".into(), Ty::I16 => "int16_t".into(),
            Ty::I32 => "int32_t".into(), Ty::I64 => "int64_t".into(),
            Ty::U8 => "uint8_t".into(), Ty::U16 => "uint16_t".into(),
            Ty::U32 => "uint32_t".into(), Ty::U64 => "uint64_t".into(),
            Ty::Bool => "bool".into(), Ty::Str => "const char*".into(),
            Ty::Char => "char".into(), Ty::Unit => "void".into(),
            Ty::Struct(s) => format!("struct {}", self.info(s).map(|i| i.c_name.clone()).unwrap_or_else(|| sanitize(s))),
            Ty::Enum(n) => format!("struct {}", self.einfo(n).map(|i| i.c_name.clone()).unwrap_or_else(|| sanitize(n))),
            Ty::Ref(inner, m) => {
                let it = self.c_ty(inner, span)?;
                if *m { format!("{it}*") } else { format!("const {it}*") }
            }
            Ty::Array(_, _) => return Err(self.err(CodegenErrorKind::UnknownType(ty.to_string()), span)),
            Ty::Unknown(n) => return Err(self.err(CodegenErrorKind::UnknownType(n.clone()), span)),
        })
    }
}

fn peel_ref(t: &Ty) -> Ty {
    match t { Ty::Ref(i, _) => (**i).clone(), other => other.clone() }
}

fn is_type_decl(s: &Stmt<'_>) -> bool {
    matches!(s, Stmt::Decl(Decl::Fn(_)) | Stmt::Decl(Decl::Struct(_)) | Stmt::Decl(Decl::Enum(_)))
}

fn ends_with_return(b: &Block<'_>) -> bool {
    let last = b.stmts.iter().rev().find(|s| !is_type_decl(s));
    match last {
        Some(Stmt::Return { .. }) => true,
        Some(Stmt::If { then_block, else_block: Some(eb), .. }) => {
            ends_with_return(then_block) && ends_with_return(eb)
        }
        Some(Stmt::Match { arms, .. }) => {
            let fallback = arms.iter().any(|a| a.wildcard && a.guard.is_none());
            let unguarded = arms.iter().all(|a| a.guard.is_none());
            !arms.is_empty() && (fallback || unguarded) && arms.iter().all(|a| ends_with_return(&a.body))
        }
        _ => false,
    }
}

fn indent(out: &mut String, depth: usize) {
    for _ in 0..depth { out.push_str("    "); }
}

fn sanitize(name: &str) -> String {
    let mut s = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' { s.push(ch); }
        else { write!(s, "_{:x}", ch as u32).unwrap(); }
    }
    if RESERVED.contains(&s.as_str()) { format!("k_{s}") } else { s }
}

fn named_ty(t: &Type<'_>) -> Ty {
    match t {
        Type::Named(id) => match id.name {
            "i8" => Ty::I8, "i16" => Ty::I16, "i32" => Ty::I32, "i64" => Ty::I64,
            "u8" => Ty::U8, "u16" => Ty::U16, "u32" => Ty::U32, "u64" => Ty::U64,
            "bool" => Ty::Bool, "str" => Ty::Str, "char" => Ty::Char,
            n => Ty::Unknown(n.to_string()),
        },
        Type::Array { element, size } => Ty::Array(Box::new(named_ty(element)), *size),
        Type::Ref { mutable, inner } => Ty::Ref(Box::new(named_ty(inner)), *mutable),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::resolve::resolve;
    use crate::typeck::type_check;

    fn gen(src: &str) -> String {
        let p = parse(src).unwrap();
        let r = resolve(&p).unwrap();
        let c = type_check(&p, &r).unwrap();
        compile_to_c(&p, &r, &c).unwrap()
    }
    fn gen_err(src: &str) -> CodegenErrorKind {
        let p = parse(src).unwrap();
        let r = resolve(&p).unwrap();
        let c = type_check(&p, &r).unwrap();
        compile_to_c(&p, &r, &c).unwrap_err().kind
    }

    #[test]
    fn functions_and_ffi() {
        let c = gen("fn add(a: i32, b: i32) -> i32 { return a + b; } fn main() -> i32 { return add(20, 2); }");
        assert!(c.contains("static int32_t add(int32_t a, int32_t b)"));
        assert!(c.contains("return (int)oster_main();"));
        let e = gen(r#"extern fn puts(s: str) -> i32; fn main() -> i32 { puts("hi"); return 0; }"#);
        assert!(e.contains("extern int32_t puts(const char* s);"));
    }

    #[test]
    fn void_main_wrapper() {
        let c = gen("fn main() { return; }");
        assert!(c.contains("static void oster_main(void)"));
        assert!(c.contains("oster_main();"));
        assert!(c.contains("return 0;"));
    }

    #[test]
    fn aggregates_and_refs() {
        let a = gen("fn main() -> i32 { let a = [1, 2, 3]; return a[1]; }");
        assert!(a.contains("int32_t a[3] = {1, 2, 3};"));
        let s = gen("struct P { x: i32 } fn get(p: &P) -> i32 { return p.x; } fn main() -> i32 { let q = P { x: 7 }; return get(&q); }");
        assert!(s.contains("const struct P *p"));
        assert!(s.contains("(p->x)"));
        let r = gen("fn first(a: &[i32; 2]) -> i32 { return a[0]; } fn main() -> i32 { let a = [5, 6]; return first(&a); }");
        assert!(r.contains("const int32_t (*a)[2]"));
    }

    #[test]
    fn enums_and_match() {
        let c = gen("enum Opt { None, Some(i32) } fn f(o: Opt) -> i32 { match o { Opt::None => { return 0; } Opt::Some(v) => { return v; } } } fn main() -> i32 { return f(Opt::Some(42)); }");
        assert!(c.contains("int32_t tag;"));
        assert!(c.contains("switch (os_m_"));
        assert!(c.contains(".data.Some"));
        // No dead `break` after a returning arm.
        assert!(!c.contains("return v;\n        break;"));
    }

    #[test]
    fn wildcard_and_guard_use_if_chain() {
        let c = gen("enum E { A(i32), B } fn f(e: E) -> i32 { match e { E::A(v) if v > 0 => { return v; } _ => { return 0; } } }");
        assert!(c.contains("bool os_hit_"));
        assert!(c.contains(".tag == 0"));
        assert!(!c.contains("switch"));
        // Regression: the binding must be declared *before* the guard reads it.
        let bind = c.find("int32_t v =").expect("binding emitted");
        let guard = c.find("(v > 0)").expect("guard emitted");
        assert!(bind < guard, "binding must precede guard");
    }

    #[test]
    fn for_range_and_array() {
        let r = gen("fn f() -> i32 { mut let s = 0; for i in 0..3 { s += i; } return s; }");
        assert!(r.contains("for (int32_t os_f_"));
        let a = gen("fn f() -> i32 { let a = [1, 2]; mut let s = 0; for x in a { s += x; } return s; }");
        assert!(a.contains("const int32_t *os_a_"));
        assert!(a.contains("for (uint64_t os_f_"));
        let p = gen("fn f(a: &[i32; 2]) -> i32 { mut let s = 0; for x in a { s += x; } return s; }");
        assert!(p.contains("(*(a))"));
    }

    #[test]
    fn try_operator_emits_c() {
        let c = gen("enum Result { Err(i32), Ok(i32) } fn f() -> Result { let x = g()?; return Result::Ok(x); } fn g() -> Result { return Result::Ok(42); }");
        assert!(c.contains("os_try_"));
        assert!(c.contains(".tag == 0"));
        assert!(c.contains("return ((struct Result)"));
    }

    #[test]
    fn casts_and_flow() {
        assert!(gen("fn main() -> i32 { return '!' as i32; }").contains("((int32_t)('!'))"));
        let c = gen("fn f() -> i32 { mut let i = 0; while i < 3 { i += 1; if i > 2 { break; } continue; } return i; }");
        assert!(c.contains("break;"));
        assert!(c.contains("continue;"));
    }

    #[test]
    fn rejects_bad_programs() {
        assert!(matches!(
            gen_err("fn f() -> i32 { let x = 1; fn g() -> i32 { return x; } return g(); }"),
            CodegenErrorKind::Capture { .. }
        ));
        assert!(matches!(
            gen_err("fn f() -> i32 { mut let x = 1; } fn main() { return; }"),
            CodegenErrorKind::MissingReturn
        ));
    }
}