//! Oster compiler driver.
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::{env, fs};

use oster::codegen::compile_to_c;
use oster::lexer::{Lexer, Span};
use oster::own::check_ownership;
use oster::parser::parse;
use oster::resolve::resolve;
use oster::typeck::type_check;

const USAGE: &str = "\
Oster — a small systems language that compiles to C11.

USAGE:
    oster <input.os> [options]

OPTIONS:
    -o <path>        output: `*.c` writes C, anything else builds a binary via cc
    --emit-tokens    print the token stream and exit
    --emit-ast       print the AST and exit
    --emit-c         print generated C to stdout (writes no file)
    --run            build to a temporary binary and execute it
    --verbose        dump every compiler stage
    -h, --help       show this message

With no options, `oster foo.os` writes `foo.c` next to the input.
";

#[derive(Default)]
struct Args {
    input: Option<String>,
    output: Option<String>,
    emit: Option<&'static str>,
    run: bool,
    verbose: bool,
    help: bool,
}

fn parse_args() -> Result<Args, String> {
    let mut a = Args::default();
    let mut it = env::args().skip(1);
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "-h" | "--help" => a.help = true,
            "-o" => a.output = Some(it.next().ok_or("`-o` needs a path")?),
            "--emit-tokens" => a.emit = Some("tokens"),
            "--emit-ast" => a.emit = Some("ast"),
            "--emit-c" => a.emit = Some("c"),
            "--run" => a.run = true,
            "--verbose" => a.verbose = true,
            other if other.starts_with('-') => return Err(format!("unknown option `{other}`")),
            other => {
                if a.input.is_some() {
                    return Err("only one input file is supported".into());
                }
                a.input = Some(other.to_string());
            }
        }
    }
    Ok(a)
}

fn main() -> ExitCode {
    let args = match parse_args() {
        Ok(a) => a,
        Err(e) => {
            eprintln!("error: {e}\n\n{USAGE}");
            return ExitCode::FAILURE;
        }
    };
    if args.help || args.input.is_none() {
        print!("{USAGE}");
        return if args.help { ExitCode::SUCCESS } else { ExitCode::FAILURE };
    }
    let path = args.input.clone().unwrap();
    let src = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read `{path}`: {e}");
            return ExitCode::FAILURE;
        }
    };
    match run(&path, &src, &args) {
        Ok(code) => code,
        Err(()) => ExitCode::FAILURE,
    }
}

fn run(path: &str, src: &str, args: &Args) -> Result<ExitCode, ()> {
    let fail = |span: Span, msg: &str| {
        print_error(src, span, msg);
    };

    if args.emit == Some("tokens") || args.verbose {
        println!("-- tokens --");
        for t in Lexer::new(src) {
            println!("{:<12} {:?}", format!("{:?}", t.kind), &src[t.span.range()]);
        }
        if args.emit == Some("tokens") { return Ok(ExitCode::SUCCESS); }
    }

    let program = match parse(src) {
        Ok(p) => p,
        Err(e) => { fail(e.span, &e.to_string()); return Err(()); }
    };
    if args.emit == Some("ast") || args.verbose {
        println!("-- ast --\n{program}");
        if args.emit == Some("ast") { return Ok(ExitCode::SUCCESS); }
    }

    let resolved = match resolve(&program) {
        Ok(r) => r,
        Err(e) => { fail(e.span, &e.to_string()); return Err(()); }
    };
    if args.verbose {
        println!("-- resolution --");
        for (i, d) in resolved.defs.iter().enumerate() {
            println!("def#{i}: {:?} `{}` mutable={}", d.kind, d.name, d.mutable);
        }
    }

    let checked = match type_check(&program, &resolved) {
        Ok(c) => c,
        Err(e) => { fail(e.span, &e.to_string()); return Err(()); }
    };
    if args.verbose {
        println!("-- types --");
        for (i, t) in checked.def_tys.iter().enumerate() {
            println!("def#{i}: {t}");
        }
    }

    if let Err(e) = check_ownership(&program, &resolved, &checked) {
        fail(e.span, &e.to_string());
        return Err(());
    }
    if args.verbose { println!("-- ownership & borrows --\nok"); }

    let c_source = match compile_to_c(&program, &resolved, &checked) {
        Ok(c) => c,
        Err(e) => { fail(e.span, &e.to_string()); return Err(()); }
    };

    if args.emit == Some("c") {
        print!("{c_source}");
        return Ok(ExitCode::SUCCESS);
    }
    if args.verbose { println!("-- c --\n{c_source}"); }

    let input = Path::new(path);
    let stem = input.file_stem().map(|s| s.to_string_lossy().into_owned()).unwrap_or_else(|| "out".into());

    // --run: build to a temp binary, execute, propagate its exit code.
    if args.run {
        let tmp = env::temp_dir().join(format!("oster_{stem}_{}", std::process::id()));
        let c_path = tmp.with_extension("c");
        if write_file(&c_path, &c_source).is_err() { return Err(()); }
        let built = cc(&c_path, &tmp);
        let _ = fs::remove_file(&c_path);
        if !built { return Err(()); }
        let status = Command::new(&tmp).status();
        let _ = fs::remove_file(&tmp);
        return match status {
            Ok(s) => Ok(ExitCode::from(s.code().unwrap_or(1) as u8)),
            Err(e) => { eprintln!("error: cannot run `{}`: {e}", tmp.display()); Err(()) }
        };
    }

    // -o with a non-.c path means "build a binary".
    match &args.output {
        Some(out) if !out.ends_with(".c") => {
            let out_path = PathBuf::from(out);
            let c_path = out_path.with_extension("c");
            if write_file(&c_path, &c_source).is_err() { return Err(()); }
            let ok = cc(&c_path, &out_path);
            let _ = fs::remove_file(&c_path);
            if !ok { return Err(()); }
            eprintln!("built `{}`", out_path.display());
        }
        Some(out) => {
            if write_file(Path::new(out), &c_source).is_err() { return Err(()); }
            eprintln!("wrote C to `{out}`");
        }
        None => {
            let c_path = input.with_extension("c");
            if write_file(&c_path, &c_source).is_err() { return Err(()); }
            eprintln!("wrote C to `{}`", c_path.display());
        }
    }
    Ok(ExitCode::SUCCESS)
}

fn write_file(path: &Path, contents: &str) -> Result<(), ()> {
    fs::write(path, contents).map_err(|e| {
        eprintln!("error: cannot write `{}`: {e}", path.display());
    })
}

fn cc(c_path: &Path, out: &Path) -> bool {
    let status = Command::new("cc")
        .arg("-std=c11")
        .arg("-O2")
        .arg("-o").arg(out)
        .arg(c_path)
        .status();
    match status {
        Ok(s) if s.success() => true,
        Ok(s) => { eprintln!("error: cc failed with {s}"); false }
        Err(e) => { eprintln!("error: cannot run `cc`: {e}"); false }
    }
}

fn print_error(src: &str, span: Span, message: &str) {
    let (line, col) = line_col(src, span.start);
    eprintln!("error: {message} at {line}:{col}");
    if let Some(text) = src.lines().nth(line - 1) {
        eprintln!("  {text}");
        eprintln!("  {:>width$}", "^", width = col);
    }
}

fn line_col(src: &str, offset: u32) -> (usize, usize) {
    let off = (offset as usize).min(src.len());
    let before = &src[..off];
    let line = before.matches('\n').count() + 1;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    (line, before[line_start..].chars().count() + 1)
}