# Oster

A small systems language that compiles to portable C11.

## Features

- **Type system**: integers (i8-i64, u8-u64), bool, str, char, arrays, structs, enums, references
- **Control flow**: if/else, while, for (arrays and ranges), break/continue, match with exhaustive checking
- **Ownership**: move semantics, NLL-lite borrow checker, escape analysis
- **Error handling**: Result-like enums + `?` operator
- **FFI**: `extern fn` for calling C functions
- **Pattern matching**: wildcards `_`, guards `if`

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile and run a program
./target/release/oster examples/hello.os -o hello
./hello

## Examples

### Hello World with FFI
```oster
extern fn puts(s: str) -> i32;

fn main() -> i32 {
    puts("Hello from Oster!");
    return 0;
}
```

### Error Handling with `?`
```oster
enum Result { Err(str), Ok(i32) }

fn divide(a: i32, b: i32) -> Result {
    if b == 0 { return Result::Err("division by zero"); }
    return Result::Ok(a / b);
}

fn main() -> i32 {
    let x = divide(10, 2)?;
    let y = divide(x, 0)?;
    return y;
}
```

### Pattern Matching with Guards
```oster
enum Shape { Circle(i32), Rect(i32), Empty }

fn describe(s: Shape) -> str {
    match s {
        Shape::Circle(r) if r > 10 => { return "big circle"; }
        Shape::Circle(r)           => { return "circle"; }
        _                          => { return "other"; }
    }
}
```
