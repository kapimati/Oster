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
