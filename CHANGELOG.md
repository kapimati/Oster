# Changelog

## [0.1.0] - 2024-11-20

### Added
- Initial release
- Type system with integers, bool, str, char, arrays, structs, enums, references
- Control flow: if/else, while, for, break/continue, match
- Ownership: move semantics, NLL-lite borrow checker, escape analysis
- Error handling: Result-like enums + `?` operator
- FFI: `extern fn` for calling C functions
- Pattern matching: wildcards `_`, guards `if`
- Backend: portable C11
