extern fn puts(s: str) -> i32;

enum Result { Err(str), Ok(i32) }

fn checked_div(a: i32, b: i32) -> Result {
    if b == 0 { return Result::Err("division by zero"); }
    return Result::Ok(a / b);
}

fn sum_divs() -> Result {
    mut let total = 0;
    for d in [2, 4, 5] {
        total += checked_div(100, d)?;
    }
    return Result::Ok(total);
}

fn classify(n: i32) -> str {
    match Result::Ok(n) {
        Result::Ok(v) if v > 50 => { return "big"; }
        Result::Ok(v) if v > 0  => { return "small"; }
        _                     => { return "other"; }
    }
}

fn main() -> i32 {
    mut let squares = 0;
    for i in 0..5 { squares += i * i; }

    match sum_divs() {
        Result::Err(msg) => { puts(msg); return 1; }
        Result::Ok(v)    => { puts(classify(v)); }
    }
    return squares;
}
