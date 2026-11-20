extern fn puts(s: str) -> i32;

enum Result {
    Err(str),
    Ok(i32),
}

fn divide(a: i32, b: i32) -> Result {
    if b == 0 { return Result::Err("division by zero"); }
    return Result::Ok(a / b);
}

fn process() -> Result {
    let x = divide(10, 2)?;
    let y = divide(x, 0)?;
    return Result::Ok(y);
}

fn main() -> i32 {
    let r = process();
    match r {
        Result::Err(msg) => {
            puts(msg);
            return 1;
        }
        Result::Ok(v) => {
            return v;
        }
    }
}
