extern fn puts(s: str) -> i32;

enum Shape { Circle(i32), Rect(i32), Empty }

fn describe(s: Shape) -> str {
    match s {
        Shape::Circle(r) if r > 10 => { return "big circle"; }
        Shape::Circle(r)           => { return "circle"; }
        Shape::Rect(w) if w == 0   => { return "degenerate"; }
        Shape::Rect(w)             => { return "rect"; }
        _                          => { return "empty"; }
    }
}

fn main() -> i32 {
    let shapes = [Shape::Circle(20), Shape::Circle(3), Shape::Rect(0), Shape::Empty];
    for s in shapes { puts(describe(s)); }
    return 0;
}
