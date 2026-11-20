extern fn puts(s: str) -> i32;
extern fn putchar(c: i32) -> i32;

fn main() -> i32 {
    puts("Hello from Oster");
    putchar('!' as i32);
    putchar('\n' as i32);
    return 0;
}
