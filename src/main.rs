mod lexer;
mod parser;

fn main() {
    println!("Hello, world!");
    let func = parser::parse("i32 test(i32 arg1)");
    println!("{:#?}", func);
}
