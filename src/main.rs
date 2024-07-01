mod lexer;
mod parser;
mod parse_tree;

fn main() {
    let func = parser::parse("
fn test() -> void {
    var a: i32 = 2;
}");
    println!("{:#?}", func);

    
}
