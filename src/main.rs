mod lexer;
mod parser;
mod parse_tree;

fn main() {
    let func = parser::parse("
    i32 test(i32 arg1) {
        <- 2;
    }");
    println!("{:#?}", func);

    
}
