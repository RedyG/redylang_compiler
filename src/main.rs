
use bumpalo::Bump;
use bytecode::Instruction::*;

mod types;
mod logger;
mod lexer;
mod parser;
mod parse_tree;
mod ast;
mod bytecode;

fn compile_fib() {
    let main = bytecode::Func { args_count: 0, locals_count: 0, instructions: vec![
        I8Const(10),
        Call(1),
        Exit,
    ]};
    let fib = bytecode::Func { args_count: 1, locals_count: 0, instructions: vec![
        LocalGet(0),
        I8Const(2),
        I32Lt,
        BrFalse(4),

        LocalGet(0),
        Ret,

        LocalGet(0),
        I8Const(1),
        I32Sub,
        Call(1),

        LocalGet(0),
        I8Const(2),
        I32Sub,
        Call(1),

        I32Add,
        Ret,
    ]};

    let bump = Bump::new();
    let main_alloc = bump.alloc(main);
    let fib_alloc = bump.alloc(fib);

    let module = bytecode::Module { funcs: vec![main_alloc, fib_alloc] };

    module.write_to("test.redy");
}

fn main() {
    let arena = Bump::new();
    let func = parser::parse("
        fn test() -> void {
            var a: i32 = 2;
        }", &arena
    );
    println!("{:#?}", func);

    compile_fib();
}
