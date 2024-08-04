use color_print::cprintln;

use crate::parse_tree::{TypePT, BinOpNodePT};

pub fn log(message: &str) {
    println!("{}", message);
}

pub fn warn(message: &str) {
    cprintln!("<yellow>warning {}</yellow>", message);
}

pub fn error(message: &str) {
    cprintln!("<red>error {}</red>", message);
}

macro_rules! error {
    ($($arg:tt)*) => {
        cprintln!("<red>error {}</red>", format!($($arg)*));
    };
}

pub fn use_of_undeclared_type<'a, 'b>(ty: &'a TypePT) {
    error!("use of undeclared type `{}`", ty.identifier.name);
}

pub fn bin_op_mismatched_types(op_node: &BinOpNodePT, lhs: &TypePT, rhs: &TypePT) {
    error!("mismatched types in binary operation `{}`: expected `{}`, found `{}`", op_node.op, lhs.identifier.name, rhs.identifier.name);

}