use crate::{bytecode::{Func, Instruction::*}, parse_tree::BinOp};

pub trait ExprAST {
    fn codegen(&self, func: &mut Func);
    fn ty(&self) -> &TypeAST;
}
pub trait StatementAST {
    fn codegen(&self, func: &mut Func);
}

pub enum TypeAST<'c> {
    Struct { fields: Vec<&'c FieldAST<'c>> },
    
}

pub struct FieldAST<'c> {
    pub ty: &'c TypeAST<'c>
}

pub struct IntAST {
    pub value: i32
}

pub struct VarUseAST<'c> {
    pub var: &'c VarAST<'c>
}

pub struct BinOpAST<'c> {
    pub lhs: &'c dyn ExprAST,
    pub rhs: &'c dyn ExprAST,
    pub op: BinOp,
    pub ty: &'c TypeAST<'c>
}

impl ExprAST for IntAST {
    fn codegen(&self, func: &mut Func) {
        func.push(I32Const(self.value));
    }
    
    fn ty(&self) -> &TypeAST {
        
    }
}

impl ExprAST for VarUseAST<'_> {
    fn codegen(&self, func: &mut Func) {
        func.push(LocalGet(0));
    }
    
    fn ty(&self) -> &TypeAST {
        
    }
}

impl ExprAST for BinOpAST<'_> {
    fn codegen(&self, func: &mut Func) {
        self.lhs.codegen(func);
        self.rhs.codegen(func);
        match self.op {
            BinOp::Add => func.push(I32Add),
            BinOp::Sub => func.push(I32Sub),
            BinOp::Mul => func.push(I32Mul),
            BinOp::Div => func.push(I32Div),
            BinOp::Eq => func.push(I32Eq),
            _ => todo!()
        }
    }
    
    fn ty(&self) -> &TypeAST {
        self.ty
    }

}

pub struct VarAST<'c> {
    pub ty: &'c TypeAST<'c>,
    pub value: Option<&'c dyn ExprAST>
}
pub struct FuncAST<'c> {
    pub proto: ProtoAST<'c>,
    pub body: &'c dyn ExprAST,
}

pub struct ProtoAST<'c> {
    pub return_type: &'c TypeAST<'c>,
    pub params: Vec<&'c VarAST<'c>>,
}
