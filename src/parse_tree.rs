use crate::lexer::{TextPos, Token, TokenKind};

//
// struture of the tree
//

pub trait ExprPT: std::fmt::Debug {
    fn to_ast(&self);
    fn node(&self) -> &NodePT;
}

pub trait StatementPT {
    fn to_ast(&self);
    fn node(&self) -> &NodePT;
}

#[derive(Clone, Debug)]
pub struct NodePT {
    pub start: TextPos,
    pub end: TextPos,
}

impl NodePT {
    pub fn new(start: TextPos, end: TextPos) -> Self {
        Self { start, end }
    }

    pub fn from_token(token: &Token) -> Self {
        Self::new(token.start, token.end)
    }
}

#[derive(Debug)]
pub struct TypePT<'a> {
    pub identifier: IdentifierPT<'a>,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct ParamPT<'a> {
    pub ty: TypePT<'a>,
    pub identifier: IdentifierPT<'a>,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct ProtoPT<'a> {
    pub identifier: IdentifierPT<'a>,
    pub return_type: TypePT<'a>,
    pub params: Vec<ParamPT<'a>>,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct FuncPT<'a> {
    pub proto: ProtoPT<'a>,
    pub body: Box<dyn ExprPT>,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct IdentifierPT<'a> {
    pub name: &'a str,
    pub node: NodePT,
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    And,
    Or,
    Eq,
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    pub fn left_associative(self) -> bool {
        true // at the moment all operators are left associative
    }

    pub fn precedence(self) -> u32 {
        match self {
            Self::And => 0,
            Self::Or => 1,
            Self::Eq => 2,
            Self::Add => 3,
            Self::Sub => 3,
            Self::Mul => 4,
            Self::Div => 4,
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match token.kind {
            TokenKind::And => Some(Self::And),
            TokenKind::Or => Some(Self::Or),
            TokenKind::Eq => Some(Self::Eq),
            TokenKind::Add => Some(Self::Add),
            TokenKind::Sub => Some(Self::Sub),
            TokenKind::Mul => Some(Self::Mul),
            TokenKind::Div => Some(Self::Div),
            _ => None,
        }
    }
}

pub enum ItemPT {
    Expr(Box<dyn ExprPT>),
    Statement(Box<dyn StatementPT>),
}

//
// implementation of the tree
//

#[derive(Debug)]
pub struct IntLiteralPT {
    pub value: i32,
    pub node: NodePT,
}

impl ExprPT for IntLiteralPT {
    fn to_ast(&self) {
        println!("IntLiteral: {}", self.value);
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

#[derive(Debug)]
pub struct BinOpPT {
    pub lhs: Box<dyn ExprPT>,
    pub op: BinOp,
    pub rhs: Box<dyn ExprPT>,
    pub node: NodePT,
}

impl ExprPT for BinOpPT {
    fn to_ast(&self) {
        println!("BinOp: {:?}", self.op);
        self.lhs.to_ast();
        self.rhs.to_ast();
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}