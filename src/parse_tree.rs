use crate::lexer::{TextPos, Token, TokenKind};

pub trait ExprPT: std::fmt::Debug {
    fn to_ast(&self);
    fn node(&self) -> &NodePT;
}

pub trait StatementPT: std::fmt::Debug {
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
    pub body: Box<dyn ExprPT + 'a>,
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

pub enum ItemPT<'a> {
    Expr(Box<dyn ExprPT + 'a>),
    Statement(Box<dyn StatementPT + 'a>),
}



#[derive(Debug)]
pub struct IdentifierPT<'a> {
    pub name: &'a str,
    pub node: NodePT,
}

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
pub struct BinOpPT<'a> {
    pub lhs: Box<dyn ExprPT + 'a>,
    pub op: BinOp,
    pub rhs: Box<dyn ExprPT + 'a>,
    pub node: NodePT,
}

impl ExprPT for BinOpPT<'_> {
    fn to_ast(&self) {
        println!("BinOp: {:?}", self.op);
        self.lhs.to_ast();
        self.rhs.to_ast();
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

impl<'a> ExprPT for IdentifierPT<'a> {
    fn to_ast(&self) {
        println!("Variable: {}", self.name);
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

#[derive(Debug)]
pub struct ParenPT<'a> {
    pub expr: Box<dyn ExprPT + 'a>,
    pub node: NodePT,
}

impl<'a> ExprPT for ParenPT<'a> {
    fn to_ast(&self) {
        println!("Parentheses:");
        self.expr.to_ast();
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

#[derive(Debug)]
pub struct BlockPT<'a> {
    pub statements: Vec<Box<dyn StatementPT + 'a>>,
    pub last_expr: Option<Box<dyn ExprPT + 'a>>,
    pub node: NodePT,
}

impl<'a> ExprPT for BlockPT<'a> {
    fn to_ast(&self) {
        println!("Block:");
        for statement in &self.statements {
            statement.to_ast();
        }
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
} 

#[derive(Debug)]
pub struct ExprStatementPT<'a> {
    pub expr: Box<dyn ExprPT + 'a>,
    pub node: NodePT,
}

impl<'a> StatementPT for ExprStatementPT<'a> {
    fn to_ast(&self) {
        self.expr.to_ast();
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

#[derive(Debug)]
pub struct VarPT<'a> {
    pub ty: TypePT<'a>,
    pub identifier: IdentifierPT<'a>,
    pub expr: Box<dyn ExprPT + 'a>,
    pub node: NodePT,
}

impl<'a> StatementPT for VarPT<'a> {
    fn to_ast(&self) {
        println!("Variable declaration:");
       // self.ty.to_ast();
        self.identifier.to_ast();
        self.expr.to_ast();
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

pub struct IfPT<'a> {
    pub condition: Box<dyn ExprPT + 'a>,
    pub then_expr: Box<dyn ExprPT + 'a>,
    pub else_expr: Option<Box<dyn ExprPT + 'a>>,
    pub node: NodePT,
}