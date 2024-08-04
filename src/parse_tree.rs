use std::{collections::HashMap, fmt::Display, hash::Hash};

use bumpalo::Bump;

use crate::{ast::*, lexer::{TextPos, Token, TokenKind}, logger};

// let UNKNOWN_TYPE = TypeDeclPT

// 'a = lifetime of the input string
// 'b = lifetime of the parse tree's arena
// 'c = lifetime of the AST's arena

struct TypedExpr<'a, 'b, 'c> {
    expr: &'c dyn ExprAST,
    ty: &'b TypePT<'a>,
}

struct TypedVar<'a, 'b, 'c> {
    var: &'c VarAST<'c>,
    ty: &'b TypePT<'a>,
}

struct Converter<'a: 'b + 'c, 'b, 'c> {
    pub module: &'b ModulePT<'a, 'b>,
    pub arena: &'c mut Bump,
    pub types: HashMap<TypePT<'a>, &'c TypeAST<'c>>,
    pub typed_vars: HashMap<&'a str, TypedVar<'a, 'b, 'c>>,
}

pub trait ExprPT: std::fmt::Debug {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> Option<TypedExpr>;
    fn node(&self) -> &NodePT;
}

pub trait StatementPT: std::fmt::Debug {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>);
    fn node(&self) -> &NodePT;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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


#[derive(Debug, PartialEq, Eq, Hash)]
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
pub struct FuncPT<'a: 'b, 'b> {
    pub proto: ProtoPT<'a>,
    pub body: &'b dyn ExprPT,
    pub node: NodePT,
}

#[derive(Debug)]
pub enum VisibilityPT {
    Public,
    Private,
}

#[derive(Debug)]
pub struct VisibilityNodePT {
    pub visibility: VisibilityPT,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct TypeDeclPT<'a> {
    pub visibility: VisibilityNodePT,
    pub identifier: IdentifierPT<'a>,
    pub ty: TypePT<'a>,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct ModulePT<'a: 'b, 'b> {
    pub identifier: IdentifierPT<'a>,
    pub node: NodePT,
    children: HashMap<&'a str, &'b ModulePT<'a, 'b>>,
    funcs: HashMap<&'a str, &'b FuncPT<'a, 'b>>,
    types: HashMap<&'a str, &'b TypeDeclPT<'a>>,

    imported_modules: HashMap<&'a str, &'b ModulePT<'a, 'b>>,
    imported_funcs: HashMap<&'a str, &'b FuncPT<'a, 'b>>,
    imported_types: HashMap<&'a str, &'b TypeDeclPT<'a>>,
}

impl<'a, 'b> ModulePT<'a, 'b> {
    pub fn new(node: NodePT, identifier: IdentifierPT<'a>) -> Self {
        Self {
            identifier,
            node,
            children: HashMap::new(),
            funcs: HashMap::new(),
            types: HashMap::new(),
            imported_modules: HashMap::new(),
            imported_funcs: HashMap::new(),
            imported_types: HashMap::new(),
        }
    }

    pub fn get_type(&self, ty: &TypePT<'a>) -> Option<&'b TypeDeclPT<'a>> {
        self.types.get(ty.identifier.name)
            .or_else(|| self.imported_types.get(ty.identifier.name))
            .or_else(|| {
                logger::use_of_undeclared_type(ty);
                None
            }).copied()
    }

    pub fn get_pub_type(&self, ty: &TypePT<'a>) -> Option<&'b TypeDeclPT<'a>> {
        self.types.get(ty.identifier.name)
            .or_else(|| self.imported_types.get(ty.identifier.name))
            .or_else(|| {
                logger::use_of_undeclared_type(ty);
                None
            }).copied()
    }
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::Eq => write!(f, "=="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug)]
pub struct BinOpNodePT {
    pub op: BinOp,
    pub node: NodePT,
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

pub enum ItemPT<'b> {
    Expr(&'b dyn ExprPT),
    Statement(&'b dyn StatementPT),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IdentifierPT<'a> {
    pub name: &'a str,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct IntLiteralPT {
    pub value: i32,
    pub node: NodePT,
}


#[derive(Debug)]
pub struct BinOpPT<'b> {
    pub lhs: &'b dyn ExprPT,
    pub op_node: BinOpNodePT,
    pub rhs: &'b dyn ExprPT,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct ParenPT<'b> {
    pub expr: &'b dyn ExprPT,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct BlockPT<'b> {
    pub statements: Vec<&'b dyn StatementPT>,
    pub last_expr: Option<&'b dyn ExprPT>,
    pub node: NodePT,
}


#[derive(Debug)]
pub struct IfPT<'b> {
    pub condition: &'b dyn ExprPT,
    pub then_expr: &'b dyn ExprPT,
    pub else_expr: Option<&'b dyn ExprPT>,
    pub node: NodePT,
}

impl ExprPT for IdentifierPT<'_> {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> Option<TypedExpr> {
        converter.typed_vars.get(self.name).map(|typed_var| TypedExpr {
            expr: converter.arena.alloc(VarUseAST {
                var: typed_var.var,
            }),
            ty: typed_var.ty,
        })
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

impl ExprPT for IntLiteralPT {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> Option<TypedExpr> {
        Some(TypedExpr {
            expr: converter.arena.alloc(IntAST {
                value: self.value,
            }),
        })
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

impl ExprPT for BinOpPT<'_> {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> Option<TypedExpr> {
        println!("BinOp: {:?}", self.op_node);
        let lhs = self.lhs.to_ast(converter);
        let rhs = self.rhs.to_ast(converter);
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) if lhs.expr.ty() as *const _ == rhs.expr.ty() as *const _ => {
                Some(TypedExpr {
                    expr: converter.arena.alloc(BinOpAST {
                        lhs: lhs.expr,
                        op: self.op_node.op,
                        rhs: rhs.expr,
                        ty: lhs.expr.ty()
                    }),
                    ty: lhs.ty,
                })
            },
            (Some(lhs), Some(rhs)) => {
                logger::bin_op_mismatched_types(&self.op_node, lhs.ty, rhs.ty);
                None
            },
            _ => None
        }
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

impl ExprPT for ParenPT<'_> {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> Option<TypedExpr> {
        println!("Parentheses:");
        self.expr.to_ast(converter);
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}
impl ExprPT for BlockPT<'_> {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> Option<TypedExpr> {
        println!("Block:");
        for statement in &self.statements {
            statement.to_ast(converter);
        }
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
} 

impl<'a> FuncPT<'a, '_> {
    pub fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> FuncAST<'a, 'c> {
        FuncAST {
            proto: self.proto.to_ast(converter),
            body: self.body.to_ast(converter),
        }
    }
}

impl<'a> ProtoPT<'a> {
    pub fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) -> ProtoAST<'a, 'c> {
        ProtoAST {
            name: self.identifier.name,
            return_type: arena.alloc(TypeAST {
                name: self.return_type.identifier.name,
                fields: vec![],
            }),
            params: self.params.iter().map(|param| &*arena.alloc(VarAST {
                ty: arena.alloc(TypeAST {
                    name: param.ty.identifier.name,
                    fields: vec![],
                }),
                name: param.identifier.name,
                value: None,
            })).collect(),
        }
    }
}




//
// statements
// 

#[derive(Debug)]
pub struct ExprStatementPT<'b> {
    pub expr: &'b dyn ExprPT,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct VarPT<'a: 'b, 'b> {
    pub ty: TypePT<'a>,
    pub identifier: IdentifierPT<'a>,
    pub expr: &'b dyn ExprPT,
    pub node: NodePT,
}

impl<'a> StatementPT for ExprStatementPT<'a> {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) {
        self.expr.to_ast(converter);
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}


impl<'a> StatementPT for VarPT<'a, '_> {
    fn to_ast<'c>(&self, converter: Converter<'_, '_, 'c>) {
        println!("Variable declaration:");
       // self.ty.to_ast();
        self.identifier.to_ast(converter);
        self.expr.to_ast(converter);
    }
    
    fn node(&self) -> &NodePT {
        &self.node
    }
}

//
// register
//