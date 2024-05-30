use crate::lexer::{Lexer, TextPos, Token, TokenKind};

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
pub struct FuncPT<'a> {
    pub identifier: IdentifierPT<'a>,
    pub return_type: TypePT<'a>,
    pub params: Vec<ParamPT<'a>>,
    pub node: NodePT,
}

#[derive(Debug)]
pub struct IdentifierPT<'a> {
    pub name: &'a str,
    pub node: NodePT,
}


pub fn parse(input: &str) -> FuncPT {
    let mut lexer = Lexer::new(input);
    lexer.consume();
    parse_func(&mut lexer)
}

fn parse_type<'a>(lexer: &mut Lexer<'a>) -> Option<TypePT<'a>> {
    let identifier = parse_identifier(lexer)?;
    Some(TypePT { node: identifier.node.clone(), identifier })
}

fn parse_type_unwrap<'a>(lexer: &mut Lexer<'a>) -> TypePT<'a> {
    if let Some(ty) = parse_type(lexer) {
        ty
    } else {
        panic!("Expected type at {}", lexer.current.start)
    }
}

fn parse_identifier<'a>(lexer: &mut Lexer<'a>) -> Option<IdentifierPT<'a>> {
    if lexer.current.kind == TokenKind::Identifier {
        let identifier = IdentifierPT { name: lexer.current.content, node: NodePT::from_token(&lexer.current) };
        lexer.consume();
        Some(identifier)
    } else {
        None
    }
}

fn parse_identifier_unwrap<'a>(lexer: &mut Lexer<'a>) -> IdentifierPT<'a> {
    if let Some(identifier) = parse_identifier(lexer) {
        identifier
    } else {
        panic!("Expected identifier at {}", lexer.current.start);
    }
}

fn parse_func<'a>(lexer: &mut Lexer<'a>) -> FuncPT<'a> {
    let return_type = parse_type_unwrap(lexer);
    let identifier = parse_identifier_unwrap(lexer);
    if lexer.current.kind != TokenKind::LParen {
        panic!("Expected '(' at {}", lexer.current.start);
    }
    lexer.consume();

    let mut params: Vec<ParamPT> = Vec::new();
    while lexer.current.kind != TokenKind::RParen {
        let param_type = parse_type_unwrap(lexer);
        let identifier = parse_identifier_unwrap(lexer);
        if lexer.consume_if(TokenKind::Comma) {
            params.push(ParamPT { node: NodePT::new(param_type.node.start, identifier.node.end), ty: param_type, identifier });
        } else if lexer.current.kind != TokenKind::RParen {
            panic!("Expected ',' or ')' at {}", lexer.current.start)
        }
    }

    let func = FuncPT { node: NodePT::new(return_type.node.start, lexer.current.end), return_type, identifier, params };
    lexer.consume();
    func
}