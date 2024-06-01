use core::panic;

use crate::{lexer::{Lexer, TokenKind}, parse_tree::*};


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

fn parse_primary(lexer: &mut Lexer) -> Box<dyn ExprPT> {
    if lexer.current.kind == TokenKind::Int {
        let number = IntLiteralPT {
            value: lexer.current.content.replace("_", "").parse().unwrap(),
            node: NodePT::from_token(&lexer.current)
        };
        lexer.consume();
        Box::new(number)
    } else {
        panic!("Expected number at {}", lexer.current.start);
    }
}

fn parse_postfix(lexer: &mut Lexer) -> Box<dyn ExprPT> {
    parse_primary(lexer)
}

fn parse_unary(lexer: &mut Lexer) -> Box<dyn ExprPT> {
    parse_postfix(lexer)
}


fn parse_item(lexer: &mut Lexer, precedence: u32) -> ItemPT {
    
    let mut expr = parse_unary(lexer);
    let start = expr.node().start;
    loop {
        let op = BinOp::from_token(&lexer.current);
        lexer.consume();
        if let Some(op) = op {
            let op_precedence = op.precedence();
            if op_precedence < precedence {
                break;
            }

            let rhs = parse_expr(lexer, op_precedence + op.left_associative() as u32);
            let end = rhs.node().end;
            expr = Box::new(BinOpPT {
                op,
                lhs: expr,
                rhs,
                node: NodePT::new(start, end)
            });
        } else {
            break;
        }
    }
    ItemPT::Expr(expr)
}

fn parse_expr(lexer: &mut Lexer, precedence: u32) -> Box<dyn ExprPT> {
    match parse_item(lexer, precedence) {
        ItemPT::Expr(expr) => expr,
        ItemPT::Statement(statement) => panic!("Expected expression at {}", statement.node().start),
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
        if lexer.current.kind != TokenKind::RParen {
            panic!("Expected ',' or ')' at {}", lexer.current.start)
        }
        params.push(ParamPT { node: NodePT::new(param_type.node.start, identifier.node.end), ty: param_type, identifier });
    }

    let proto_end = lexer.current.end;
    lexer.consume();
    let body = parse_expr(lexer, 1);
    FuncPT {
        node: NodePT::new(return_type.node.start, body.node().end),
        proto: ProtoPT {
            node: NodePT::new(return_type.node.start, proto_end),
            identifier,
            return_type,
            params,
        },
        body,
    }
}