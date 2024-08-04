use core::panic;

use bumpalo::Bump;

use crate::{lexer::{Lexer, TokenKind}, parse_tree::*};


pub fn parse<'a, 'b>(input: &'a str, arena: &'b Bump) -> FuncPT<'a, 'b> {
    let mut lexer = Lexer::new(input);
    lexer.consume();

    parse_func(&mut lexer, &arena)
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

fn parse_primary<'a: 'b, 'b>(lexer: &mut Lexer<'a>, arena: &'b Bump) -> &'b dyn ExprPT {
    match lexer.current.kind {
        TokenKind::Int => {
            let number = IntLiteralPT {
                value: lexer.current.content.replace("_", "").parse().unwrap(),
                node: NodePT::from_token(&lexer.current)
            };
            lexer.consume();
            arena.alloc(number)
        },
        TokenKind::Identifier => {
            let identifier = IdentifierPT { name: lexer.current.content, node: NodePT::from_token(&lexer.current) };
            lexer.consume();
            arena.alloc(identifier)
        },
        TokenKind::LParen => {
            let start = lexer.current.start;
            lexer.consume();
            let inner = parse_expr(lexer, 1, arena);
            if lexer.current.kind != TokenKind::RParen {
                panic!("Expected ')' at {}", lexer.current.start);
            }
            let expr = ParenPT {
                expr: inner,
                node: NodePT::new(start, lexer.current.end),
            };
            lexer.consume();
            arena.alloc(expr)
        },
        TokenKind::LCurly => {
            let start = lexer.current.start;
            lexer.consume();
            let mut statements = Vec::<&'b dyn StatementPT>::new();
            let mut last_expr: Option<&'b dyn ExprPT> = None;
            while lexer.current.kind != TokenKind::RCurly {
                match parse_item(lexer, 1, arena) {
                    ItemPT::Expr(expr) if lexer.current.kind == TokenKind::SemiColon => {
                        let end = lexer.current.end;
                        lexer.consume();
                        statements.push(arena.alloc(ExprStatementPT { node: NodePT { start: expr.node().start, end }, expr }));
                    },
                    ItemPT::Expr(expr) => {
                        last_expr = Some(expr);
                        if lexer.current.kind == TokenKind::RCurly {
                            break;
                        }
                        panic!("Expected '}}' at {}", lexer.current.start);
                    },
                    ItemPT::Statement(statement) => statements.push(statement),                    
                }
            }
            let end = lexer.current.end;
            lexer.consume();
            arena.alloc(BlockPT {
                statements,
                last_expr,
                node: NodePT::new(start, end),
            })
        },
        _ => panic!("Unexpected token at {}", lexer.current.start),
    }
}

fn parse_postfix<'a: 'b, 'b>(lexer: &mut Lexer<'a>, arena: &'b Bump) -> &'b dyn ExprPT {
    parse_primary(lexer, arena)
}

fn parse_unary<'a: 'b, 'b>(lexer: &mut Lexer<'a>, arena: &'b Bump) -> &'b dyn ExprPT{
    parse_postfix(lexer, arena)
}


fn parse_item<'a: 'b, 'b>(lexer: &mut Lexer<'a>, precedence: u32, arena: &'b Bump) -> ItemPT<'b> {
    if lexer.current.kind == TokenKind::Var {
        let start = lexer.current.start;
        lexer.consume();
        let identifier = parse_identifier_unwrap(lexer);

        if lexer.current.kind != TokenKind::Colon {
            panic!("Expected ':' at {}", lexer.current.start);
        }
        lexer.consume();

        let ty = parse_type_unwrap(lexer);
        if lexer.current.kind != TokenKind::Eq {
            panic!("Expected '=' at {}", lexer.current.start);
        }
        lexer.consume();
        let expr = parse_expr(lexer, 1, arena);
        if lexer.current.kind != TokenKind::SemiColon {
            panic!("Expected ';' at {}", lexer.current.start);
        }
        let end = lexer.current.end;
        lexer.consume();
        return ItemPT::Statement(arena.alloc(VarPT {
            ty,
            identifier,
            expr,
            node: NodePT::new(start, end),
        }))
    }

    let mut expr = parse_unary(lexer, arena);
    let start = expr.node().start;
    loop {
        let op = BinOp::from_token(&lexer.current);
        if let Some(op) = op {
            lexer.consume();
            let op_precedence = op.precedence();
            if op_precedence < precedence {
                break;
            }
            let rhs = parse_expr(lexer, op_precedence + op.left_associative() as u32, arena);
            let end = rhs.node().end;
            expr = arena.alloc(BinOpPT {
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

fn parse_expr<'a: 'b, 'b>(lexer: &mut Lexer<'a>, precedence: u32, arena: &'b Bump) -> &'b dyn ExprPT {
    match parse_item(lexer, precedence, arena) {
        ItemPT::Expr(expr) => expr,
        ItemPT::Statement(statement) => panic!("Expected expression at {}", statement.node().start),
    }
}

fn parse_func<'a, 'b>(lexer: &mut Lexer<'a>, arena: &'b Bump) -> FuncPT<'a, 'b> {
    let start = lexer.current.start;
    lexer.consume();
    let identifier = parse_identifier_unwrap(lexer);
    if lexer.current.kind != TokenKind::LParen {
        panic!("Expected '(' at {}", lexer.current.start);
    }
    lexer.consume();

    let mut params: Vec<ParamPT> = Vec::new();
    while !lexer.consume_if(TokenKind::RParen) {
        let identifier = parse_identifier_unwrap(lexer);
        if lexer.current.kind != TokenKind::Colon {
            panic!("Expected ':' at {}", lexer.current.start);
        }
        lexer.consume();
        let param_type = parse_type_unwrap(lexer);
        if lexer.current.kind != TokenKind::RParen {
            panic!("Expected ',' or ')' at {}", lexer.current.start)
        }
        params.push(ParamPT { node: NodePT::new(param_type.node.start, identifier.node.end), ty: param_type, identifier });
    }

    if lexer.current.kind != TokenKind::Arrow {
        panic!("Expected '->' at {}", lexer.current.start);
    }
    lexer.consume();

    let return_type = parse_type_unwrap(lexer);
    let body = parse_expr(lexer, 1, arena);
    FuncPT {
        node: NodePT::new(start, body.node().end),
        proto: ProtoPT {
            node: NodePT::new(start, return_type.node.end),
            identifier,
            return_type,
            params,
        },
        body,
    }
}
