use serde_json::{Number as JNumber, Value as JValue};
use winnow::{
    combinator::{alt, opt, peek, repeat, separated0, separated1},
    dispatch,
    error::StrContext::Label,
    prelude::*,
    token::any,
};

use crate::{
    ast::types::{
        BinaryPart, BodyItem, ExpressionStatement, Identifier, Literal, Program, ReturnStatement,
        UnaryExpression, UnaryOperator, Value, VariableDeclaration, VariableDeclarator,
    },
    errors::{KclError, KclErrorDetails},
    token::{Token, TokenType},
    SourceRange,
};

mod error;

type PResult<O, E = error::ContextError> = winnow::prelude::PResult<O, E>;

type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

pub fn run_parser(i: TokenSlice) -> Result<Program, KclError> {
    if i.is_empty() {
        return Err(KclError::Syntax(KclErrorDetails {
            source_ranges: vec![],
            message: "file is empty".to_string(),
        }));
    }

    program.parse(i).map_err(KclError::from)
}

fn body_item(i: TokenSlice) -> PResult<BodyItem> {
    dispatch! {peek(any);
        token @ Token { .. } if token.declaration_keyword().is_some() => declaration.map(BodyItem::VariableDeclaration),
        _ => expression.map(BodyItem::ExpressionStatement),
    }
    .context(Label("a KCL program body item, i.e. a declaration or expression"))
    .parse_next(i)
}

fn program(i: TokenSlice) -> PResult<Program> {
    ignore_whitespace(i);
    let body: Vec<_> = separated1(body_item, whitespace)
        .context(Label(
            "at least one KCL body item, i.e. a declaration or expression",
        ))
        .parse_next(i)?;
    let trailing_ws = opt(whitespace).parse_next(i)?.unwrap_or_default();
    let end = trailing_ws
        .last()
        .map(|tok| tok.end)
        .unwrap_or_else(|| match body.last().unwrap() {
            BodyItem::VariableDeclaration(v) => v.end,
            BodyItem::ExpressionStatement(e) => e.end,
            BodyItem::ReturnStatement(r) => r.end,
        });
    Ok(Program {
        start: 0,
        end,
        body,
        non_code_meta: Default::default(), // TODO: support comments.
    })
}

fn bool_value(i: TokenSlice) -> PResult<Identifier> {
    let (name, token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::Keyword if token.value == "true" => Ok(("true", token)),
            TokenType::Keyword if token.value == "false" => Ok(("false", token)),
            _ => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: "invalid boolean literal".to_owned(),
            })),
        })
        .context(Label("a boolean literal (either true or false)"))
        .parse_next(i)?;
    Ok(Identifier {
        start: token.start,
        end: token.end,
        name: name.to_owned(),
    })
}

pub fn literal(i: TokenSlice) -> PResult<Literal> {
    alt((string_literal, unsigned_number_literal))
        .context(Label("a KCL literal, like 'myPart' or 3"))
        .parse_next(i)
}

/// Parse a KCL string literal
pub fn string_literal(i: TokenSlice) -> PResult<Literal> {
    let (value, token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::String => {
                let s = token.value[1..token.value.len() - 1].to_string();
                Ok((JValue::String(s), token))
            }
            _ => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: "invalid string literal".to_owned(),
            })),
        })
        .context(Label("string literal (like \"myPart\""))
        .parse_next(i)?;
    Ok(Literal {
        start: token.start,
        end: token.end,
        value,
        raw: token.value.clone(),
    })
}

/// Parse a KCL literal number, with no - sign.
fn unsigned_number_literal(i: TokenSlice) -> PResult<Literal> {
    let (value, token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::Number => {
                if let Ok(x) = token.value.parse::<i64>() {
                    return Ok((JValue::Number(JNumber::from(x)), token));
                }
                let x: f64 = token.value.parse().map_err(|_| {
                    KclError::Syntax(KclErrorDetails {
                        source_ranges: token.as_source_ranges(),
                        message: format!("Invalid float: {}", token.value),
                    })
                })?;

                match JNumber::from_f64(x) {
                    Some(n) => Ok((JValue::Number(n), token)),
                    None => Err(KclError::Syntax(KclErrorDetails {
                        source_ranges: token.as_source_ranges(),
                        message: format!("Invalid float: {}", token.value),
                    })),
                }
            }
            _ => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: "invalid literal".to_owned(),
            })),
        })
        .context(Label("an unsigned number literal (e.g. 3 or 12.5)"))
        .parse_next(i)?;
    Ok(Literal {
        start: token.start,
        end: token.end,
        value,
        raw: token.value.clone(),
    })
}

/// Parse a KCL operand that can be used with an operator.
fn operand(i: TokenSlice) -> PResult<BinaryPart> {
    const TODO_783: &str = "found a value, but this kind of value cannot be used as the operand to an operator yet (see https://github.com/KittyCAD/modeling-app/issues/783)";
    let op = possible_operands
        .try_map(|part| {
            let val = match part {
                Value::UnaryExpression(x) => BinaryPart::UnaryExpression(x),
                Value::Literal(x) => BinaryPart::Literal(x),
                Value::Identifier(x) => BinaryPart::Identifier(x),
            };
            Ok(val)
        })
        .context(Label(
            "an operand (a value which can be used with an operator)",
        ))
        .parse_next(i)?;
    Ok(op)
}

impl TokenType {
    fn parse_from(self, i: TokenSlice) -> PResult<Token> {
        any.try_map(|token: Token| {
            if token.token_type == self {
                Ok(token)
            } else {
                Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!(
                        "expected {self} but found {} which is a {}",
                        token.value.as_str(),
                        token.token_type
                    ),
                }))
            }
        })
        .parse_next(i)
    }
}

/// Parse some whitespace (i.e. at least one whitespace token)
fn whitespace(i: TokenSlice) -> PResult<Vec<Token>> {
    repeat(
        1..,
        any.try_map(|token: Token| {
            if token.token_type == TokenType::Whitespace {
                Ok(token)
            } else {
                Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!(
                        "expected whitespace, found '{}' which is {}",
                        token.value.as_str(),
                        token.token_type
                    ),
                }))
            }
        }),
    )
    .context(Label("some whitespace (e.g. spaces, tabs, new lines)"))
    .parse_next(i)
}

/// Parse the = operator.
fn equals(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Operator) && token.value == "="
    })
    .context(Label("the equals operator, ="))
    .parse_next(i)
}

/// Parse the body of a user-defined function.
pub fn function_body(i: TokenSlice) -> PResult<Program> {
    let leading_whitespace = opt(whitespace).parse_next(i)?;
    let body: Vec<_> =
        separated1(
            alt((
                return_stmt.map(BodyItem::ReturnStatement),
                declaration.map(BodyItem::VariableDeclaration),
                expression.map(BodyItem::ExpressionStatement),
            )),
            ws_with_newline,
        )
        .context(Label("a sequence of function body items (functions are made up of variable declarations, expressions, and return statements, each of those is a possible body item"))
        .parse_next(i)?;
    let start_whitespace = leading_whitespace.and_then(|w| w.first().map(|tok| tok.start));
    // Safe to unwrap `body.first()` because `body` is `separated1` therefore guaranteed
    // to have len >= 1.
    let start = start_whitespace.unwrap_or_else(|| body.first().unwrap().start()) - 1;
    let end_ws = opt(whitespace)
        .parse_next(i)?
        .and_then(|ws| ws.first().map(|tok| tok.end));
    // Safe to unwrap `body.last()` for the same reason as `body.first()`.
    let end = end_ws.unwrap_or_else(|| match body.last().unwrap() {
        BodyItem::VariableDeclaration(v) => v.end,
        BodyItem::ExpressionStatement(e) => e.end,
        BodyItem::ReturnStatement(r) => r.end,
    }) + 1;
    Ok(Program {
        start,
        end,
        body,
        non_code_meta: Default::default(), // TODO: support comments
    })
}

/// Parse a return statement of a user-defined function, e.g. `return x`.
pub fn return_stmt(i: TokenSlice) -> PResult<ReturnStatement> {
    let start = any
        .try_map(|token: Token| {
            if matches!(token.token_type, TokenType::Keyword) && token.value == "return" {
                Ok(token.start)
            } else {
                Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!("{} is not a return keyword", token.value.as_str()),
                }))
            }
        })
        .context(Label(
            "the 'return' keyword, which ends your function (and becomes this function's value when it's called)",
        ))
        .parse_next(i)?;
    require_whitespace(i)?;
    let argument = value(i)?;
    Ok(ReturnStatement {
        start,
        end: argument.end(),
        argument,
    })
}

/// Parse a KCL value
fn value(i: TokenSlice) -> PResult<Value> {
    alt((value_but_not_pipe,))
        .context(Label("a KCL value"))
        .parse_next(i)
}

fn value_but_not_pipe(i: TokenSlice) -> PResult<Value> {
    alt((
        unary_expression.map(Box::new).map(Value::UnaryExpression),
        bool_value.map(Box::new).map(Value::Identifier),
        literal.map(Box::new).map(Value::Literal),
        identifier.map(Box::new).map(Value::Identifier),
    ))
    .context(Label("a KCL value (but not a pipe expression)"))
    .parse_next(i)
}

fn possible_operands(i: TokenSlice) -> PResult<Value> {
    alt((
        unary_expression.map(Box::new).map(Value::UnaryExpression),
        bool_value.map(Box::new).map(Value::Identifier),
        literal.map(Box::new).map(Value::Literal),
        identifier.map(Box::new).map(Value::Identifier),
    ))
    .context(Label(
        "a KCL value which can be used as an argument/operand to an operator",
    ))
    .parse_next(i)
}

/// Parse a variable/constant declaration.
fn declaration(i: TokenSlice) -> PResult<VariableDeclaration> {
    const EXPECTED: &str = "expected a variable declaration keyword (e.g. 'let') but found";
    let (kind, start) = any
        .try_map(|token: Token| {
            let Some(kind) = token.declaration_keyword() else {
                return Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!("{EXPECTED} {}", token.value.as_str()),
                }));
            };

            Ok((kind, token.start))
        })
        .context(Label("declaring a name, e.g. 'let width = 3'"))
        .parse_next(i)?;
    require_whitespace(i)?;
    let id = identifier
        .context(Label(
            "an identifier, which becomes name you're binding the value to",
        ))
        .parse_next(i)?;
    ignore_whitespace(i);
    equals(i)?;
    ignore_whitespace(i);

    let val = value
        .context(Label("a KCL value, which is being bound to a variable"))
        .parse_next(i)?;
    let end = val.end();
    Ok(VariableDeclaration {
        start,
        end,
        declarations: vec![VariableDeclarator {
            start: id.start,
            end,
            id,
            init: val,
        }],
        kind,
    })
}

/// Parse a KCL identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Identifier> {
    any.try_map(|token: Token| {
        if token.token_type == TokenType::Word {
            Ok(Identifier {
                start: token.start,
                end: token.end,
                name: token.value,
            })
        } else {
            Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "{} is not an identifier, it is a {}",
                    token.value.as_str(),
                    token.token_type
                ),
            }))
        }
    })
    .context(Label("an identifier, e.g. 'width' or 'myPart'"))
    .parse_next(i)
}

/// Helper function. Matches any number of whitespace tokens and ignores them.
fn ignore_whitespace(i: TokenSlice) {
    let _: PResult<()> = repeat(0.., whitespace).parse_next(i);
}

/// Matches at least 1 whitespace.
fn require_whitespace(i: TokenSlice) -> PResult<()> {
    repeat(1.., whitespace).parse_next(i)
}

fn unary_expression(i: TokenSlice) -> PResult<UnaryExpression> {
    const EXPECTED: &str = "expected a unary operator (like '-', the negative-numeric operator),";
    let (operator, op_token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::Operator if token.value == "-" => Ok((UnaryOperator::Neg, token)),
            // TODO: negation. Original parser doesn't support `not` yet.
            TokenType::Operator => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "{EXPECTED} but found {} which is an operator, but not a unary one (unary operators apply to just a single operand, your operator applies to two or more operands)",
                    token.value.as_str(),
                ),
            })),
            other => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "{EXPECTED} but found {} which is {}",
                    token.value.as_str(),
                    other,
                ),
            })),
        })
        .context(Label("a unary expression, e.g. -x or -3"))
        .parse_next(i)?;
    let argument = operand.parse_next(i)?;
    Ok(UnaryExpression {
        start: op_token.start,
        end: argument.end(),
        operator,
        argument,
    })
}

/// Parse a KCL expression.
fn expression(i: TokenSlice) -> PResult<ExpressionStatement> {
    let val = value
        .context(Label(
            "an expression (i.e. a value, or an algorithm for calculating one), e.g. 'x + y' or '3' or 'width * 2'",
        ))
        .parse_next(i)?;
    Ok(ExpressionStatement {
        start: val.start(),
        end: val.end(),
        expression: val,
    })
}

/// Parse the given brace symbol.
fn some_brace(symbol: &'static str, i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Brace) && token.value == symbol
    })
    .context(Label(symbol))
    .parse_next(i)
}

/// Parse a => operator.
fn big_arrow(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Operator) && token.value == "=>"
    })
    .context(Label("the => symbol, used for declaring functions"))
    .parse_next(i)
}

fn ws_with_newline(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Whitespace) && token.value.contains('\n')
    })
    .context(Label("a newline, possibly with whitespace"))
    .parse_next(i)
}

fn comma(i: TokenSlice) -> PResult<()> {
    TokenType::Comma.parse_from(i)?;
    Ok(())
}

/// Parse a comma, optionally followed by some whitespace.
fn comma_sep(i: TokenSlice) -> PResult<()> {
    (comma, opt(whitespace))
        .context(Label("a comma, optionally followed by whitespace"))
        .parse_next(i)?;
    Ok(())
}

/// Arguments are passed into a function.
fn arguments(i: TokenSlice) -> PResult<Vec<Value>> {
    separated0(value, comma_sep)
        .context(Label("function arguments"))
        .parse_next(i)
}
