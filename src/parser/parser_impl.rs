use serde_json::Value as JValue;
use winnow::{
    combinator::{alt, opt, peek, repeat, separated1},
    dispatch,
    error::StrContext::Label,
    prelude::*,
    token::any,
};

use crate::{
    ast::types::{
        BodyItem, ExpressionStatement, Identifier, Literal, Program, Value, VariableDeclaration,
        VariableDeclarator,
    },
    errors::{KclError, KclErrorDetails},
    token::{Token, TokenType},
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
        });
    Ok(Program {
        start: 0,
        end,
        body,
        non_code_meta: Default::default(), // TODO: support comments.
    })
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

/// Parse a KCL value
fn value(i: TokenSlice) -> PResult<Value> {
    alt((
        string_literal.map(Box::new).map(Value::Literal),
        identifier.map(Box::new).map(Value::Identifier),
    ))
    .context(Label("a KCL value (but not a pipe expression)"))
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
