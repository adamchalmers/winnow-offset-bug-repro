use winnow::{
    combinator::{alt, repeat},
    error::StrContext::Label,
    prelude::*,
    token::any,
};

use crate::{
    errors::{KclError, KclErrorDetails},
    token::{Token, TokenType},
};

type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

/// Parse a KCL string literal
pub fn string_literal(i: TokenSlice) -> PResult<Token> {
    let token = any
        .try_map(|token: Token| match token.token_type {
            TokenType::String => Ok(token),
            _ => Err(KclError::Syntax(KclErrorDetails {
                message: "invalid string literal".to_owned(),
            })),
        })
        .context(Label("string literal (like \"myPart\""))
        .parse_next(i)?;
    Ok(token)
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
                    message: format!(
                        "expected whitespace, found '{}' which is {:?}",
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
fn value(i: TokenSlice) -> PResult<Token> {
    alt((string_literal, identifier))
        .context(Label("a KCL value (but not a pipe expression)"))
        .parse_next(i)
}

/// Parse a variable/constant declaration.
pub fn declaration(i: TokenSlice) -> PResult<()> {
    const EXPECTED: &str = "expected a variable declaration keyword (e.g. 'let') but found";
    let _kind = any
        .try_map(|token: Token| {
            let Some(kind) = token.declaration_keyword() else {
                return Err(KclError::Syntax(KclErrorDetails {
                    message: format!("{EXPECTED} {}", token.value.as_str()),
                }));
            };

            Ok(kind)
        })
        .context(Label("declaring a name, e.g. 'let width = 3'"))
        .parse_next(i)?;
    require_whitespace(i)?;
    identifier
        .context(Label(
            "an identifier, which becomes name you're binding the value to",
        ))
        .parse_next(i)?;
    equals(i)?;

    value
        .context(Label("a KCL value, which is being bound to a variable"))
        .parse_next(i)?;
    Ok(())
}

/// Parse a KCL identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Token> {
    any.try_map(|token: Token| {
        if token.token_type == TokenType::Word {
            Ok(token)
        } else {
            Err(KclError::Syntax(KclErrorDetails {
                message: format!(
                    "{} is not an identifier, it is a {:?}",
                    token.value.as_str(),
                    token.token_type
                ),
            }))
        }
    })
    .context(Label("an identifier, e.g. 'width' or 'myPart'"))
    .parse_next(i)
}

/// Matches at least 1 whitespace.
fn require_whitespace(i: TokenSlice) -> PResult<()> {
    repeat(1.., whitespace).parse_next(i)
}

/// Parse a KCL expression.
fn expression(i: TokenSlice) -> PResult<()> {
    value
        .context(Label(
            "an expression (i.e. a value, or an algorithm for calculating one), e.g. 'x + y' or '3' or 'width * 2'",
        ))
        .parse_next(i)?;
    Ok(())
}
