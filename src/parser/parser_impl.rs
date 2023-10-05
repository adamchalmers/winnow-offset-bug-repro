use winnow::{combinator::repeat, prelude::*, token::any};

use crate::token::{Token, TokenType};

type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

/// Parse a KCL string literal
pub fn string_literal(i: TokenSlice) -> PResult<Token> {
    let token = any
        .verify(|token: &Token| token.token_type == TokenType::String)
        .parse_next(i)?;
    Ok(token)
}

/// Parse some whitespace (i.e. at least one whitespace token)
fn whitespace(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| token.token_type == TokenType::Whitespace)
        .parse_next(i)
}

/// Parse the = operator.
fn equals(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Operator) && token.value == "="
    })
    .parse_next(i)
}

/// Parse a variable/constant declaration.
pub fn declaration(i: TokenSlice) -> PResult<()> {
    let _kind = any
        .verify(|token: &Token| token.token_type == TokenType::Keyword)
        .parse_next(i)?;
    require_whitespace(i)?;
    identifier.parse_next(i)?;
    equals(i)?;

    string_literal.parse_next(i)?;
    Ok(())
}

/// Parse a KCL identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| token.token_type == TokenType::Word)
        .parse_next(i)
}

/// Matches at least 1 whitespace.
fn require_whitespace(i: TokenSlice) -> PResult<()> {
    repeat(1.., whitespace).parse_next(i)
}
