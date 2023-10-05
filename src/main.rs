use winnow::{prelude::*, token::any};

fn main() {
    let tokens = [
        Token {
            token_type: TokenType::Keyword,
            value: "const".to_owned(),
        },
        Token {
            token_type: TokenType::Whitespace,
            value: " ".to_owned(),
        },
        // If this token was changed to TokenType::Word and value "asdf",
        // the parser would succeed.
        Token {
            token_type: TokenType::Keyword,
            value: "let".to_owned(),
        },
        Token {
            token_type: TokenType::Operator,
            value: "=".to_owned(),
        },
        Token {
            token_type: TokenType::String,
            value: "\"thing\"".to_owned(),
        },
    ];
    let result = declaration.parse(&mut tokens.as_slice());
    eprintln!("{result:#?}");
    let err = result.unwrap_err();
    let stream = err.input();
    let bad_token = &stream[err.offset()];
    eprintln!("Bad token: {bad_token:?}");
}

/* The stream is a vec of custom tokens. */
type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    Word,
    Operator,
    String,
    Keyword,
    Whitespace,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

/* Parsers */

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
    whitespace.parse_next(i)?;
    identifier.parse_next(i)?;
    equals(i)?;

    string_literal.parse_next(i)?;
    Ok(())
}

/// Parse an identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| token.token_type == TokenType::Word)
        .parse_next(i)
}
