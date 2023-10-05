use winnow::{prelude::*, token::any};

fn main() {
    let tokens = [
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
    Keyword,
    Whitespace,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

/* Parsers */

/// Parse some whitespace (i.e. at least one whitespace token)
fn whitespace(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| token.token_type == TokenType::Whitespace)
        .parse_next(i)
}

/// Parse a variable/constant declaration.
pub fn declaration(i: TokenSlice) -> PResult<()> {
    whitespace.parse_next(i)?;
    identifier.parse_next(i)?;
    Ok(())
}

/// Parse an identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| token.token_type == TokenType::Word)
        .parse_next(i)
}
