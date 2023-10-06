use std::mem::size_of;

use winnow::{prelude::*, token::any};

fn main() {
    let tokens = [
        Token {
            token_type: TokenType::Keyword,
            start: 0,
            end: 2,
            value: "fn".to_owned(),
        },
        Token {
            token_type: TokenType::Whitespace,
            start: 2,
            end: 3,
            value: " ".to_owned(),
        },
        Token {
            token_type: TokenType::Word,
            start: 3,
            end: 13,
            value: "firstPrime".to_owned(),
        },
        Token {
            token_type: TokenType::Whitespace,
            start: 13,
            end: 14,
            value: " ".to_owned(),
        },
        Token {
            token_type: TokenType::Operator,
            start: 14,
            end: 15,
            value: "=".to_owned(),
        },
        Token {
            token_type: TokenType::Whitespace,
            start: 15,
            end: 16,
            value: " ".to_owned(),
        },
        Token {
            token_type: TokenType::Brace,
            start: 16,
            end: 17,
            value: "(".to_owned(),
        },
    ];

    println!("{}", size_of::<Token>());
    let result = declaration.parse(&mut tokens.as_slice());
    eprintln!("{result:#?}");
    let err = result.unwrap_err();
    let stream = err.input();

    // Expectation: I can find the bad token (the token which caused the error
    // by indexing err.offset() into err.input().
    // However, this actually gives an index-out-of-bounds panic, because
    // the offset is 32 and the input slice only has length 2.
    let bad_token = &stream[err.offset()];
    eprintln!("Bad token: {bad_token:?}");
}

/* The stream is a vec of custom tokens. */
type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    /// A number.
    Number,
    /// A word.
    Word,
    /// An operator.
    Operator,
    /// A string.
    String,
    /// A keyword.
    Keyword,
    /// A brace.
    Brace,
    /// Whitespace.
    Whitespace,
    /// A comma.
    Comma,
    /// A colon.
    Colon,
    /// A period.
    Period,
    /// A double period: `..`.
    DoublePeriod,
    /// A line comment.
    LineComment,
    /// A block comment.
    BlockComment,
    /// A function name.
    Function,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub end: usize,
    pub value: String,
}

/* Parsers */

/// Parse a variable/constant declaration.
pub fn declaration(i: TokenSlice) -> PResult<()> {
    any.parse_next(i)?;
    any.parse_next(i)?;
    any.parse_next(i)?;
    any.parse_next(i)?;
    any.parse_next(i)?;
    any.parse_next(i)?;
    any.parse_next(i)?;
    identifier.parse_next(i)?;
    Ok(())
}

/// Parse an identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| token.token_type == TokenType::Word)
        .parse_next(i)
}
