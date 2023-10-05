use winnow::Parser;

use crate::token::{Token, TokenType};

pub mod ast;
// pub mod executor;
pub mod parser;
pub mod token;

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
    let result = parser::parser_impl::declaration.parse(&mut tokens.as_slice());
    eprintln!("{result:#?}");
    let err = result.unwrap_err();
    let stream = err.input();
    let bad_token = &stream[err.offset()];
    eprintln!("Bad token: {bad_token:?}");
}
