use serde::{Deserialize, Serialize};

use crate::token::{Token, TokenType};

pub mod ast;
pub mod errors;
// pub mod executor;
pub mod parser;
pub mod token;

fn main() {
    let tokens = [
        Token {
            token_type: TokenType::Keyword,
            start: 0,
            end: 5,
            value: "const".to_owned(),
        },
        Token {
            token_type: TokenType::Whitespace,
            start: 5,
            end: 6,
            value: " ".to_owned(),
        },
        Token {
            token_type: TokenType::Keyword,
            start: 6,
            end: 9,
            value: "let".to_owned(),
        },
        Token {
            token_type: TokenType::Operator,
            start: 10,
            end: 11,
            value: "=".to_owned(),
        },
        Token {
            token_type: TokenType::String,
            start: 12,
            end: 19,
            value: "\"thing\"".to_owned(),
        },
    ];
    let result = parser::parser_impl::run_parser(&mut tokens.as_slice());
    eprintln!("{result:?}")
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub struct SourceRange([usize; 2]);
