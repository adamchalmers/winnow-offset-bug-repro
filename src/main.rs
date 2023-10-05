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
            token_type: TokenType::Whitespace,
            start: 9,
            end: 10,
            value: " ".to_owned(),
        },
        Token {
            token_type: TokenType::Operator,
            start: 10,
            end: 11,
            value: "=".to_owned(),
        },
        Token {
            token_type: TokenType::Whitespace,
            start: 11,
            end: 12,
            value: " ".to_owned(),
        },
        Token {
            token_type: TokenType::String,
            start: 12,
            end: 19,
            value: "\"thing\"".to_owned(),
        },
    ];
    let result = parser::parser_impl::run_parser(&mut tokens.as_slice());
    assert!(result.is_err());
    assert_eq!(
        result.err().unwrap().to_string(),
        r#"syntax: KclErrorDetails { source_ranges: [SourceRange([6, 9])], message: "Cannot assign a variable to a reserved keyword: let" }"#
    );
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub struct SourceRange([usize; 2]);
