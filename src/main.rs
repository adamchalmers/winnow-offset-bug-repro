use serde::{Deserialize, Serialize};

pub mod ast;
pub mod errors;
// pub mod executor;
pub mod parser;
pub mod token;

fn main() {
    let some_program_string = r#"const let = "thing""#;
    let tokens = crate::token::lexer(some_program_string);
    let result = parser::parser_impl::run_parser(&mut tokens.as_slice());
    assert!(result.is_err());
    assert_eq!(
        result.err().unwrap().to_string(),
        r#"syntax: KclErrorDetails { source_ranges: [SourceRange([6, 9])], message: "Cannot assign a variable to a reserved keyword: let" }"#
    );
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub struct SourceRange([usize; 2]);
