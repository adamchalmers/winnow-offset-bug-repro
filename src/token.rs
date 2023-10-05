use parse_display::{Display, FromStr};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{ast::types::VariableKind, SourceRange};

mod tokeniser;

/// The types of tokens.
#[derive(
    Debug, PartialEq, Eq, Copy, Clone, Deserialize, Serialize, JsonSchema, FromStr, Display,
)]
#[serde(rename_all = "camelCase")]
#[display(style = "camelCase")]
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

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
pub struct Token {
    #[serde(rename = "type")]
    pub token_type: TokenType,
    /// Offset in the source code where this token begins.
    pub start: usize,
    /// Offset in the source code where this token ends.
    pub end: usize,
    pub value: String,
}

impl Token {
    pub fn from_range(range: std::ops::Range<usize>, token_type: TokenType, value: String) -> Self {
        Self {
            start: range.start,
            end: range.end,
            value,
            token_type,
        }
    }
    pub fn is_code_token(&self) -> bool {
        !matches!(
            self.token_type,
            TokenType::Whitespace | TokenType::LineComment | TokenType::BlockComment
        )
    }

    pub fn as_source_range(&self) -> SourceRange {
        SourceRange([self.start, self.end])
    }

    pub fn as_source_ranges(&self) -> Vec<SourceRange> {
        vec![self.as_source_range()]
    }

    /// Is this token the beginning of a variable/function declaration?
    /// If so, what kind?
    /// If not, returns None.
    pub fn declaration_keyword(&self) -> Option<VariableKind> {
        if !matches!(self.token_type, TokenType::Keyword) {
            return None;
        }
        Some(match self.value.as_str() {
            "var" => VariableKind::Var,
            "let" => VariableKind::Let,
            "fn" => VariableKind::Fn,
            "const" => VariableKind::Const,
            _ => return None,
        })
    }
}

impl From<Token> for SourceRange {
    fn from(token: Token) -> Self {
        Self([token.start, token.end])
    }
}

impl From<&Token> for SourceRange {
    fn from(token: &Token) -> Self {
        Self([token.start, token.end])
    }
}

pub fn lexer(s: &str) -> Vec<Token> {
    tokeniser::lexer(s).unwrap_or_default()
}
