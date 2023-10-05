use parse_display::{Display, FromStr};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{ast::types::VariableKind, SourceRange};

/// The types of tokens.
#[derive(
    Debug, PartialEq, Eq, Copy, Clone, Deserialize, Serialize, JsonSchema, FromStr, Display,
)]
#[serde(rename_all = "camelCase")]
#[display(style = "camelCase")]
pub enum TokenType {
    /// A word.
    Word,
    /// An operator.
    Operator,
    /// A string.
    String,
    /// A keyword.
    Keyword,
    /// Whitespace.
    Whitespace,
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
