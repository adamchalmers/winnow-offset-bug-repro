use crate::ast::types::VariableKind;

/// The types of tokens.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

impl Token {
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
