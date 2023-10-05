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
