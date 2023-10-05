//! Data types for the AST.

#[derive(Debug, Clone, PartialEq)]
pub enum VariableKind {
    /// Declare a variable.
    Let,
    /// Declare a variable that is read-only.
    Const,
    /// Declare a function.
    Fn,
    /// Declare a variable.
    Var,
}
