//! Data types for the AST.

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<BodyItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyItem {
    ExpressionStatement(()),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableKind, // Change to enum if there are specific values
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclarator {
    /// The identifier of the variable.
    pub id: Token,
    /// The value of the variable.
    pub init: Token,
}

impl VariableDeclarator {
    pub fn new(id: Token, init: Token) -> Self {
        Self { id, init }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}
