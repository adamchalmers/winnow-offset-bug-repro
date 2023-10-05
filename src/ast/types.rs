//! Data types for the AST.

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<BodyItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyItem {
    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(Box<Literal>),
    Identifier(Box<Identifier>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Value,
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
    pub id: Identifier,
    /// The value of the variable.
    pub init: Value,
}

impl VariableDeclarator {
    pub fn new(name: &str, init: Value) -> Self {
        Self {
            id: Identifier::new(name),
            init,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: serde_json::Value,
    pub raw: String,
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        Value::Literal(Box::new(literal))
    }
}

impl Literal {
    pub fn new(value: serde_json::Value) -> Self {
        Self {
            raw: value.to_string(),
            value,
        }
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
