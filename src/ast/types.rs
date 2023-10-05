//! Data types for the AST.

use std::collections::HashMap;

use anyhow::Result;
use parse_display::{Display, FromStr};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    errors::{KclError, KclErrorDetails},
    SourceRange,
};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Program {
    pub start: usize,
    pub end: usize,
    pub body: Vec<BodyItem>,
    pub non_code_meta: NonCodeMeta,
}

pub trait ValueMeta {
    fn start(&self) -> usize;

    fn end(&self) -> usize;
}

macro_rules! impl_value_meta {
    {$name:ident} => {
        impl crate::ast::types::ValueMeta for $name {
            fn start(&self) -> usize {
                self.start
            }

            fn end(&self) -> usize {
                self.end
            }
        }

    };
}

pub(crate) use impl_value_meta;

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub enum BodyItem {
    ExpressionStatement(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
}

impl BodyItem {
    pub fn start(&self) -> usize {
        match self {
            BodyItem::ExpressionStatement(expression_statement) => expression_statement.start(),
            BodyItem::VariableDeclaration(variable_declaration) => variable_declaration.start(),
        }
    }

    pub fn end(&self) -> usize {
        match self {
            BodyItem::ExpressionStatement(expression_statement) => expression_statement.end(),
            BodyItem::VariableDeclaration(variable_declaration) => variable_declaration.end(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub enum Value {
    Literal(Box<Literal>),
    Identifier(Box<Identifier>),
}

impl Value {
    pub fn start(&self) -> usize {
        match self {
            Value::Literal(literal) => literal.start(),
            Value::Identifier(identifier) => identifier.start(),
        }
    }

    pub fn end(&self) -> usize {
        match self {
            Value::Literal(literal) => literal.end(),
            Value::Identifier(identifier) => identifier.end(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub enum BinaryPart {
    Literal(Box<Literal>),
    Identifier(Box<Identifier>),
}

impl BinaryPart {
    pub fn start(&self) -> usize {
        match self {
            BinaryPart::Literal(literal) => literal.start(),
            BinaryPart::Identifier(identifier) => identifier.start(),
        }
    }

    pub fn end(&self) -> usize {
        match self {
            BinaryPart::Literal(literal) => literal.end(),
            BinaryPart::Identifier(identifier) => identifier.end(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub struct NonCodeNode {
    pub start: usize,
    pub end: usize,
    pub value: NonCodeValue,
}

impl NonCodeNode {
    pub fn value(&self) -> String {
        match &self.value {
            NonCodeValue::InlineComment { value } => value.clone(),
            NonCodeValue::BlockComment { value } => value.clone(),
            NonCodeValue::NewLineBlockComment { value } => value.clone(),
            NonCodeValue::NewLine => "\n\n".to_string(),
        }
    }

    pub fn format(&self, indentation: &str) -> String {
        match &self.value {
            NonCodeValue::InlineComment { value } => format!(" // {}\n", value),
            NonCodeValue::BlockComment { value } => {
                let add_start_new_line = if self.start == 0 { "" } else { "\n" };
                if value.contains('\n') {
                    format!("{}{}/* {} */\n", add_start_new_line, indentation, value)
                } else {
                    format!("{}{}// {}\n", add_start_new_line, indentation, value)
                }
            }
            NonCodeValue::NewLineBlockComment { value } => {
                let add_start_new_line = if self.start == 0 { "" } else { "\n\n" };
                if value.contains('\n') {
                    format!("{}{}/* {} */\n", add_start_new_line, indentation, value)
                } else {
                    format!("{}{}// {}\n", add_start_new_line, indentation, value)
                }
            }
            NonCodeValue::NewLine => "\n\n".to_string(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum NonCodeValue {
    /// An inline comment.
    /// An example of this is the following: `1 + 1 // This is an inline comment`.
    InlineComment {
        value: String,
    },
    /// A block comment.
    /// An example of this is the following:
    /// ```python,no_run
    /// /* This is a
    ///     block comment */
    /// 1 + 1
    /// ```
    /// Now this is important. The block comment is attached to the next line.
    /// This is always the case. Also the block comment doesnt have a new line above it.
    /// If it did it would be a `NewLineBlockComment`.
    BlockComment {
        value: String,
    },
    /// A block comment that has a new line above it.
    /// The user explicitly added a new line above the block comment.
    NewLineBlockComment {
        value: String,
    },
    // A new line like `\n\n` NOT a new line like `\n`.
    // This is also not a comment.
    NewLine,
}

#[derive(Debug, Default, Clone, Serialize, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct NonCodeMeta {
    pub non_code_nodes: HashMap<usize, NonCodeNode>,
    pub start: Option<NonCodeNode>,
}

// implement Deserialize manually because we to force the keys of non_code_nodes to be usize
// and by default the ts type { [statementIndex: number]: NonCodeNode } serializes to a string i.e. "0", "1", etc.
impl<'de> Deserialize<'de> for NonCodeMeta {
    fn deserialize<D>(deserializer: D) -> Result<NonCodeMeta, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(rename_all = "camelCase")]
        struct NonCodeMetaHelper {
            non_code_nodes: HashMap<String, NonCodeNode>,
            start: Option<NonCodeNode>,
        }

        let helper = NonCodeMetaHelper::deserialize(deserializer)?;
        let mut non_code_nodes = HashMap::new();
        for (key, value) in helper.non_code_nodes {
            non_code_nodes.insert(key.parse().map_err(serde::de::Error::custom)?, value);
        }
        Ok(NonCodeMeta {
            non_code_nodes,
            start: helper.start,
        })
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub struct ExpressionStatement {
    pub start: usize,
    pub end: usize,
    pub expression: Value,
}

impl_value_meta!(ExpressionStatement);

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub struct VariableDeclaration {
    pub start: usize,
    pub end: usize,
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableKind, // Change to enum if there are specific values
}

impl_value_meta!(VariableDeclaration);

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema, FromStr, Display)]
#[serde(rename_all = "snake_case")]
#[display(style = "snake_case")]
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

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub struct VariableDeclarator {
    pub start: usize,
    pub end: usize,
    /// The identifier of the variable.
    pub id: Identifier,
    /// The value of the variable.
    pub init: Value,
}

impl_value_meta!(VariableDeclarator);

impl VariableDeclarator {
    pub fn new(name: &str, init: Value) -> Self {
        Self {
            start: 0,
            end: 0,
            id: Identifier::new(name),
            init,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub struct Literal {
    pub start: usize,
    pub end: usize,
    pub value: serde_json::Value,
    pub raw: String,
}

impl_value_meta!(Literal);

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        Value::Literal(Box::new(literal))
    }
}

impl Literal {
    pub fn new(value: serde_json::Value) -> Self {
        Self {
            start: 0,
            end: 0,
            raw: value.to_string(),
            value,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema)]
#[serde(tag = "type")]
pub struct Identifier {
    pub start: usize,
    pub end: usize,
    pub name: String,
}

impl_value_meta!(Identifier);

impl Identifier {
    pub fn new(name: &str) -> Self {
        Self {
            start: 0,
            end: 0,
            name: name.to_string(),
        }
    }
}

pub fn parse_json_number_as_f64(
    j: &serde_json::Value,
    source_range: SourceRange,
) -> Result<f64, KclError> {
    if let serde_json::Value::Number(n) = &j {
        n.as_f64().ok_or_else(|| {
            KclError::Syntax(KclErrorDetails {
                source_ranges: vec![source_range],
                message: format!("Invalid number: {}", j),
            })
        })
    } else {
        Err(KclError::Syntax(KclErrorDetails {
            source_ranges: vec![source_range],
            message: format!("Invalid number: {}", j),
        }))
    }
}

pub fn parse_json_number_as_usize(
    j: &serde_json::Value,
    source_range: SourceRange,
) -> Result<usize, KclError> {
    if let serde_json::Value::Number(n) = &j {
        Ok(n.as_i64().ok_or_else(|| {
            KclError::Syntax(KclErrorDetails {
                source_ranges: vec![source_range],
                message: format!("Invalid index: {}", j),
            })
        })? as usize)
    } else {
        Err(KclError::Syntax(KclErrorDetails {
            source_ranges: vec![source_range],
            message: format!("Invalid index: {}", j),
        }))
    }
}

pub fn parse_json_value_as_string(j: &serde_json::Value) -> Option<String> {
    if let serde_json::Value::String(n) = &j {
        Some(n.clone())
    } else {
        None
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema, FromStr, Display)]
#[serde(rename_all = "snake_case")]
#[display(style = "snake_case")]
pub enum BinaryOperator {
    /// Add two numbers.
    #[serde(rename = "+")]
    #[display("+")]
    Add,
    /// Subtract two numbers.
    #[serde(rename = "-")]
    #[display("-")]
    Sub,
    /// Multiply two numbers.
    #[serde(rename = "*")]
    #[display("*")]
    Mul,
    /// Divide two numbers.
    #[serde(rename = "/")]
    #[display("/")]
    Div,
    /// Modulo two numbers.
    #[serde(rename = "%")]
    #[display("%")]
    Mod,
}

impl BinaryOperator {
    pub fn precedence(&self) -> u8 {
        match &self {
            BinaryOperator::Add | BinaryOperator::Sub => 11,
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 12,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, JsonSchema, FromStr, Display)]
#[serde(rename_all = "snake_case")]
#[display(style = "snake_case")]
pub enum UnaryOperator {
    /// Negate a number.
    #[serde(rename = "-")]
    #[display("-")]
    Neg,
    /// Negate a boolean.
    #[serde(rename = "!")]
    #[display("!")]
    Not,
}
