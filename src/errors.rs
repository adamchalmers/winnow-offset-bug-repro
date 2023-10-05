use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::SourceRange;

#[derive(Error, Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum KclError {
    #[error("syntax: {0:?}")]
    Syntax(KclErrorDetails),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct KclErrorDetails {
    #[serde(rename = "sourceRanges")]
    pub source_ranges: Vec<SourceRange>,
    #[serde(rename = "msg")]
    pub message: String,
}

impl KclError {
    /// Get the error message, line and column from the error and input code.
    pub fn get_message_line_column(&self, input: &str) -> (String, Option<usize>, Option<usize>) {
        let (type_, source_range, message) = match &self {
            KclError::Syntax(e) => ("syntax", e.source_ranges.clone(), e.message.clone()),
        };

        // Calculate the line and column of the error from the source range.
        let (line, column) = if let Some(range) = source_range.first() {
            let line = input[..range.0[0]].lines().count();
            let column = input[..range.0[0]]
                .lines()
                .last()
                .map(|l| l.len())
                .unwrap_or_default();

            (Some(line), Some(column))
        } else {
            (None, None)
        };

        (format!("{}: {}", type_, message), line, column)
    }
}
