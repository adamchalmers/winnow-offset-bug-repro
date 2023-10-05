use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Error, Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum KclError {
    #[error("syntax: {0:?}")]
    Syntax(KclErrorDetails),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct KclErrorDetails {
    #[serde(rename = "msg")]
    pub message: String,
}
