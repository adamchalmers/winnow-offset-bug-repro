use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum KclError {
    #[error("syntax: {0:?}")]
    Syntax(KclErrorDetails),
}

#[derive(Debug, Clone)]
pub struct KclErrorDetails {
    pub message: String,
}
