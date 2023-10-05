use winnow::error::{ErrorKind, ParseError, StrContext};

use crate::{
    errors::{KclError, KclErrorDetails},
    token::Token,
};

/// Accumulate context while backtracking errors
/// Very similar to [`winnow::error::ContextError`] type,
/// but the 'cause' field is always a [`KclError`],
/// instead of a dynamic [`std::error::Error`] trait object.
#[derive(Debug, Clone)]
pub struct ContextError<C = StrContext> {
    pub context: Vec<C>,
    pub cause: Option<KclError>,
}

impl From<ParseError<&[Token], ContextError>> for KclError {
    fn from(err: ParseError<&[Token], ContextError>) -> Self {
        let token_index = err.offset();
        eprintln!(
            "Input length is {}, error occurred at offset {token_index}",
            err.input().len()
        );
        let bad_token = &err.input()[token_index];
        let err = err.into_inner();
        // TODO: use _all_ the context entries, instead of just the first one.
        // See https://github.com/KittyCAD/modeling-app/issues/784
        let cause = err
            .context
            .first()
            .map(|c| c.to_string())
            .unwrap_or("unknown".to_owned());
        match err.cause {
            None => KclError::Syntax(KclErrorDetails {
                source_ranges: bad_token.as_source_ranges(),
                message: cause,
            }),
            Some(e) => e,
        }
    }
}

impl<C> From<KclError> for ContextError<C> {
    fn from(e: KclError) -> Self {
        Self {
            context: Default::default(),
            cause: Some(e),
        }
    }
}

impl<C> std::default::Default for ContextError<C> {
    fn default() -> Self {
        Self {
            context: Default::default(),
            cause: None,
        }
    }
}

impl<I, C> winnow::error::ParserError<I> for ContextError<C> {
    #[inline]
    fn from_error_kind(_input: &I, _kind: ErrorKind) -> Self {
        Self::default()
    }

    #[inline]
    fn append(self, _input: &I, _kind: ErrorKind) -> Self {
        self
    }

    #[inline]
    fn or(self, other: Self) -> Self {
        other
    }
}

impl<C, I> winnow::error::AddContext<I, C> for ContextError<C> {
    #[inline]
    fn add_context(mut self, _input: &I, ctx: C) -> Self {
        self.context.push(ctx);
        self
    }
}

impl<C, I> winnow::error::FromExternalError<I, KclError> for ContextError<C> {
    #[inline]
    fn from_external_error(_input: &I, _kind: ErrorKind, e: KclError) -> Self {
        let mut err = Self::default();
        {
            err.cause = Some(e);
        }
        err
    }
}
