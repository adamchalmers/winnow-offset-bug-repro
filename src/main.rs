use winnow::{prelude::*, token::any};

fn main() {
    let tokens = [Token('a')];

    let result = declaration.parse(&mut tokens.as_slice());
    eprintln!("{result:#?}");
    let err = result.unwrap_err();
    let stream = err.input();

    // Expectation: I can find the bad token (the token which caused the error
    // by indexing err.offset() into err.input().
    // However, this actually gives an index-out-of-bounds panic, because
    // the offset is 1 and the input slice only has length 1.
    let bad_token = &stream[err.offset()];
    eprintln!("Bad token: {bad_token:?}");
}

/* The stream is a vec of custom tokens. */
type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token(char);

/* Parsers */

/// Parse a variable/constant declaration.
pub fn declaration(i: TokenSlice) -> PResult<()> {
    any.parse_next(i)?;
    any.parse_next(i)?;
    Ok(())
}
