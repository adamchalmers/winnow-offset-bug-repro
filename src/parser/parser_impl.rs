use serde_json::{Number as JNumber, Value as JValue};
use winnow::{
    combinator::{alt, opt, peek, preceded, repeat, separated0, separated1, terminated},
    dispatch,
    error::StrContext::Label,
    prelude::*,
    token::any,
};

use crate::{
    ast::types::{
        ArrayExpression, BinaryPart, BodyItem, CallExpression, ExpressionStatement,
        FunctionExpression, Identifier, Literal, NonCodeMeta, NonCodeNode, NonCodeValue,
        ObjectExpression, ObjectProperty, PipeExpression, PipeSubstitution, Program,
        ReturnStatement, UnaryExpression, UnaryOperator, Value, VariableDeclaration,
        VariableDeclarator,
    },
    errors::{KclError, KclErrorDetails},
    token::{Token, TokenType},
    SourceRange,
};

mod error;

type PResult<O, E = error::ContextError> = winnow::prelude::PResult<O, E>;

type TokenSlice<'slice, 'input> = &'slice mut &'input [Token];

pub fn run_parser(i: TokenSlice) -> Result<Program, KclError> {
    if i.is_empty() {
        return Err(KclError::Syntax(KclErrorDetails {
            source_ranges: vec![],
            message: "file is empty".to_string(),
        }));
    }

    program.parse(i).map_err(KclError::from)
}

fn body_item(i: TokenSlice) -> PResult<BodyItem> {
    dispatch! {peek(any);
        token @ Token { .. } if token.declaration_keyword().is_some() => declaration.map(BodyItem::VariableDeclaration),
        _ => expression.map(BodyItem::ExpressionStatement),
    }
    .context(Label("a KCL program body item, i.e. a declaration or expression"))
    .parse_next(i)
}

fn program(i: TokenSlice) -> PResult<Program> {
    ignore_whitespace(i);
    let body: Vec<_> = separated1(body_item, whitespace)
        .context(Label(
            "at least one KCL body item, i.e. a declaration or expression",
        ))
        .parse_next(i)?;
    let trailing_ws = opt(whitespace).parse_next(i)?.unwrap_or_default();
    let end = trailing_ws
        .last()
        .map(|tok| tok.end)
        .unwrap_or_else(|| match body.last().unwrap() {
            BodyItem::VariableDeclaration(v) => v.end,
            BodyItem::ExpressionStatement(e) => e.end,
            BodyItem::ReturnStatement(r) => r.end,
        });
    Ok(Program {
        start: 0,
        end,
        body,
        non_code_meta: Default::default(), // TODO: support comments.
    })
}

fn pipe_surrounded_by_whitespace(i: TokenSlice) -> PResult<()> {
    (
        repeat(0.., whitespace).map(|_: Vec<_>| ()),
        pipe_operator,
        repeat(0.., whitespace).map(|_: Vec<_>| ()),
    )
        .parse_next(i)?;
    Ok(())
}

/// Matches all four cases of NonCodeValue
fn non_code_node(i: TokenSlice) -> PResult<NonCodeNode> {
    /// Matches one case of NonCodeValue
    /// See docstring on [NonCodeValue::NewLineBlockComment] for why that case is different to the others.
    fn non_code_node_leading_whitespace(i: TokenSlice) -> PResult<NonCodeNode> {
        let leading_whitespace = any
            .verify(|token: &Token| {
                token.token_type == TokenType::Whitespace && token.value.contains('\n')
            })
            .context(Label("whitespace, with a newline"))
            .parse_next(i)?;
        non_code_node_no_leading_whitespace
            .verify_map(|node: NonCodeNode| match node.value {
                NonCodeValue::BlockComment { value } => Some(NonCodeNode {
                    start: leading_whitespace.start,
                    end: node.end,
                    value: NonCodeValue::NewLineBlockComment { value },
                }),
                _ => None,
            })
            .context(Label("a comment or whitespace"))
            .parse_next(i)
    }

    // Matches remaining three cases of NonCodeValue
    fn non_code_node_no_leading_whitespace(i: TokenSlice) -> PResult<NonCodeNode> {
        any.verify_map(|token: Token| {
            if token.is_code_token() {
                None
            } else {
                let value = match token.token_type {
                    TokenType::Whitespace if token.value.contains('\n') => NonCodeValue::NewLine,
                    TokenType::LineComment => NonCodeValue::InlineComment {
                        value: token.value.trim_start_matches("//").trim().to_owned(),
                    },
                    TokenType::BlockComment => NonCodeValue::BlockComment {
                        value: token
                            .value
                            .trim_start_matches("/*")
                            .trim_end_matches("*/")
                            .trim()
                            .to_owned(),
                    },
                    _ => return None,
                };
                Some(NonCodeNode {
                    start: token.start,
                    end: token.end,
                    value,
                })
            }
        })
        .context(Label("Non-code token (comments or whitespace)"))
        .parse_next(i)
    }
    alt((
        non_code_node_leading_whitespace,
        non_code_node_no_leading_whitespace,
    ))
    .parse_next(i)
}

fn pipe_expression(i: TokenSlice) -> PResult<PipeExpression> {
    let mut non_code_meta = NonCodeMeta::default();
    let (head, noncode) = terminated(
        (value_but_not_pipe, preceded(whitespace, opt(non_code_node))),
        peek(pipe_surrounded_by_whitespace),
    )
    .context(Label("an expression, followed by the |> (pipe) operator"))
    .parse_next(i)?;
    if let Some(nc) = noncode {
        non_code_meta.non_code_nodes.insert(0, nc);
    }
    let mut values = vec![head];
    let value_surrounded_by_comments = (
        repeat(0.., preceded(opt(whitespace), non_code_node)),
        // terminated(value_but_not_pipe, opt(whitespace)),
        preceded(opt(whitespace), value_but_not_pipe),
        repeat(0.., preceded(opt(whitespace), non_code_node)),
    );
    let tail: Vec<(Vec<_>, _, Vec<_>)> = repeat(
        1..,
        preceded(pipe_surrounded_by_whitespace, value_surrounded_by_comments),
    )
    .context(Label(
        "a sequence of at least one |> (pipe) operator, followed by an expression",
    ))
    .parse_next(i)?;

    // All child parsers have been run. Time to structure the return value.
    let mut code_count = 0;
    let mut max_noncode_end = 0;
    for (noncode_before, code, noncode_after) in tail {
        for nc in noncode_before {
            max_noncode_end = nc.end.max(max_noncode_end);
            non_code_meta.non_code_nodes.insert(code_count, nc);
        }
        values.push(code);
        code_count += 1;
        for nc in noncode_after {
            max_noncode_end = nc.end.max(max_noncode_end);
            non_code_meta.non_code_nodes.insert(code_count, nc);
        }
    }
    Ok(PipeExpression {
        start: values.first().unwrap().start(),
        end: values.last().unwrap().end().max(max_noncode_end),
        body: values,
        non_code_meta,
    })
}

fn bool_value(i: TokenSlice) -> PResult<Identifier> {
    let (name, token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::Keyword if token.value == "true" => Ok(("true", token)),
            TokenType::Keyword if token.value == "false" => Ok(("false", token)),
            _ => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: "invalid boolean literal".to_owned(),
            })),
        })
        .context(Label("a boolean literal (either true or false)"))
        .parse_next(i)?;
    Ok(Identifier {
        start: token.start,
        end: token.end,
        name: name.to_owned(),
    })
}

pub fn literal(i: TokenSlice) -> PResult<Literal> {
    alt((string_literal, unsigned_number_literal))
        .context(Label("a KCL literal, like 'myPart' or 3"))
        .parse_next(i)
}

/// Parse a KCL string literal
pub fn string_literal(i: TokenSlice) -> PResult<Literal> {
    let (value, token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::String => {
                let s = token.value[1..token.value.len() - 1].to_string();
                Ok((JValue::String(s), token))
            }
            _ => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: "invalid string literal".to_owned(),
            })),
        })
        .context(Label("string literal (like \"myPart\""))
        .parse_next(i)?;
    Ok(Literal {
        start: token.start,
        end: token.end,
        value,
        raw: token.value.clone(),
    })
}

/// Parse a KCL literal number, with no - sign.
fn unsigned_number_literal(i: TokenSlice) -> PResult<Literal> {
    let (value, token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::Number => {
                if let Ok(x) = token.value.parse::<i64>() {
                    return Ok((JValue::Number(JNumber::from(x)), token));
                }
                let x: f64 = token.value.parse().map_err(|_| {
                    KclError::Syntax(KclErrorDetails {
                        source_ranges: token.as_source_ranges(),
                        message: format!("Invalid float: {}", token.value),
                    })
                })?;

                match JNumber::from_f64(x) {
                    Some(n) => Ok((JValue::Number(n), token)),
                    None => Err(KclError::Syntax(KclErrorDetails {
                        source_ranges: token.as_source_ranges(),
                        message: format!("Invalid float: {}", token.value),
                    })),
                }
            }
            _ => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: "invalid literal".to_owned(),
            })),
        })
        .context(Label("an unsigned number literal (e.g. 3 or 12.5)"))
        .parse_next(i)?;
    Ok(Literal {
        start: token.start,
        end: token.end,
        value,
        raw: token.value.clone(),
    })
}

/// Parse a KCL operand that can be used with an operator.
fn operand(i: TokenSlice) -> PResult<BinaryPart> {
    const TODO_783: &str = "found a value, but this kind of value cannot be used as the operand to an operator yet (see https://github.com/KittyCAD/modeling-app/issues/783)";
    let op = possible_operands
        .try_map(|part| {
            let val = match part {
                // TODO: these should be valid operands eventually,
                // users should be able to run "let x = f() + g()"
                // see https://github.com/KittyCAD/modeling-app/issues/783
                Value::FunctionExpression(_)
                | Value::PipeExpression(_)
                | Value::PipeSubstitution(_)
                | Value::ArrayExpression(_)
                | Value::ObjectExpression(_) => {
                    return Err(KclError::Syntax(KclErrorDetails {
                        source_ranges: vec![SourceRange([part.start(), part.end()])],
                        message: TODO_783.to_owned(),
                    }))
                }
                Value::UnaryExpression(x) => BinaryPart::UnaryExpression(x),
                Value::Literal(x) => BinaryPart::Literal(x),
                Value::Identifier(x) => BinaryPart::Identifier(x),
                Value::CallExpression(x) => BinaryPart::CallExpression(x),
            };
            Ok(val)
        })
        .context(Label(
            "an operand (a value which can be used with an operator)",
        ))
        .parse_next(i)?;
    Ok(op)
}

impl TokenType {
    fn parse_from(self, i: TokenSlice) -> PResult<Token> {
        any.try_map(|token: Token| {
            if token.token_type == self {
                Ok(token)
            } else {
                Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!(
                        "expected {self} but found {} which is a {}",
                        token.value.as_str(),
                        token.token_type
                    ),
                }))
            }
        })
        .parse_next(i)
    }
}

/// Parse some whitespace (i.e. at least one whitespace token)
fn whitespace(i: TokenSlice) -> PResult<Vec<Token>> {
    repeat(
        1..,
        any.try_map(|token: Token| {
            if token.token_type == TokenType::Whitespace {
                Ok(token)
            } else {
                Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!(
                        "expected whitespace, found '{}' which is {}",
                        token.value.as_str(),
                        token.token_type
                    ),
                }))
            }
        }),
    )
    .context(Label("some whitespace (e.g. spaces, tabs, new lines)"))
    .parse_next(i)
}

/// Parse the = operator.
fn equals(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Operator) && token.value == "="
    })
    .context(Label("the equals operator, ="))
    .parse_next(i)
}

/// Parse a KCL array of elements.
fn array(i: TokenSlice) -> PResult<ArrayExpression> {
    let start = open_bracket(i)?.start;
    ignore_whitespace(i);
    let elements = alt((integer_range, separated0(value, comma_sep)))
        .context(Label(
            "array contents, either a numeric range (like 0..10) or a list of elements (like [1, 2, 3])",
        ))
        .parse_next(i)?;
    ignore_whitespace(i);
    let end = close_bracket(i)?.end;
    Ok(ArrayExpression {
        start,
        end,
        elements,
    })
}

/// Parse n..m into a vec of numbers [n, n+1, ..., m]
fn integer_range(i: TokenSlice) -> PResult<Vec<Value>> {
    let (token0, floor) = integer.parse_next(i)?;
    double_period.parse_next(i)?;
    let (_token1, ceiling) = integer.parse_next(i)?;
    Ok((floor..=ceiling)
        .map(|num| {
            Value::Literal(Box::new(Literal {
                start: token0.start,
                end: token0.end,
                value: JValue::Number(num.into()),
                raw: num.to_string(),
            }))
        })
        .collect())
}

fn object_property(i: TokenSlice) -> PResult<ObjectProperty> {
    let key = identifier
        .context(Label(
            "the property's key (the name or identifier of the property), e.g. in 'height: 4', 'height' is the property key",
        ))
        .parse_next(i)?;
    colon
        .context(Label(
            "a colon, which separates the property's key from the value you're setting it to, e.g. 'height: 4'",
        ))
        .parse_next(i)?;
    ignore_whitespace(i);
    let val = value
        .context(Label(
            "the value which you're setting the property to, e.g. in 'height: 4', the value is 4",
        ))
        .parse_next(i)?;
    Ok(ObjectProperty {
        start: key.start,
        end: val.end(),
        key,
        value: val,
    })
}

/// Parse a KCL object value.
fn object(i: TokenSlice) -> PResult<ObjectExpression> {
    let start = open_brace(i)?.start;
    ignore_whitespace(i);
    let properties = separated0(object_property, comma_sep)
        .context(Label(
            "a comma-separated list of properties, e.g. 'height: 4, width: 3'",
        ))
        .parse_next(i)?;
    ignore_whitespace(i);
    let end = close_brace(i)?.end;
    Ok(ObjectExpression {
        start,
        end,
        properties,
    })
}

/// Parse the % symbol, used to substitute a curried argument from a |> (pipe).
fn pipe_sub(i: TokenSlice) -> PResult<PipeSubstitution> {
    any.try_map(|token: Token| {
        if matches!(token.token_type, TokenType::Operator) && token.value == "%" {
            Ok(PipeSubstitution {
                start: token.start,
                end: token.end,
            })
        } else {
            Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "expected a pipe substitution symbol (%) but found {}",
                    token.value.as_str()
                ),
            }))
        }
    })
    .context(Label("the substitution symbol, %"))
    .parse_next(i)
}

// Looks like
// (arg0, arg1) => {
//     const x = arg0 + arg1;
//     return x
// }
fn function_expression(i: TokenSlice) -> PResult<FunctionExpression> {
    let start = open_paren(i)?.start;
    let params = parameters(i)?;
    close_paren(i)?;
    ignore_whitespace(i);
    big_arrow(i)?;
    ignore_whitespace(i);
    open_brace(i)?;
    let body = function_body(i)?;
    let end = close_brace(i)?.end;
    Ok(FunctionExpression {
        start,
        end,
        params,
        body,
    })
}

/// Parse the body of a user-defined function.
pub fn function_body(i: TokenSlice) -> PResult<Program> {
    let leading_whitespace = opt(whitespace).parse_next(i)?;
    let body: Vec<_> =
        separated1(
            alt((
                return_stmt.map(BodyItem::ReturnStatement),
                declaration.map(BodyItem::VariableDeclaration),
                expression.map(BodyItem::ExpressionStatement),
            )),
            ws_with_newline,
        )
        .context(Label("a sequence of function body items (functions are made up of variable declarations, expressions, and return statements, each of those is a possible body item"))
        .parse_next(i)?;
    let start_whitespace = leading_whitespace.and_then(|w| w.first().map(|tok| tok.start));
    // Safe to unwrap `body.first()` because `body` is `separated1` therefore guaranteed
    // to have len >= 1.
    let start = start_whitespace.unwrap_or_else(|| body.first().unwrap().start()) - 1;
    let end_ws = opt(whitespace)
        .parse_next(i)?
        .and_then(|ws| ws.first().map(|tok| tok.end));
    // Safe to unwrap `body.last()` for the same reason as `body.first()`.
    let end = end_ws.unwrap_or_else(|| match body.last().unwrap() {
        BodyItem::VariableDeclaration(v) => v.end,
        BodyItem::ExpressionStatement(e) => e.end,
        BodyItem::ReturnStatement(r) => r.end,
    }) + 1;
    Ok(Program {
        start,
        end,
        body,
        non_code_meta: Default::default(), // TODO: support comments
    })
}

/// Parse a return statement of a user-defined function, e.g. `return x`.
pub fn return_stmt(i: TokenSlice) -> PResult<ReturnStatement> {
    let start = any
        .try_map(|token: Token| {
            if matches!(token.token_type, TokenType::Keyword) && token.value == "return" {
                Ok(token.start)
            } else {
                Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!("{} is not a return keyword", token.value.as_str()),
                }))
            }
        })
        .context(Label(
            "the 'return' keyword, which ends your function (and becomes this function's value when it's called)",
        ))
        .parse_next(i)?;
    require_whitespace(i)?;
    let argument = value(i)?;
    Ok(ReturnStatement {
        start,
        end: argument.end(),
        argument,
    })
}

/// Parse a KCL value
fn value(i: TokenSlice) -> PResult<Value> {
    alt((
        pipe_expression.map(Box::new).map(Value::PipeExpression),
        value_but_not_pipe,
    ))
    .context(Label("a KCL value"))
    .parse_next(i)
}

fn value_but_not_pipe(i: TokenSlice) -> PResult<Value> {
    alt((
        unary_expression.map(Box::new).map(Value::UnaryExpression),
        bool_value.map(Box::new).map(Value::Identifier),
        literal.map(Box::new).map(Value::Literal),
        fn_call.map(Box::new).map(Value::CallExpression),
        identifier.map(Box::new).map(Value::Identifier),
        array.map(Box::new).map(Value::ArrayExpression),
        object.map(Box::new).map(Value::ObjectExpression),
        pipe_sub.map(Box::new).map(Value::PipeSubstitution),
        function_expression
            .map(Box::new)
            .map(Value::FunctionExpression),
    ))
    .context(Label("a KCL value (but not a pipe expression)"))
    .parse_next(i)
}

fn possible_operands(i: TokenSlice) -> PResult<Value> {
    alt((
        unary_expression.map(Box::new).map(Value::UnaryExpression),
        bool_value.map(Box::new).map(Value::Identifier),
        literal.map(Box::new).map(Value::Literal),
        fn_call.map(Box::new).map(Value::CallExpression),
        identifier.map(Box::new).map(Value::Identifier),
    ))
    .context(Label(
        "a KCL value which can be used as an argument/operand to an operator",
    ))
    .parse_next(i)
}

/// Parse a variable/constant declaration.
fn declaration(i: TokenSlice) -> PResult<VariableDeclaration> {
    const EXPECTED: &str = "expected a variable declaration keyword (e.g. 'let') but found";
    let (kind, start) = any
        .try_map(|token: Token| {
            let Some(kind) = token.declaration_keyword() else {
                return Err(KclError::Syntax(KclErrorDetails {
                    source_ranges: token.as_source_ranges(),
                    message: format!("{EXPECTED} {}", token.value.as_str()),
                }));
            };

            Ok((kind, token.start))
        })
        .context(Label("declaring a name, e.g. 'let width = 3'"))
        .parse_next(i)?;
    require_whitespace(i)?;
    let id = identifier
        .context(Label(
            "an identifier, which becomes name you're binding the value to",
        ))
        .parse_next(i)?;
    ignore_whitespace(i);
    equals(i)?;
    ignore_whitespace(i);

    let val = value
        .context(Label("a KCL value, which is being bound to a variable"))
        .parse_next(i)?;
    let end = val.end();
    Ok(VariableDeclaration {
        start,
        end,
        declarations: vec![VariableDeclarator {
            start: id.start,
            end,
            id,
            init: val,
        }],
        kind,
    })
}

/// Parse a KCL identifier (name of a constant/variable/function)
fn identifier(i: TokenSlice) -> PResult<Identifier> {
    any.try_map(|token: Token| {
        if token.token_type == TokenType::Word {
            Ok(Identifier {
                start: token.start,
                end: token.end,
                name: token.value,
            })
        } else {
            Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "{} is not an identifier, it is a {}",
                    token.value.as_str(),
                    token.token_type
                ),
            }))
        }
    })
    .context(Label("an identifier, e.g. 'width' or 'myPart'"))
    .parse_next(i)
}

/// Helper function. Matches any number of whitespace tokens and ignores them.
fn ignore_whitespace(i: TokenSlice) {
    let _: PResult<()> = repeat(0.., whitespace).parse_next(i);
}

/// Matches at least 1 whitespace.
fn require_whitespace(i: TokenSlice) -> PResult<()> {
    repeat(1.., whitespace).parse_next(i)
}

fn unary_expression(i: TokenSlice) -> PResult<UnaryExpression> {
    const EXPECTED: &str = "expected a unary operator (like '-', the negative-numeric operator),";
    let (operator, op_token) = any
        .try_map(|token: Token| match token.token_type {
            TokenType::Operator if token.value == "-" => Ok((UnaryOperator::Neg, token)),
            // TODO: negation. Original parser doesn't support `not` yet.
            TokenType::Operator => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "{EXPECTED} but found {} which is an operator, but not a unary one (unary operators apply to just a single operand, your operator applies to two or more operands)",
                    token.value.as_str(),
                ),
            })),
            other => Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "{EXPECTED} but found {} which is {}",
                    token.value.as_str(),
                    other,
                ),
            })),
        })
        .context(Label("a unary expression, e.g. -x or -3"))
        .parse_next(i)?;
    let argument = operand.parse_next(i)?;
    Ok(UnaryExpression {
        start: op_token.start,
        end: argument.end(),
        operator,
        argument,
    })
}

/// Parse a KCL expression.
fn expression(i: TokenSlice) -> PResult<ExpressionStatement> {
    let val = value
        .context(Label(
            "an expression (i.e. a value, or an algorithm for calculating one), e.g. 'x + y' or '3' or 'width * 2'",
        ))
        .parse_next(i)?;
    Ok(ExpressionStatement {
        start: val.start(),
        end: val.end(),
        expression: val,
    })
}

/// Parse a KCL integer, and the token that held it.
fn integer(i: TokenSlice) -> PResult<(Token, u64)> {
    let num = any
        .verify(|token: &Token| matches!(token.token_type, TokenType::Number))
        .context(Label("a number token e.g. 3"))
        .try_map(|token: Token| {
            let source_ranges = token.as_source_ranges();
            let value = token.value.clone();
            token.value.parse().map(|num| (token, num)).map_err(|e| {
                KclError::Syntax(KclErrorDetails {
                    source_ranges,
                    message: format!("invalid integer {value}: {e}"),
                })
            })
        })
        .context(Label("an integer e.g. 3 (but not 3.1)"))
        .parse_next(i)?;
    Ok(num)
}

/// Parse the given brace symbol.
fn some_brace(symbol: &'static str, i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Brace) && token.value == symbol
    })
    .context(Label(symbol))
    .parse_next(i)
}

/// Parse a => operator.
fn big_arrow(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Operator) && token.value == "=>"
    })
    .context(Label("the => symbol, used for declaring functions"))
    .parse_next(i)
}
/// Parse a |> operator.
fn pipe_operator(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Operator) && token.value == "|>"
    })
    .context(Label(
        "the |> operator, used for 'piping' one function's output into another function's input",
    ))
    .parse_next(i)
}

fn ws_with_newline(i: TokenSlice) -> PResult<Token> {
    any.verify(|token: &Token| {
        matches!(token.token_type, TokenType::Whitespace) && token.value.contains('\n')
    })
    .context(Label("a newline, possibly with whitespace"))
    .parse_next(i)
}

/// (
fn open_paren(i: TokenSlice) -> PResult<Token> {
    some_brace("(", i)
}

/// )
fn close_paren(i: TokenSlice) -> PResult<Token> {
    some_brace(")", i)
}

/// [
fn open_bracket(i: TokenSlice) -> PResult<Token> {
    some_brace("[", i)
}

/// ]
fn close_bracket(i: TokenSlice) -> PResult<Token> {
    some_brace("]", i)
}

/// {
fn open_brace(i: TokenSlice) -> PResult<Token> {
    some_brace("{", i)
}

/// }
fn close_brace(i: TokenSlice) -> PResult<Token> {
    some_brace("}", i)
}

fn comma(i: TokenSlice) -> PResult<()> {
    TokenType::Comma.parse_from(i)?;
    Ok(())
}

fn double_period(i: TokenSlice) -> PResult<Token> {
    any.try_map(|token: Token| {
        if matches!(token.token_type, TokenType::DoublePeriod) {
            Ok(token)
        } else {
            Err(KclError::Syntax(KclErrorDetails {
                source_ranges: token.as_source_ranges(),
                message: format!(
                    "expected a '..' (double period) found {} which is {}",
                    token.value.as_str(),
                    token.token_type
                ),
            }))
        }
    })
    .context(Label("the .. operator, used for array ranges like [0..10]"))
    .parse_next(i)
}

fn colon(i: TokenSlice) -> PResult<()> {
    TokenType::Colon.parse_from(i)?;
    Ok(())
}

/// Parse a comma, optionally followed by some whitespace.
fn comma_sep(i: TokenSlice) -> PResult<()> {
    (comma, opt(whitespace))
        .context(Label("a comma, optionally followed by whitespace"))
        .parse_next(i)?;
    Ok(())
}

/// Arguments are passed into a function.
fn arguments(i: TokenSlice) -> PResult<Vec<Value>> {
    separated0(value, comma_sep)
        .context(Label("function arguments"))
        .parse_next(i)
}

/// Parameters are declared in a function signature, and used within a function.
fn parameters(i: TokenSlice) -> PResult<Vec<Identifier>> {
    separated0(identifier, comma_sep)
        .context(Label("function parameters"))
        .parse_next(i)
}

fn fn_call(i: TokenSlice) -> PResult<CallExpression> {
    let fn_name = identifier(i)?;
    let _ = open_paren(i)?;
    let args = arguments(i)?;
    let end = close_paren(i)?.end;
    Ok(CallExpression {
        start: fn_name.start,
        end,
        callee: fn_name,
        arguments: args,
        optional: false,
    })
}
