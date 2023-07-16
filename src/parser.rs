use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, terminated},
    Finish, IResult, InputTake, Offset, Parser,
};

use crate::{ExprEnum, Expression, Span, Statement, Statements};

fn space_delimited<'src, O, E>(
    f: impl Parser<Span<'src>, O, E>,
) -> impl FnMut(Span<'src>) -> IResult<Span<'src>, O, E>
where
    E: ParseError<Span<'src>>,
{
    delimited(multispace0, f, multispace0)
}

fn parens(i: Span) -> IResult<Span, Expression> {
    space_delimited(delimited(tag("("), num_expr, tag(")")))(i)
}

fn func_call(i: Span) -> IResult<Span, Expression> {
    let (r, ident) = space_delimited(identifier)(i)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(
            multispace0,
            num_expr,
            space_delimited(opt(tag(","))),
        )),
        tag(")"),
    ))(r)?;
    Ok((
        r,
        Expression::new(ExprEnum::FnInvoke(*ident, args), calc_offset(i, r)),
    ))
}

fn ident(input: Span) -> IResult<Span, Expression> {
    let (r, res) = space_delimited(identifier)(input)?;
    Ok((
        r,
        Expression::new(ExprEnum::Ident(*res), calc_offset(input, r)),
    ))
}

fn identifier(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn num_literal(input: Span) -> IResult<Span, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::NumLiteral(v.parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input,
                    code: nom::error::ErrorKind::Digit,
                })
            })?),
            v,
        ),
    ))
}

/// Calculate offset between the start positions of the input spans and return a span between them.
///
/// Note: `i` shall start earlier than `r`, otherwise wrapping would occur.
pub(crate) fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
    i.take(i.offset(&r))
}

fn factor(i: Span) -> IResult<Span, Expression> {
    alt((num_literal, func_call, ident, parens))(i)
}

fn term(i: Span) -> IResult<Span, Expression> {
    let (r, init) = factor(i)?;

    let res = fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(i, acc.span);
            match op {
                '*' => Expression::new(ExprEnum::Mul(Box::new(acc), Box::new(val)), span),
                '/' => Expression::new(ExprEnum::Div(Box::new(acc), Box::new(val)), span),
                _ => panic!(
                    "Multiplicative expression should have '*' \
                or '/' operator"
                ),
            }
        },
    )(r);
    res
}

fn num_expr(i: Span) -> IResult<Span, Expression> {
    let (r, init) = term(i)?;

    let res = fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(i, acc.span);
            match op {
                '+' => Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span),
                '-' => Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span),
                _ => panic!("Additive expression should have '+' or '-' operator"),
            }
        },
    )(r);
    res
}

fn var_def(i: Span) -> IResult<Span, Statement> {
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(num_expr)(i)?;
    let (i, _) = char(';')(i)?;
    Ok((i, Statement::VarDef(*name, expr)))
}

fn open_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

fn close_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(char('}'))(i)?;
    Ok((i, ()))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("("))(i)?;
    let (i, args) = separated_list0(char(','), space_delimited(identifier))(i)?;
    let (i, _) = space_delimited(tag(")"))(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::FnDef {
            name: *name,
            args: args.into_iter().map(|arg| *arg).collect(),
            stmts,
        },
    ))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, res) = num_expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn general_statement<'a>(last: bool) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Statement> {
    let terminator = move |i| -> IResult<Span<'a>, ()> {
        let mut semicolon = pair(tag(";"), multispace0);
        if last {
            Ok((opt(semicolon)(i)?.0, ()))
        } else {
            Ok((semicolon(i)?.0, ()))
        }
    };
    move |input| {
        alt((
            var_def,
            fn_def_statement,
            terminated(expr_statement, terminator),
        ))(input)
    }
}

pub(crate) fn last_statement(input: Span) -> IResult<Span, Statement> {
    general_statement(true)(input)
}

pub(crate) fn statement(input: Span) -> IResult<Span, Statement> {
    general_statement(false)(input)
}

fn statements(i: Span) -> IResult<Span, Statements> {
    let (i, mut stmts) = many0(statement)(i)?;
    let (i, last) = opt(last_statement)(i)?;
    let (i, _) = opt(multispace0)(i)?;
    if let Some(last) = last {
        stmts.push(last);
    }
    Ok((i, stmts))
}

pub(crate) fn statements_finish(i: Span) -> Result<Statements, nom::error::Error<Span>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}
