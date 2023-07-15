use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0},
    error::ParseError,
    multi::fold_many0,
    number::complete::recognize_float,
    sequence::{delimited, pair},
    IResult, InputTake, Offset, Parser,
};
use nom_locate::LocatedSpan;

use inkwell::OptimizationLevel;
use inkwell::{
    builder::Builder,
    context::Context,
    values::{FloatMathValue, FloatValue},
};

fn main() {
    let context = Context::create();
    let i32_type = context.i32_type();
    let i8_type = context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);

    // Module
    let module = context.create_module("main");

    // Function
    let printf_fn_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    let printf_function = module.add_function("printf", printf_fn_type, None);
    let main_fn_type = i32_type.fn_type(&[], false);
    let main_function = module.add_function("main", main_fn_type, None);

    // Block
    let entry_basic_block = context.append_basic_block(main_function, "entry");

    // Instruction(Builder)
    let builder = context.create_builder();
    builder.position_at_end(entry_basic_block);
    let hw_string_ptr = builder.build_global_string_ptr("Compiled: %f\n", "hw");
    let ast = match num_expr(Span::new("40 + 2")) {
        Ok(val) => val.1,
        Err(e) => {
            eprintln!("Parse error: {e}");
            return;
        }
    };
    let code = codegen(&context, &builder, &ast);
    builder.build_call(
        printf_function,
        &[hw_string_ptr.as_pointer_value().into(), code.into()],
        "call",
    );

    // let sum = builder.build_int_add(x, y, "sum");
    // let sum = builder.build_int_add(sum, z, "sum");

    builder.build_return(Some(&i32_type.const_int(0, false)));

    module.print_to_file("out.ll").unwrap();

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .unwrap();
    unsafe {
        execution_engine
            .get_function::<unsafe extern "C" fn()>("main")
            .unwrap()
            .call();
    }
}

fn codegen<'b>(context: &'b Context, builder: &'b Builder, ast: &Expression) -> FloatValue<'b> {
    use ExprEnum::*;
    match &ast.expr {
        NumLiteral(val) => context.f64_type().const_float(*val),
        Add(lhs, rhs) => {
            let lhs = codegen(context, builder, &lhs);
            let rhs = codegen(context, builder, &rhs);
            builder.build_float_add(lhs, rhs, "name")
        }
        Sub(lhs, rhs) => {
            let lhs = codegen(context, builder, &lhs);
            let rhs = codegen(context, builder, &rhs);
            builder.build_float_sub(lhs, rhs, "sub")
        }
        Mul(lhs, rhs) => {
            let lhs = codegen(context, builder, &lhs);
            let rhs = codegen(context, builder, &rhs);
            builder.build_float_mul(lhs, rhs, "mul")
        }
        Div(lhs, rhs) => {
            let lhs = codegen(context, builder, &lhs);
            let rhs = codegen(context, builder, &rhs);
            builder.build_float_mul(lhs, rhs, "mul")
        }
    }
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'a> {
    pub(crate) expr: ExprEnum<'a>,
    pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
        Self { expr, span }
    }
}

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
    alt((num_literal, parens))(i)
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
