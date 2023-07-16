use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, terminated},
    Finish, IResult, InputTake, Offset, Parser,
};
use nom_locate::LocatedSpan;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FloatValue, FunctionValue},
    OptimizationLevel,
};

fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read from stdin");
    }
    build_program(&buf);
}

#[derive(Clone)]
struct Compiler<'b, 'c, B> {
    context: &'b Context,
    module: &'c Module<'b>,
    builder: &'c B,
    printf_function: &'c FunctionValue<'b>,
    user_functions: Rc<RefCell<HashMap<String, FunctionValue<'b>>>>,
}

impl<'b, 'c> Compiler<'b, 'c, ()> {
    fn convert(&self, builder: &'c Builder<'b>) -> Compiler<'b, 'c, Builder<'b>> {
        Compiler::<Builder> {
            context: self.context,
            module: self.module,
            builder,
            printf_function: self.printf_function,
            user_functions: self.user_functions.clone(),
        }
    }
}

fn build_program(source: &str) {
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

    let ast = match statements_finish(Span::new(source)) {
        Ok(val) => val,
        Err(e) => {
            eprintln!("Parse error: {e}");
            return;
        }
    };

    dbg!(&ast);

    let user_functions = Rc::new(RefCell::new(HashMap::new()));

    let compiler = Compiler::<()> {
        context: &context,
        module: &module,
        builder: &(),
        printf_function: &printf_function,
        user_functions: user_functions.clone(),
    };

    for stmt in &ast {
        compile_fn_statement(&compiler, stmt);
    }

    // Block
    let entry_basic_block = context.append_basic_block(main_function, "entry");

    // Instruction(Builder)
    let builder = context.create_builder();
    builder.position_at_end(entry_basic_block);

    let compiler = Compiler::<Builder> {
        context: &context,
        module: &module,
        builder: &builder,
        printf_function: &printf_function,
        user_functions: user_functions.clone(),
    };

    for stmt in &ast {
        compile_print_statement(&compiler, stmt);
    }

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

fn compile_fn_statement<'b, 'c>(compiler: &'c Compiler<'b, 'c, ()>, ast: &Statement)
where
    'b: 'c,
{
    match ast {
        Statement::FnDef { name, expr, .. } => {
            let fn_type = compiler.context.f64_type().fn_type(&[], false);
            let function = compiler.module.add_function(name, fn_type, None);
            let entry_basic_block = compiler.context.append_basic_block(function, name);
            println!("Can I build multiple builders?");
            let builder = compiler.context.create_builder();
            println!("No?");
            builder.position_at_end(entry_basic_block);
            let subcompiler = compiler.convert(&builder);
            let res = compile_expr(&subcompiler, expr);
            builder.build_return(Some(&res));
            println!("No???");
            let mut user_functions = compiler.user_functions.borrow_mut();
            user_functions.insert(name.to_string(), function);
        }
        _ => (),
    }
}

fn compile_print_statement<'b, 'c>(compiler: &'c Compiler<'b, 'c, Builder<'b>>, ast: &Statement)
where
    'b: 'c,
{
    match ast {
        Statement::Expression(ex) => {
            let hw_string_ptr = {
                compiler
                    .builder
                    .build_global_string_ptr("Compiled: %f\n", "hw")
            };
            let code = compile_expr(compiler, ex);
            println!("Compiling build_call");
            compiler.builder.build_call(
                *compiler.printf_function,
                &[hw_string_ptr.as_pointer_value().into(), code.into()],
                "call",
            );
        }
        _ => (),
    }
}

fn compile_expr<'b, 'c, 'd: 'c>(
    compiler: &'d Compiler<'b, 'c, Builder<'b>>,
    ast: &Expression,
) -> FloatValue<'b>
where
    'b: 'c,
{
    use ExprEnum::*;
    match &ast.expr {
        NumLiteral(val) => compiler.context.f64_type().const_float(*val),
        FnInvoke(name, _ex) => {
            let user_functions = compiler.user_functions.borrow();
            println!("user_functions: {:?}", *user_functions);
            let retval = compiler
                .builder
                .build_call(user_functions[*name], &[], name);
            let bv = retval.try_as_basic_value().unwrap_left();
            bv.into_float_value()
        }
        Add(lhs, rhs) => {
            let lhs = compile_expr(compiler, &lhs);
            let rhs = compile_expr(compiler, &rhs);
            compiler.builder.build_float_add(lhs, rhs, "name")
        }
        Sub(lhs, rhs) => {
            let lhs = compile_expr(compiler, &lhs);
            let rhs = compile_expr(compiler, &rhs);
            compiler.builder.build_float_sub(lhs, rhs, "sub")
        }
        Mul(lhs, rhs) => {
            let lhs = compile_expr(compiler, &lhs);
            let rhs = compile_expr(compiler, &rhs);
            compiler.builder.build_float_mul(lhs, rhs, "mul")
        }
        Div(lhs, rhs) => {
            let lhs = compile_expr(compiler, &lhs);
            let rhs = compile_expr(compiler, &rhs);
            compiler.builder.build_float_div(lhs, rhs, "div")
        }
    }
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    NumLiteral(f64),
    FnInvoke(&'src str, Vec<Expression<'src>>),
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

#[derive(Debug)]
enum Statement<'src> {
    Expression(Expression<'src>),
    FnDef {
        name: &'src str,
        args: Vec<&'src str>,
        expr: Expression<'src>,
    },
}

type Statements<'a> = Vec<Statement<'a>>;

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
    alt((num_literal, func_call, parens))(i)
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
    let (i, expr) = delimited(open_brace, num_expr, close_brace)(i)?;
    Ok((
        i,
        Statement::FnDef {
            name: *name,
            args: args.into_iter().map(|arg| *arg).collect(),
            expr,
        },
    ))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, res) = num_expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn statement(i: Span) -> IResult<Span, Statement> {
    alt((fn_def_statement, terminated(expr_statement, char(';'))))(i)
}

fn statements(i: Span) -> IResult<Span, Statements> {
    let (i, stmts) = many0(statement)(i)?;
    let (i, _) = opt(char(';'))(i)?;
    Ok((i, stmts))
}

fn statements_finish(i: Span) -> Result<Statements, nom::error::Error<Span>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}
