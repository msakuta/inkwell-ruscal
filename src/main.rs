mod compiler;
mod parser;

use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc};

use nom_locate::LocatedSpan;

use inkwell::{builder::Builder, context::Context, values::FunctionValue, OptimizationLevel};

use crate::{
    compiler::{
        compile_expr_statement, compile_fn_statement, compile_print, compile_putc, Compiler,
    },
    parser::statements_finish,
};

fn main() -> std::io::Result<()> {
    let mut executable = None;
    let mut show_ast = false;
    let mut emit_llvm = false;
    let mut src_file = None;

    for arg in std::env::args() {
        if executable.is_none() {
            executable = Some(arg.clone());
            continue;
        }
        match &arg as &str {
            "-a" => show_ast = true,
            "-e" => emit_llvm = true,
            "-h" => {
                eprintln!(
                    r#"usage: {} [options] <source.rscl>

Options:
    -a    Show AST
    -e    Emit LLVM IR to "out.ll"
    -h    Show this message

<source.rscl> can be a file name or "-" to get from standard input.
"#,
                    executable
                        .as_ref()
                        .map(AsRef::as_ref)
                        .unwrap_or("inkwell-ruscal")
                );
                return Ok(());
            }
            src => src_file = Some(src.to_string()),
        }
    }

    let src = if let Some("-") = src_file.as_ref().map(AsRef::as_ref) {
        let mut buf = String::new();
        if !std::io::stdin().read_to_string(&mut buf).is_ok() {
            panic!("Failed to read from stdin");
        }
        buf
    } else if let Some(src_file) = src_file {
        std::fs::read_to_string(src_file)?
    } else {
        eprintln!(
            "no input: see {} -h for help",
            executable
                .as_ref()
                .map(AsRef::as_ref)
                .unwrap_or("inkwell-ruscal")
        );
        return Ok(());
    };

    build_program(&src, show_ast, emit_llvm);
    Ok(())
}

fn build_program(source: &str, show_ast: bool, emit_llvm: bool) {
    let context = Context::create();
    let i32_type = context.i32_type();
    let i8_type = context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);

    // Module
    let module = context.create_module("main");

    // Function
    let printf_fn_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    let printf_function = module.add_function("printf", printf_fn_type, None);
    let putchar_fn_type = i32_type.fn_type(&[i8_type.into()], false);
    let putchar_function = module.add_function("putchar", putchar_fn_type, None);
    let main_fn_type = i32_type.fn_type(&[], false);
    let main_function = module.add_function("main", main_fn_type, None);

    let ast = match statements_finish(Span::new(source)) {
        Ok(val) => val,
        Err(e) => {
            eprintln!("Parse error: {e}");
            return;
        }
    };

    if show_ast {
        println!("AST: {ast:?}");
    }

    let user_functions = HashMap::new();
    let user_functions = Rc::new(RefCell::new(user_functions));

    let mut compiler = Compiler::<(), ()>::new(
        &context,
        &module,
        &(),
        &(),
        &printf_function,
        &putchar_function,
        user_functions.clone(),
    );

    compile_print(&mut compiler);
    compile_putc(&mut compiler);

    for stmt in &ast {
        compile_fn_statement(&mut compiler, stmt);
    }

    // Block
    let entry_basic_block = context.append_basic_block(main_function, "entry");

    // Instruction(Builder)
    let builder = context.create_builder();
    builder.position_at_end(entry_basic_block);

    let mut compiler = Compiler::<FunctionValue, Builder>::new(
        &context,
        &module,
        &main_function,
        &builder,
        &printf_function,
        &putchar_function,
        user_functions.clone(),
    );

    for stmt in &ast {
        compile_expr_statement(&mut compiler, stmt);
    }

    builder.build_return(Some(&i32_type.const_int(0, false)));

    if emit_llvm {
        module.print_to_file("out.ll").unwrap();
    }

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

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ExprEnum<'src> {
    NumLiteral(f64),
    Ident(&'src str),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Expression<'a> {
    pub(crate) expr: ExprEnum<'a>,
    pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
        Self { expr, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: &'src str,
        args: Vec<&'src str>,
        stmts: Statements<'src>,
    },
    Return(Expression<'src>),
}

type Statements<'a> = Vec<Statement<'a>>;
