mod parser;

use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc};

use nom_locate::LocatedSpan;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FloatValue, FunctionValue},
    OptimizationLevel,
};

use parser::statements_finish;

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
    variables: HashMap<String, FloatValue<'b>>,
}

impl<'b, 'c> Compiler<'b, 'c, ()> {
    fn convert(
        &self,
        builder: &'c Builder<'b>,
        variables: HashMap<String, FloatValue<'b>>,
    ) -> Compiler<'b, 'c, Builder<'b>> {
        Compiler::<Builder> {
            context: self.context,
            module: self.module,
            builder,
            printf_function: self.printf_function,
            user_functions: self.user_functions.clone(),
            variables,
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
        variables: HashMap::new(),
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
        variables: HashMap::new(),
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
        Statement::FnDef { name, args, expr } => {
            let args_ty: Vec<_> = args
                .iter()
                .map(|_| compiler.context.f64_type().into())
                .collect();
            let fn_type = compiler.context.f64_type().fn_type(&args_ty, false);
            let function = compiler.module.add_function(name, fn_type, None);
            let entry_basic_block = compiler.context.append_basic_block(function, name);
            let builder = compiler.context.create_builder();
            builder.position_at_end(entry_basic_block);
            let arg_vals: HashMap<_, _> = args
                .iter()
                .map(|arg| arg.to_string())
                .zip(
                    function
                        .get_param_iter()
                        .map(|param| param.into_float_value()),
                )
                .collect();
            let subcompiler = compiler.convert(&builder, arg_vals);
            let res = compile_expr(&subcompiler, expr);
            builder.build_return(Some(&res));
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
        Ident(id) => compiler.variables[*id],
        FnInvoke(name, args) => {
            let args: Vec<_> = args
                .iter()
                .map(|arg| compile_expr(compiler, arg).into())
                .collect();
            let user_functions = compiler.user_functions.borrow();
            println!("user_functions: {:?}", *user_functions);
            let retval = compiler
                .builder
                .build_call(user_functions[*name], &args, name);
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
    Ident(&'src str),
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
