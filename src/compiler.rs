use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, FloatValue, FunctionValue},
};

use crate::{ExprEnum, Expression, Statement};

pub(crate) type UserFunctions<'b> = Rc<RefCell<HashMap<String, FunctionValue<'b>>>>;
pub(crate) type Variables<'b> = HashMap<String, FloatValue<'b>>;

#[derive(Clone)]
pub(crate) struct Compiler<'b, 'c, B> {
    context: &'b Context,
    module: &'c Module<'b>,
    builder: &'c B,
    printf_function: &'c FunctionValue<'b>,
    user_functions: UserFunctions<'b>,
    variables: Variables<'b>,
}

impl<'b, 'c, B> Compiler<'b, 'c, B> {
    pub(crate) fn new(
        context: &'b Context,
        module: &'c Module<'b>,
        builder: &'c B,
        printf_function: &'c FunctionValue<'b>,
        user_functions: UserFunctions<'b>,
    ) -> Self {
        Self {
            context: &context,
            module: &module,
            builder: &builder,
            printf_function: &printf_function,
            user_functions: user_functions.clone(),
            variables: HashMap::new(),
        }
    }
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

/// The first pass will compile the function definitions, while expression statements are skipped.
/// This is because (apparently) LLVM segfaults when you try to run multiple builders at once,
/// even in safe Rust.
pub(crate) fn compile_fn_statement<'b, 'c>(compiler: &mut Compiler<'b, 'c, ()>, ast: &Statement)
where
    'b: 'c,
{
    match ast {
        Statement::FnDef { name, args, stmts } => {
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
            let mut subcompiler = compiler.convert(&builder, arg_vals);
            let mut res = None;
            for stmt in stmts.iter() {
                res = compile_print_statement(&mut subcompiler, stmt);
            }
            builder.build_return(res.as_ref().map(|res| res as &dyn BasicValue));
            let mut user_functions = RefCell::borrow_mut(&compiler.user_functions);
            user_functions.insert(name.to_string(), function);
        }
        _ => (),
    }
}

/// The second pass compiles expressions, skipping function definitions.
pub(crate) fn compile_print_statement<'b, 'c>(
    compiler: &mut Compiler<'b, 'c, Builder<'b>>,
    ast: &Statement,
) -> Option<FloatValue<'b>>
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
            compiler.builder.build_call(
                *compiler.printf_function,
                &[hw_string_ptr.as_pointer_value().into(), code.into()],
                "call",
            );
            Some(code)
        }
        Statement::VarDef(name, ex) => {
            let val = compile_expr(compiler, ex);
            let varibales = &mut compiler.variables;
            varibales.insert(name.to_string(), val);
            None
        }
        _ => None,
    }
}

fn compile_expr<'b, 'c>(
    compiler: &Compiler<'b, 'c, Builder<'b>>,
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
