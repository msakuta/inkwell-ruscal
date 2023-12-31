use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{AnyValue, BasicValue, FloatValue, FunctionValue, PointerValue},
    FloatPredicate,
};

use crate::{ExprEnum, Expression, Statement};

pub(crate) type UserFunctions<'b> = Rc<RefCell<HashMap<String, FunctionValue<'b>>>>;

#[derive(Clone)]
pub(crate) enum Variable<'b> {
    Register(FloatValue<'b>),
    Stack(PointerValue<'b>),
}

pub(crate) type Variables<'b> = HashMap<String, Variable<'b>>;

#[derive(Clone)]
pub(crate) struct Compiler<'b, 'c, F, B> {
    context: &'b Context,
    module: &'c Module<'b>,
    function: &'c F,
    builder: &'c B,
    printf_function: &'c FunctionValue<'b>,
    putchar_function: &'c FunctionValue<'b>,
    user_functions: UserFunctions<'b>,
    variables: Variables<'b>,
}

impl<'b, 'c, F, B> Compiler<'b, 'c, F, B> {
    pub(crate) fn new(
        context: &'b Context,
        module: &'c Module<'b>,
        function: &'c F,
        builder: &'c B,
        printf_function: &'c FunctionValue<'b>,
        putchar_function: &'c FunctionValue<'b>,
        user_functions: UserFunctions<'b>,
    ) -> Self {
        Self {
            context,
            module,
            function,
            builder,
            printf_function,
            putchar_function,
            user_functions: user_functions.clone(),
            variables: HashMap::new(),
        }
    }
}

impl<'b, 'c> Compiler<'b, 'c, (), ()> {
    fn convert(
        &self,
        function: &'c FunctionValue<'b>,
        builder: &'c Builder<'b>,
        variables: Variables<'b>,
    ) -> Compiler<'b, 'c, FunctionValue<'b>, Builder<'b>> {
        Compiler::<FunctionValue, Builder> {
            context: self.context,
            module: self.module,
            function,
            builder,
            printf_function: self.printf_function,
            putchar_function: self.putchar_function,
            user_functions: self.user_functions.clone(),
            variables,
        }
    }
}

pub(crate) fn compile_print<'b, 'c>(compiler: &mut Compiler<'b, 'c, (), ()>) -> FunctionValue<'b> {
    let f64_type = compiler.context.f64_type();
    let print_fn_type = f64_type.fn_type(&[f64_type.into()], false);
    let function = compiler.module.add_function("print", print_fn_type, None);
    compiler
        .user_functions
        .borrow_mut()
        .insert("print".to_string(), function);
    let entry_basic_block = compiler.context.append_basic_block(function, "entry");
    let builder = compiler.context.create_builder();
    builder.position_at_end(entry_basic_block);
    let fmt_string_ptr = builder.build_global_string_ptr("%f\n", "fmt-float");
    let arg = function.get_first_param().unwrap().into_float_value();
    builder.build_call(
        *compiler.printf_function,
        &[fmt_string_ptr.as_pointer_value().into(), arg.into()],
        "call",
    );
    builder.build_return(None);
    function
}

pub(crate) fn compile_putc<'b, 'c>(compiler: &mut Compiler<'b, 'c, (), ()>) -> FunctionValue<'b> {
    let f64_type = compiler.context.f64_type();
    let print_fn_type = f64_type.fn_type(&[f64_type.into()], false);
    let function = compiler.module.add_function("putc", print_fn_type, None);
    compiler
        .user_functions
        .borrow_mut()
        .insert("putc".to_string(), function);
    let entry_basic_block = compiler.context.append_basic_block(function, "entry");
    let builder = compiler.context.create_builder();
    builder.position_at_end(entry_basic_block);
    let arg = function.get_first_param().unwrap().into_float_value();
    let arg = builder.build_float_to_signed_int(arg, compiler.context.i8_type(), "to-char");
    builder.build_call(*compiler.putchar_function, &[arg.into()], "call");
    builder.build_return(None);
    function
}

/// The first pass will compile the function definitions, while expression statements are skipped.
/// This is because (apparently) LLVM segfaults when you try to run multiple builders at once,
/// even in safe Rust.
pub(crate) fn compile_fn_statement<'b, 'c>(compiler: &mut Compiler<'b, 'c, (), ()>, ast: &Statement)
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
                        .map(|param| Variable::Register(param.into_float_value())),
                )
                .collect();
            let mut subcompiler = compiler.convert(&function, &builder, arg_vals);
            let mut res = None;
            for stmt in stmts.iter() {
                res = compile_expr_statement(&mut subcompiler, stmt);
            }
            builder.build_return(res.as_ref().map(|res| res as &dyn BasicValue));
            let mut user_functions = RefCell::borrow_mut(&compiler.user_functions);
            user_functions.insert(name.to_string(), function);
        }
        _ => (),
    }
}

/// The second pass compiles expressions, skipping function definitions.
pub(crate) fn compile_expr_statement<'b, 'c>(
    compiler: &mut Compiler<'b, 'c, FunctionValue<'b>, Builder<'b>>,
    ast: &Statement,
) -> Option<FloatValue<'b>>
where
    'b: 'c,
{
    match ast {
        Statement::Expression(ex) => {
            // let hw_string_ptr = {
            //     compiler
            //         .builder
            //         .build_global_string_ptr("Compiled: %f\n", "hw")
            // };
            let code = compile_expr(compiler, ex);
            // compiler.builder.build_call(
            //     *compiler.printf_function,
            //     &[hw_string_ptr.as_pointer_value().into(), code.into()],
            //     "call",
            // );
            Some(code)
        }
        Statement::VarDef(name, ex) => {
            let val = compile_expr(compiler, ex);
            let val_stack = compiler
                .builder
                .build_alloca(compiler.context.f64_type(), name);
            compiler.builder.build_store(val_stack, val);
            let varibales = &mut compiler.variables;
            varibales.insert(name.to_string(), Variable::Stack(val_stack));
            None
        }
        Statement::VarAssign(name, ex) => {
            let val = compile_expr(compiler, ex);
            let varibales = &mut compiler.variables;
            let Variable::Stack(var_stack) = varibales.get(*name).expect("Variable exists") else {
                panic!("Attempt to assign to a register variable; not permitted with LLVM SSA form")
            };
            compiler.builder.build_store(*var_stack, val);
            varibales.insert(name.to_string(), Variable::Register(val));
            None
        }
        Statement::For {
            loop_var,
            start,
            end,
            stmts,
        } => {
            let cond_bb = compiler.builder.get_insert_block().unwrap_or_else(|| {
                compiler
                    .context
                    .append_basic_block(*compiler.function, "cond_bb")
            });

            let loop_bb = compiler
                .context
                .append_basic_block(*compiler.function, "loop");
            let end_bb = compiler
                .context
                .append_basic_block(*compiler.function, "endloop");

            compiler.builder.position_at_end(cond_bb);
            let start_value = compile_expr(compiler, start);
            let end_value = compile_expr(compiler, end);
            let loop_var_ptr = compiler
                .builder
                .build_alloca(compiler.context.f64_type(), loop_var);
            compiler.builder.build_store(loop_var_ptr, start_value);

            compiler.builder.build_unconditional_branch(loop_bb);

            compiler.builder.position_at_end(loop_bb);
            let loop_var_reg = compiler
                .builder
                .build_load(loop_var_ptr, "loop_var_load")
                .as_any_value_enum()
                .into_float_value();
            compiler
                .variables
                .insert(loop_var.to_string(), Variable::Register(loop_var_reg));
            for stmt in stmts {
                compile_expr_statement(compiler, stmt);
            }
            let cmp_value = compiler.builder.build_float_compare(
                FloatPredicate::OLT,
                loop_var_reg,
                end_value,
                "loopcond",
            );
            let loop_var_incremented = compiler.builder.build_float_add(
                loop_var_reg,
                compiler.context.f64_type().const_float(1.).into(),
                "incr",
            );
            compiler
                .builder
                .build_store(loop_var_ptr, loop_var_incremented);
            compiler
                .builder
                .build_conditional_branch(cmp_value, loop_bb, end_bb);

            compiler.builder.position_at_end(end_bb);

            None
        }
        Statement::Return(ex) => {
            let val = compile_expr(compiler, ex);
            compiler.builder.build_return(Some(&val));
            None
        }
        _ => None,
    }
}

fn compile_expr<'b, 'c>(
    compiler: &mut Compiler<'b, 'c, FunctionValue<'b>, Builder<'b>>,
    ast: &Expression,
) -> FloatValue<'b>
where
    'b: 'c,
{
    use ExprEnum::*;
    match &ast.expr {
        NumLiteral(val) => compiler.context.f64_type().const_float(*val),
        Ident(id) => match compiler.variables[*id] {
            Variable::Register(val) => val,
            Variable::Stack(stk) => compiler
                .builder
                .build_load(stk, "stack-var")
                .as_any_value_enum()
                .into_float_value(),
        },
        FnInvoke(name, args) => {
            let args: Vec<_> = args
                .iter()
                .map(|arg| compile_expr(compiler, arg).into())
                .collect();
            let user_functions = compiler.user_functions.borrow();
            // println!("user_functions: {:?}", *user_functions);
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
        Lt(lhs, rhs) => {
            let lhs = compile_expr(compiler, &lhs);
            let rhs = compile_expr(compiler, &rhs);
            let cmp =
                compiler
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "comp.lt");
            compiler.builder.build_unsigned_int_to_float(
                cmp,
                compiler.context.f64_type(),
                "cmp.f64",
            )
        }
        Gt(lhs, rhs) => {
            let lhs = compile_expr(compiler, &lhs);
            let rhs = compile_expr(compiler, &rhs);
            let cmp =
                compiler
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "comp.gt");
            compiler.builder.build_unsigned_int_to_float(
                cmp,
                compiler.context.f64_type(),
                "cmp.f64",
            )
        }
        If(cond, t_case, f_case) => {
            // let string_ptr = {
            //     compiler
            //         .builder
            //         .build_global_string_ptr("Entering branch\n", "hw")
            // };
            // compiler.builder.build_call(
            //     *compiler.printf_function,
            //     &[string_ptr.as_pointer_value().into()],
            //     "call",
            // );

            let cond_bb = compiler.builder.get_insert_block().unwrap_or_else(|| {
                compiler
                    .context
                    .append_basic_block(*compiler.function, "cond_bb")
            });

            compiler.builder.position_at_end(cond_bb);
            let cond_value = compile_expr(compiler, cond);
            let cond = compiler.builder.build_float_compare(
                FloatPredicate::ONE,
                cond_value,
                compiler.context.f64_type().const_zero(),
                "cond-cmp",
            );

            let then_bb = compiler
                .context
                .append_basic_block(*compiler.function, "then_bb");
            compiler.builder.position_at_end(then_bb);
            let then_block = t_case
                .iter()
                .fold(None, |_, cur| compile_expr_statement(compiler, cur))
                .unwrap_or_else(|| compiler.context.f64_type().const_float(0.));

            let else_bb = compiler
                .context
                .append_basic_block(*compiler.function, "else_bb");
            compiler.builder.position_at_end(else_bb);
            let else_block = f_case
                .as_ref()
                .and_then(|f_case| {
                    f_case
                        .iter()
                        .fold(None, |_, cur| compile_expr_statement(compiler, cur))
                })
                .unwrap_or_else(|| compiler.context.f64_type().const_float(0.));
            let else_bb_after = compiler.builder.get_insert_block().unwrap();

            compiler.builder.position_at_end(cond_bb);
            compiler
                .builder
                .build_conditional_branch(cond, then_bb, else_bb);

            let end_bb = compiler
                .context
                .append_basic_block(*compiler.function, "end");
            compiler.builder.position_at_end(then_bb);
            compiler.builder.build_unconditional_branch(end_bb);

            compiler.builder.position_at_end(else_bb_after);
            compiler.builder.build_unconditional_branch(end_bb);

            compiler.builder.position_at_end(end_bb);
            let phi_value = compiler
                .builder
                .build_phi(compiler.context.f64_type(), "phi");
            phi_value.add_incoming(&[(&then_block, then_bb), (&else_block, else_bb_after)]);
            let res = phi_value.as_any_value_enum().into_float_value();
            res
        }
    }
}
