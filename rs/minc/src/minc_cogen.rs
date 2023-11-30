// Author: Yutaro Oguri
use crate::minc_ast;
use std::collections::LinkedList;

// function argument registers for x86-64
const ARG_REG: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
// caller-saved registers for x86-64
const CALLEE_SAVED_REG: [&str; 5] = ["%rbx", "%r12", "%r13", "%r14", "%r15"];
// registers for x86-64 (excluding ones that has special meaning)
const REG: [&str; 10] = [
    "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9", "%rbx", "%r12", "%r13", "%r14",
];
// register for transit (used when moving data between memory and memory)
const TRAINSIT_REG: &str = "%r15";

// An entry of env
struct EnvEntry {
    name: String, // variable name
    loc: String,  // location of the variable
}
// Environment for definition
struct Env {
    list: LinkedList<EnvEntry>, // list of (var_name, loc)
    regs: Vec<String>,          // list of registers
    next_st: usize,             // next stack offset
    next_rg: usize,             // next register number
    next_label: usize,          // next label number
    n_extra_args: usize,        // number of extra arguments
    fn_name: String,            // name of the current function
}

impl Env {
    fn new() -> Env {
        Env {
            list: LinkedList::new(),
            regs: REG.iter().map(|s| s.to_string()).collect(),
            next_st: 1,
            next_rg: 0,
            next_label: 0,
            n_extra_args: 0,
            fn_name: "".to_string(),
        }
    }
    fn lookup(&self, name: &str) -> String {
        // lookup the location of a variable
        // if the variable is not found, panic!
        // if the variable is found, return the location
        // (e.g., -8(%rbp))
        for entry in &self.list {
            if entry.name == name {
                return entry.loc.clone();
            }
        }
        panic!("variable {} not found", name);
    }
    fn add_fn_arg(&mut self, name: &str) -> String {
        // add a function argument to the environment
        // return the location of the argument
        let loc = format!("{}(%rbp)", (self.n_extra_args + 2) * 8);
        self.list.push_back(EnvEntry {
            name: name.to_string(),
            loc: loc.clone(),
        });
        self.n_extra_args += 1;
        loc
    }
    fn add_var(&mut self, name: &str) -> String {
        // add a variable to the environment
        // return the location of the variable
        // (e.g., -8(%rbp))
        let loc = format!("-{}(%rbp)", self.next_st * 8);
        self.list.push_back(EnvEntry {
            name: name.to_string(),
            loc: loc.clone(),
        });
        self.next_st += 1;
        loc
    }
    fn new_label(&mut self) -> String {
        // generate a new label
        // return the label name
        let label = format!(".L{}{}", self.next_label, self.fn_name);
        self.next_label += 1;
        label
    }
    fn get_end_label(&self) -> String {
        // return the end label of the current function
        format!(".Lend{}", self.fn_name)
    }
    fn use_reg(&mut self) -> String {
        // use a register for temporary variable allocation unless all registers are used
        // return the register name
        // (e.g., %rax)
        if self.next_rg < self.regs.len() {
            let reg = self.regs[self.next_rg].clone();
            self.next_rg += 1;
            reg
        } else {
            panic!("out of registers")
        }
    }
    fn add_tmp(&mut self) -> String {
        // add a temporary variable to the environment (in stack or register)
        // return the location of the variable
        // (e.g., -8(%rbp))
        if self.next_rg < self.regs.len() {
            // use register for tmp allocation
            let reg = self.use_reg();
            if reg == "%rdx" {
                // skip rdx, since it is used for division
                self.add_tmp()
            } else {
                reg
            }
        } else {
            // use stack for tmp allocation
            let loc = format!("-{}(%rbp)", self.next_st * 8);
            self.next_st += 1;
            loc
        }
    }
    fn total_size(&self) -> i64 {
        // return the total size of all variables in the environment
        (self.next_st as i64 - 1) * 8
    }
}

fn gen_mov_inst(src: &str, dst: &str) -> String {
    // generate asm for mov instruction
    // when src and dst are memory reference, it issues 2 mov insts via transit register
    // otherwise, it issues 1 mov inst directly

    if src == dst {
        // if src and dst are the same, do nothing
        return "".to_string();
    }

    let mut asm = String::new();
    if src.starts_with("%") || dst.starts_with("%") {
        // if src or dst is a register, issue 1 mov inst
        asm.push_str(&format!("    {}    {}, {}\n", "movq", src, dst));
    } else {
        // if src and dst are memory reference, issue 2 mov insts via transit register
        asm.push_str(&format!("    {}    {}, {}\n", "movq", src, TRAINSIT_REG));
        asm.push_str(&format!("    {}    {}, {}\n", "movq", TRAINSIT_REG, dst));
    }
    asm
}

fn gen_movzb_inst(src: &str, dst: &str) -> String {
    // generate asm for movzb instruction
    // when dst is a memory reference, it issues 2 insts:
    //   movzbq src, transit_reg + movq transit_reg, dst
    // since movzbq does not support memory reference as dst
    // otherwise, it issues 1 movzbq inst directly

    if src == dst {
        // if src and dst are the same, do nothing
        return "".to_string();
    }
    let mut asm = String::new();
    if dst.starts_with("%") {
        // if dst is a register, issue 1 movzbq inst
        asm.push_str(&format!("    {}    {}, {}\n", "movzbq", src, dst));
    } else {
        // if dst is a memory reference, issue 2 mov insts via transit register
        asm.push_str(&format!("    {}    {}, {}\n", "movzbq", src, TRAINSIT_REG));
        asm.push_str(&format!("    {}    {}, {}\n", "movq", TRAINSIT_REG, dst));
    }
    asm
}

fn gen_cmp_inst(lhs: &str, rhs: &str) -> String {
    // generate asm for cmp instruction
    // when lhs and rhs are memory reference, it issues 2 insts:
    //  movq lhs, transit_reg + cmpq transit_reg, rhs
    // since cmpq does not support both operands as memory reference
    // otherwise, it issues 1 cmpq inst directly

    let mut asm = String::new();
    if lhs.starts_with("%") || rhs.starts_with("%") {
        // if lhs or rhs is a register, issue 1 cmpq inst
        asm.push_str(&format!("    {}    {}, {}\n", "cmpq", lhs, rhs));
    } else {
        // if lhs and rhs are memory reference, issue 2 mov insts via transit register
        asm.push_str(&format!("    {}    {}, {}\n", "movq", lhs, TRAINSIT_REG));
        asm.push_str(&format!("    {}    {}, {}\n", "cmpq", TRAINSIT_REG, rhs));
    }
    asm
}

fn gen_bin_inst(op: &str, lhs: &str, rhs: &str, dst: &str) -> String {
    // generate asm for binary instruction (e.g., addq, subq, imulq)
    // it issues 3 insts:
    //   - move lhs to transit register
    //   - exec operater with rhs and transit register. store the result in transit register
    //   - move transit register to dst

    let mut asm = String::new();
    asm.push_str(&format!("    {}    {}, {}\n", "movq", lhs, TRAINSIT_REG));
    asm.push_str(&format!("    {}    {}, {}\n", op, rhs, TRAINSIT_REG));
    asm.push_str(&format!("    {}    {}, {}\n", "movq", TRAINSIT_REG, dst));
    asm
}

fn gen_unary_op(op: &str, arg: &minc_ast::Expr, env: &mut Env, asm: &mut String) -> String {
    // generate asm for a unary operator

    // compile the argument expression, which returns the location of the result
    let src = gen_expr(arg, env, asm);

    let dst = match op {
        // NEGATE
        "-" => {
            // allocate temporary variable
            let dst = env.add_tmp();
            asm.push_str(&gen_mov_inst(&src, &dst));
            asm.push_str(&format!("    negq    {}\n", dst));
            dst
        }
        // NOT
        "!" => {
            // allocate temporary variable
            let dst = env.add_tmp();
            asm.push_str(&format!("    cmpq    $0, {}\n", src));
            asm.push_str(&format!("    sete    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        _ => {
            panic!("unknown unary op {}", op)
        }
    };
    // return the location of the result
    dst
}

fn gen_binary_op(
    op: &str,
    lhs: &minc_ast::Expr,
    rhs: &minc_ast::Expr,
    env: &mut Env,
    asm: &mut String,
) -> String {
    // generate asm for binary operator

    // compile the left-hand side and right-hand side expressions
    // which return the locations of the results for each expression
    let lhs_reg = gen_expr(lhs, env, asm);
    let rhs_reg = gen_expr(rhs, env, asm);

    let dst = match op {
        // ARITHMETIC OPERATORS
        "+" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_bin_inst("addq", &lhs_reg, &rhs_reg, dst.as_str()));
            dst
        }
        "-" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_bin_inst("subq", &lhs_reg, &rhs_reg, dst.as_str()));
            dst
        }
        "*" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_bin_inst("imulq", &lhs_reg, &rhs_reg, dst.as_str()));
            dst
        }
        "/" => {
            let dst = env.add_tmp();
            // rdx:rax / tmp -> rax
            asm.push_str(&format!("    movq    $0, %rdx\n"));
            asm.push_str(&format!("    movq    {}, %rax\n", lhs_reg));
            asm.push_str(&format!("    cqto\n"));
            asm.push_str(&format!("    idivq   {}\n", rhs_reg));
            asm.push_str(&format!("    movq    %rax, {}\n", dst));
            dst
        }
        "%" => {
            let dst = env.add_tmp();
            // rdx:rax % tmp -> rdx
            asm.push_str(&format!("    movq    $0, %rdx\n"));
            asm.push_str(&format!("    movq    {}, %rax\n", lhs_reg));
            asm.push_str(&format!("    cqto\n"));
            asm.push_str(&format!("    idivq   {}\n", rhs_reg));
            asm.push_str(&format!("    movq    %rdx, {}\n", dst));
            dst
        }
        // COMP OPERATORS
        "==" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_cmp_inst(&rhs_reg, &lhs_reg));
            asm.push_str(&format!("    sete    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        "!=" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_cmp_inst(&rhs_reg, &lhs_reg));
            asm.push_str(&format!("    setne    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        "<" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_cmp_inst(&rhs_reg, &lhs_reg));
            asm.push_str(&format!("    setl    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        "<=" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_cmp_inst(&rhs_reg, &lhs_reg));
            asm.push_str(&format!("    setle    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        ">" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_cmp_inst(&rhs_reg, &lhs_reg));
            asm.push_str(&format!("    setg    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        ">=" => {
            let dst = env.add_tmp();
            asm.push_str(&gen_cmp_inst(&rhs_reg, &lhs_reg));
            asm.push_str(&format!("    setge    %al\n"));
            asm.push_str(&gen_movzb_inst("%al", &dst));
            dst
        }
        // ASSIGNMENT OPERATOR
        "=" => {
            // move rhs to lhs
            let dst = lhs_reg.to_string();
            asm.push_str(&gen_mov_inst(&rhs_reg, &lhs_reg));
            dst
        }
        _ => {
            panic!("unknown binary op {}", op)
        }
    };
    // return the location of the result
    dst
}

fn gen_expr(expr: &minc_ast::Expr, env: &mut Env, asm: &mut String) -> String {
    // generate asm for an expression
    // matching the expression type
    match expr {
        minc_ast::Expr::IntLiteral(val) => {
            // allocate temporary variable
            let dst = env.add_tmp();
            // move the value to the temporary variable
            asm.push_str(&gen_mov_inst(&format!("${}", val), dst.as_str()));
            // return the location of the result
            dst.to_string()
        }
        minc_ast::Expr::Id(name) => {
            // lookup the location of the variable in the environment
            let loc = env.lookup(name);
            loc
        }
        minc_ast::Expr::Op(op, args) => match args.len() {
            // generate asm for unary or binary operator
            1 => {
                let dst = gen_unary_op(op, &args[0], env, asm);
                dst.to_string()
            }
            2 => {
                let dst = gen_binary_op(op, &args[0], &args[1], env, asm);
                dst.to_string()
            }
            _ => {
                panic!("op with more than 2 args is not implemented")
            }
        },
        minc_ast::Expr::Call(fun, args) => {
            // generate asm for a function call
            // asm for this expression
            let mut arg_asm = String::new();
            // pass arguments
            let n_args_with_reg = std::cmp::min(args.len(), ARG_REG.len());
            for i in 0..n_args_with_reg {
                let dst = gen_expr(&args[i], env, &mut arg_asm);
                arg_asm.push_str(&format!("    movq   {}, {}\n", dst, ARG_REG[i]));
            }
            // push arguments to stack (in reverse order)
            for i in (n_args_with_reg..args.len()).rev() {
                let dst = gen_expr(&args[i], env, &mut arg_asm);
                arg_asm.push_str(&format!("    pushq   {}\n", dst));
            }
            // call function
            let f_name = match fun.as_ref() {
                minc_ast::Expr::Id(name) => name,
                _ => panic!("function name must be an identifier"),
            };
            arg_asm.push_str(&format!("    call    {}\n", f_name));
            // get return value of function call (stored in %rax)
            let dst = env.add_tmp();
            arg_asm.push_str(&gen_mov_inst("%rax", dst.as_str()));
            asm.push_str(&arg_asm);
            // return the location of the result
            dst.to_string()
        }
        minc_ast::Expr::Paren(sub_expr) => {
            // generate asm for a parenthesized expression
            // call gen_expr recursively
            let dst = gen_expr(sub_expr, env, asm);
            dst.to_string()
        }
    }
}

fn gen_stmt_return(expr: &minc_ast::Expr, env: &mut Env) -> String {
    // generate asm for a return statement
    let mut asm = String::new();
    let dst = gen_expr(expr, env, &mut asm);
    // move the result to %rax (return register)
    asm.push_str(&format!("    movq    {}, %rax\n", dst));
    // jump to the end of the function (epilogue)
    asm.push_str(&format!("    jmp     {}\n", env.get_end_label()));
    asm
}

fn gen_stmt_expr(expr: &minc_ast::Expr, env: &mut Env) -> String {
    // generate asm for an expression statement
    let mut asm = String::new();
    gen_expr(expr, env, &mut asm);
    asm
}

fn gen_stmt_compound(
    decls: &Vec<minc_ast::Decl>,
    stmts: &Vec<minc_ast::Stmt>,
    env: &mut Env,
) -> String {
    // generate asm for a compound statement
    // add local variables to the environment
    for decl in decls {
        env.add_var(&decl.name);
    }
    // generate asm for each statement
    stmts
        .iter()
        .map(|stmt| gen_stmt(stmt, env))
        .collect::<Vec<String>>()
        .join("\n")
}

fn gen_stmt_if_else(
    cond: &minc_ast::Expr,
    then_stmt: &minc_ast::Stmt,
    else_stmt: &minc_ast::Stmt,
    env: &mut Env,
) -> String {
    // generate asm for if-else statement
    let mut asm = String::new();
    // compile the condition expression
    let dst = gen_expr(cond, env, &mut asm);
    // compare the result with 0
    asm.push_str(&format!("    cmpq    $0, {}\n", dst));
    let else_label = env.new_label();
    let end_label = env.new_label();
    // if the result is 0, jump to else branch
    asm.push_str(&format!("    je      {}\n", else_label));
    // compile then branch
    asm.push_str(&gen_stmt(then_stmt, env));
    // jump to the end of if-else
    asm.push_str(&format!("    jmp     {}\n", end_label));
    // else branch section
    asm.push_str(&format!("{}:\n", else_label));
    asm.push_str(&gen_stmt(else_stmt, env));
    // end of if-else
    asm.push_str(&format!("{}:\n", end_label));
    asm
}

fn gen_stmt_if(cond: &minc_ast::Expr, then_stmt: &minc_ast::Stmt, env: &mut Env) -> String {
    // generate asm for if statement
    let mut asm = String::new();
    // compile the condition expression
    let dst = gen_expr(cond, env, &mut asm);
    // compare the result with 0
    asm.push_str(&format!("    cmpq    $0, {}\n", dst));
    let end_label = env.new_label();
    // if the result is 0, jump to the end of if
    asm.push_str(&format!("    je      {}\n", end_label));
    // compile then branch
    asm.push_str(&gen_stmt(then_stmt, env));
    // end of if
    asm.push_str(&format!("{}:\n", end_label));
    asm
}

fn gen_stmt_while(cond: &minc_ast::Expr, body: &minc_ast::Stmt, env: &mut Env) -> String {
    // generate asm for while statement
    let mut asm = String::new();
    // labels
    let cond_label = env.new_label();
    let body_label = env.new_label();
    // goto cond
    asm.push_str(&format!("    jmp     {}\n", cond_label));
    // body stmt section
    asm.push_str(&format!("{}:\n", body_label));
    asm.push_str(&gen_stmt(body, env));
    // condition section
    asm.push_str(&format!("{}:\n", cond_label));
    let cond_dst = gen_expr(cond, env, &mut asm);
    // compare cond with 0
    asm.push_str(&format!("    cmpq    $0, {}\n", cond_dst));
    // jump to body if cond is true
    asm.push_str(&format!("    jne     {}\n", body_label));

    asm
}

fn gen_stmt(stmt: &minc_ast::Stmt, env: &mut Env) -> String {
    // generate asm for a statement
    match stmt {
        minc_ast::Stmt::Empty => String::new(),
        minc_ast::Stmt::Continue => {
            panic!("continue is not implemented");
        }
        minc_ast::Stmt::Break => {
            panic!("break is not implemented");
        }
        minc_ast::Stmt::Return(expr) => gen_stmt_return(expr, env),
        minc_ast::Stmt::Expr(expr) => gen_stmt_expr(expr, env),
        minc_ast::Stmt::Compound(decls, stmts) => gen_stmt_compound(decls, stmts, env),
        minc_ast::Stmt::If(cond, then_stmt, Some(else_stmt)) => {
            gen_stmt_if_else(cond, then_stmt, else_stmt, env)
        }
        minc_ast::Stmt::If(cond, then_stmt, None) => gen_stmt_if(cond, then_stmt, env),
        minc_ast::Stmt::While(cond, body) => gen_stmt_while(cond, body, env),
    }
}

fn gen_fun_prologue(fn_name: &str, params: &Vec<minc_ast::Decl>, env: &mut Env) -> String {
    // generate asm for function prologue
    let mut asm = String::new();
    // header
    asm.push_str(&format!(".globl {}\n", fn_name));
    // function label
    asm.push_str(&format!("{}:\n", fn_name));
    // save original stack pointer
    asm.push_str(&format!("    pushq %rbp\n"));
    asm.push_str(&format!("    movq %rsp, %rbp\n"));
    // save callee-saved registers
    for reg in CALLEE_SAVED_REG.iter() {
        asm.push_str(&format!("    pushq {}\n", reg));
    }
    // function args
    let mut arg_asm = String::new();
    for (i, param) in params.iter().enumerate() {
        if i < ARG_REG.len() {
            // use register (<= 6 args)
            env.use_reg();
            let loc = env.add_var(param.name.as_str());
            arg_asm.push_str(&format!("    movq {}, {}\n", ARG_REG[i], loc));
        } else {
            // extra args are stored on the stack
            env.add_fn_arg(param.name.as_str());
        }
    }
    // allocate stack for local variables
    asm.push_str(&format!("    subq ${}, %rsp\n", env.total_size()));
    asm.push_str(&arg_asm);

    asm
}

fn gen_fun_epilogue(env: &Env) -> String {
    // generate asm for function epilogue
    let mut asm = String::new();
    // label for the end of the function
    asm.push_str(&format!("{}:\n", env.get_end_label()));
    // restore callee-saved registers
    for reg in CALLEE_SAVED_REG.iter().rev() {
        asm.push_str(&format!("    popq {}\n", reg));
    }
    // restore stack pointer
    asm.push_str(&format!("    movq %rbp, %rsp\n"));
    asm.push_str(&format!("    popq %rbp\n"));
    // return
    asm.push_str(&format!("    retq\n"));
    asm
}

fn gen_def(def: &minc_ast::Def) -> String {
    // generate asm for a function definition
    let mut asm = String::new();
    // create an environment for the function (each function has its own environment)
    let mut env = Env::new();
    match def {
        minc_ast::Def::Fun(name, params, _, body) => {
            // generate end label
            env.fn_name = name.clone();
            // generate asm for function prologue, body, and epilogue
            let prologue_asm = gen_fun_prologue(name, params, &mut env);
            let body_asm = gen_stmt(body, &mut env);
            let epilogue_asm = gen_fun_epilogue(&env);

            // join all the asm
            asm.push_str(&prologue_asm);
            // allocate stack for local variables (actual total usage is already stored in env)
            asm.push_str(&format!("    subq ${}, %rsp\n", env.total_size()));
            asm.push_str(&body_asm);
            asm.push_str(&epilogue_asm);
        }
    }
    return asm;
}

pub fn ast_to_asm_program(_program: minc_ast::Program) -> String {
    // join all the assembly code for each def
    let asm = _program
        .defs
        .iter()
        .map(gen_def)
        .collect::<Vec<String>>()
        .join("\n");

    asm.to_string()
}
