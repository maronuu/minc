/* Abstract Syntax Tree */

/* type expression:

   for now, we only have a primitive type (long),
   which is TypeExpr::Primitive("long") */

pub enum TypeExpr {
    Primitive(String)		// type name (always "long", for now)
}

/* variable declaration:

   long x; -> Decl{var_type=TypeExpr::Primitive("long"), name="x"}
   also (ab)used to represent a function parameter */

pub struct Decl {
    pub var_type : TypeExpr,
    pub name : String
}

/* expression */
pub enum Expr {
    IntLiteral(i64),            /* 1, 2, 3, ... */
    Id(String),                 /* x, y, z, ... */
    Op(String, Vec<Expr>),      /* -x, x - y, ... */
    Call(Box<Expr>, Vec<Expr>), /* f(1, 2, 3) */
    Paren(Box<Expr>)            /* (x + y) */
}

/* statement */

pub enum Stmt {
    Empty,                      /* ; */
    Continue,                   /* continue; */
    Break,                      /* break; */
    Return(Expr),               /* return e; */
    Expr(Expr),                 /* f(x); */
    Compound(Vec<Decl>, Vec<Stmt>), /* { int x; return x + 1; } */
    If(Expr, Box<Stmt>, Option<Box<Stmt>>), /* if (expr) stmt [else stmt] */
    While(Expr, Box<Stmt>)                  /* while (expr) stmt */
}

/* toplevel definition */
pub enum Def {
    /* function definition
     e.g., long f(long x, long y) { return x; } */
    Fun(String, Vec<Decl>, TypeExpr, Stmt)
}

/* program is just a list of definitions */
pub struct Program {
    pub defs : Vec<Def>
}

/* convert ast back to C string 

   ast_to_str_xxx converts an AST to a string 
   valid as a C program.

   it will not be used anywhere in your compiler, 
   but is given for illustrating how you walk
   the AST and what kind of C program each AST 
   is actually meant to represent.
*/

/* AST for type expression -> C string
   TypePrimitive{"long"} -> "long" */
#[allow(dead_code)]
pub fn ast_to_str_type(type_expr : &TypeExpr) -> String {
    match type_expr {
        TypeExpr::Primitive(name) => { format!("{}", name) }
    }
}

/* AST for function parameter -> C string
   Decl{TypeExpr::Primitive("long"), "x"} -> "long x" */
#[allow(dead_code)]
pub fn ast_to_str_param(param : &Decl) -> String {
    format!("{} {}", ast_to_str_type(&param.var_type), param.name)
}

/* AST for a variable declaration -> C string
   Decl{TypeExpr::Primitive("long"), "x"} -> "long x;" */
#[allow(dead_code)]
pub fn ast_to_str_decl(decl : &Decl) -> String {
    format!("{} {};", ast_to_str_type(&decl.var_type), decl.name)
}

/* AST for an expression -> C string */
#[allow(dead_code)]
pub fn ast_to_str_expr(expr : &Expr) -> String {
    match expr {
        /* Expr::IntLiteral{123} -> "123" */
        Expr::IntLiteral(val) => format!("{}", val),
        /* Expr::Id("x") -> "x" */
        Expr::Id(name) => format!("{}", name),
        /* Expr::Op("+" [Expr::Id("x"); Expr::IntLiteral("123")]) -> "x + 123" */
        Expr::Op(op, args) => {
            args.iter().map(ast_to_str_expr).collect::<Vec<String>>().join(&format!(" {} ", op))
        },
        /* Expr::Call(Expr::Id("f"), [Expr::Id("x"); Expr::IntLiteral("123")]) -> "f(x, 123)" */
        Expr::Call(fun, args) => {
            format!("{}({})",
                    ast_to_str_expr(fun),
                    args.iter().map(ast_to_str_expr).collect::<Vec<String>>().join(", "))
        },
        /* Expr::Paren(Expr::Op("+", [Expr::Id("x"); Expr::IntLiteral("123")])) -> "(x + 123)" */
        Expr::Paren(sub_expr) => {
            format!("({})", ast_to_str_expr(sub_expr))
        }
    }
}

/* AST for a statement -> C string */
#[allow(dead_code)]
pub fn ast_to_str_stmt(stmt : &Stmt) -> String {
    match stmt {
        Stmt::Empty => format!(";"),
        Stmt::Continue => format!("continue;"),
        Stmt::Break => format!("break;"),
        Stmt::Return(expr) => format!("return {};", ast_to_str_expr(expr)),
        Stmt::Expr(expr) => format!("{};", ast_to_str_expr(expr)),
        Stmt::Compound(decls, stmts) => {
            format!("{{\n{}\n{}\n}}", 
                    decls.iter().map(ast_to_str_decl).collect::<Vec<String>>().join("\n"),
                    stmts.iter().map(ast_to_str_stmt).collect::<Vec<String>>().join("\n"))
        },
        Stmt::If(cond, then_stmt, Some(else_stmt)) => {
            format!("if ({}) {} else {}",
                    ast_to_str_expr(cond),
                    ast_to_str_stmt(then_stmt),
                    ast_to_str_stmt(else_stmt))
        }
        Stmt::If(cond, then_stmt, None) => {
            format!("if ({}) {}",
                    ast_to_str_expr(cond),
                    ast_to_str_stmt(then_stmt))
        }
        Stmt::While(cond, body) => {
            format!("while ({}) {}",
                    ast_to_str_expr(cond),
                    ast_to_str_stmt(body))
        }
    }
}

/* AST for a definition -> C string

   DefFun("f", [Decl(TypeExpr::Primitive("long"), "x")], TypeExpr::Primitive("long"), 
                    StmtCompound([], [])) ->
       "long f(long x) {}"
*/
#[allow(dead_code)]
pub fn ast_to_str_def(def : &Def) -> String {
    match def {
        Def::Fun(name, params, return_type, body) => {
            format!("{} {}({}) {}",
                    ast_to_str_type(return_type),
                    name,
                    params.iter().map(ast_to_str_param).collect::<Vec<String>>().join(", "),
                    ast_to_str_stmt(body))
        }
    }
}

#[allow(dead_code)]
pub fn ast_to_str_program(program : &Program) -> String {
    program.defs.iter().map(ast_to_str_def).collect::<Vec<String>>().join("\n")
}

