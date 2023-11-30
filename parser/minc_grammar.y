@@grammar::minC

# the whole thing is a program, followed by EOF
start =
  program $ ;

# program is zero or more definitions
program =
  {definition}*
  ;

# definition is a function definition (no global vars or type definitions)
definition =
  fun_definition
  ;

# function definition is like "long f(long x, long y) { ... }"
fun_definition =
  type_expr identifier "(" parameter_list ")" compound_stmt
  ;

# parameter_list is a comma-separated list of parameter (like "long x")
parameter_list =
  ","%{parameter}*
  ;

# a parameter is a type followed by a variable name (identifier)
parameter =
  type_expr identifier
  ;

# only type actually supported is long
type_expr =
  "long"
  ;

# statements
stmt =
  | ";"                         # empty (no-op)
  | "continue" ";"              # continue
  | "break" ";"                 # break
  | "return" expr ";"           # return
  | compound_stmt               # { ... }
  | if_stmt                     # if
  | while_stmt                  # while
  | expr ";"                    # expression (e.g., f(x))
  ;

compound_stmt =
  "{" {var_decl}* {stmt}* "}"
  ;

var_decl =
  type_expr identifier ";"
  ;

if_stmt =
  "if" "(" expr ")" stmt [ "else" stmt ]
  ;

while_stmt =
  "while" "(" expr ")" stmt
  ;

# definition of expression reflects operator precedence
# and left-/right-associativity
# the entire expression
# the operator having the weakest precedence is =
expr =
  | equality_expr "=" expr
  | equality_expr
  ;

# the operators having the weakest precedence in equality_expr are
# == and !=
equality_expr =
  | equality_expr ("=="|"!=") cmp_expr
  | cmp_expr
  ;

# the operators having the weakest precedence in cmp_expr are
# <=, >=, < and >
cmp_expr =
  | cmp_expr ("<="|">="|"<"|">") additive_expr
  | additive_expr
  ;

# the operators having the weakest precedence in additive_expr are
# + and -
additive_expr =
  | additive_expr ("+"|"-") multiplicative_expr
  | multiplicative_expr
  ;

# the operators having the weakest precedence in multiplicative_expr are
# *, / and %
multiplicative_expr =
  | multiplicative_expr ("*"|"/"|"%") unary_expr
  | unary_expr
  ;

unary_expr =
  | number                        # 123
  | identifier "(" arg_list ")"   # f(x, y)
  | identifier                    # x
  | "(" expr ")"                  # (x * y + z)
  | ("+"|"-"|"!"|"~") unary_expr  # -x
  ;

# comma-separated list of zero or more expressions
arg_list =
  ","%{expr}*
  ;

number = /\d+/ ;
identifier = /[A-Za-z_][A-Za-z_0-9]*/ ;
