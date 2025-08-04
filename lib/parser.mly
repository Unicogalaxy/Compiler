/* parser.mly - ocamlyacc strict compatible version */
%{
  open Ast
%}

/* TOKEN DECLARATIONS */
%token <int> NUMBER
%token <string> ID
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token ASSIGN
%token OR AND
%token EQ NEQ
%token LT LTE GT GTE
%token PLUS MINUS TIMES DIV MOD NOT
%token EOF

/* PRECEDENCE AND ASSOCIATIVITY */
%right ELSE
%right ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT UMINUS UPLUS

/* START SYMBOL & TYPE ANNOTATIONS */

/* CORRECTED: Removed type from %start */
%start comp_unit

/* CORRECTED: Added type declaration for comp_unit here */
%type <Ast.program> comp_unit
%type <Ast.func_def list> func_def_list
%type <Ast.func_def> func_def
%type <Ast.func_type> func_type
%type <Ast.param list> param_list_optional
%type <Ast.param list> param_list_nonempty
%type <Ast.stmt> stmt
%type <Ast.stmt> block
%type <Ast.stmt list> stmt_list
%type <Ast.expr> expr
%type <Ast.expr list> arg_list_optional
%type <Ast.expr list> arg_list_nonempty

%%

/* GRAMMAR RULES - USING $1, $2, etc. */

comp_unit:
  func_def_list EOF { List.rev $1 }
;

func_def_list:
  | func_def
    { [$1] }
  | func_def_list func_def
    { $2 :: $1 }
;

func_type:
  | INT     { TIntReturn }
  | VOID    { TVoidReturn }
;

func_def:
  func_type ID LPAREN param_list_optional RPAREN block
    { { ftype = $1; fname = $2; params = $4; body = $6 } }
;

param_list_optional:
  /* empty */
    { [] }
  | param_list_nonempty
    { List.rev $1 }
;

param_list_nonempty:
  param
    { [$1] }
  | param_list_nonempty COMMA param
    { $3 :: $1 }
;

param:
  INT ID
    { (TInt, $2) }
;

block:
  LBRACE stmt_list RBRACE
    { Block (List.rev $2) }
;

stmt_list:
  /* empty */
    { [] }
  | stmt_list stmt
    { $2 :: $1 }
;

stmt:
  SEMI
    { Empty }
  | expr SEMI
    { ExprStmt $1 }
  | INT ID ASSIGN expr SEMI
    { VarDef(TInt, $2, $4) }
  | ID ASSIGN expr SEMI
    { Assign($1, $3) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
    { If($3, $5, Some $7) }
  | IF LPAREN expr RPAREN stmt
    { If($3, $5, None) }
  | WHILE LPAREN expr RPAREN stmt
    { While($3, $5) }
  | BREAK SEMI
    { Break }
  | CONTINUE SEMI
    { Continue }
  | RETURN SEMI
    { Return None }
  | RETURN expr SEMI
    { Return (Some $2) }
  | block
    { $1 }
;

expr:
  NUMBER
    { Constant $1 }
  | ID
    { Var $1 }
  | LPAREN expr RPAREN
    { $2 }
  | ID LPAREN arg_list_optional RPAREN
    { Call($1, $3) }
  | MINUS expr %prec UMINUS
    { UnaOp(Neg, $2) }
  | PLUS expr %prec UPLUS       
    { UnaOp(Pos, $2) }
  | NOT expr
    { UnaOp(Not, $2) }
  | expr OR expr
    { BinOp($1, Or, $3) }
  | expr AND expr
    { BinOp($1, And, $3) }
  | expr EQ expr
    { BinOp($1, Eq, $3) }
  | expr NEQ expr
    { BinOp($1, Neq, $3) }
  | expr LT expr
    { BinOp($1, Lt, $3) }
  | expr LTE expr
    { BinOp($1, Lte, $3) }
  | expr GT expr
    { BinOp($1, Gt, $3) }
  | expr GTE expr
    { BinOp($1, Gte, $3) }
  | expr PLUS expr
    { BinOp($1, Add, $3) }
  | expr MINUS expr
    { BinOp($1, Sub, $3) }
  | expr TIMES expr
    { BinOp($1, Mul, $3) }
  | expr DIV expr
    { BinOp($1, Div, $3) }
  | expr MOD expr
    { BinOp($1, Mod, $3) }
;

arg_list_optional:
  /* empty */
    { [] }
  | arg_list_nonempty
    { List.rev $1 }
;

arg_list_nonempty:
  expr
    { [$1] }
  | arg_list_nonempty COMMA expr
    { $3 :: $1 }
;
%%