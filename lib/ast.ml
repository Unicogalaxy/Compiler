(* 二元运算符 *)
type binary_op = 
| Add 
| Sub
| Mul 
| Div
| Mod 
| Lt  (* < *)
| Lte  (* <= *)
| Gt   (* > *)
| Gte  (* >= *)
| Eq   (* == *)
| Neq  (* != *)
| And  (* && *)
| Or   (* || *)

(*一元运算符*)
type unary_op = 
| Neg (*- 取负*)
| Not (*! 非*)
| Pos (*+ 取正*)

(* expr *)
type expr = 
| Constant of int 
| Var of string
| UnaOp of unary_op * expr
| BinOp of expr * binary_op * expr
| Call of string * (expr list)

(*返回类型*)
type func_type =
  | TIntReturn
  | TVoidReturn

(*变量类型*)
type var_type = 
| TInt

and stmt = 
  | Empty
  | VarDef of var_type * string * expr
  | Assign of string * expr
  | If of expr * stmt * (stmt option) 
  | While of expr * stmt
  | Return of expr option
  | Block of stmt list
  | ExprStmt of expr
  | Break
  | Continue 
  
type param = var_type * string

(* 函数定义 (Function Definition) *)
type func_def = {
  ftype:  func_type;      (* 返回类型 *)
  fname:  string;         (* 函数名 *)
  params: param list;      (* 形参列表 *)
  body:   stmt;           (* 函数体 (必须是一个 Block) *)
}

(* CompUnit *)
type program = func_def list

