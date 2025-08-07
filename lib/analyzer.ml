(* lib/analyzer.ml *)
open Ast 

(* 定义异常 *)
exception Semantic_error of string

let semantic_error msg = raise (Semantic_error msg)


(* 定义符号表条目 *)
type symbol_entry = {
  sym_name: string;
  sym_offset: int;
  sym_type: var_type;
}

(* 定义annotated AST *)
module AnnotatedAst = struct
  type expr' = 
    | AConstant of int
    | AVar of string * symbol_entry
    | AUnaOp of unary_op * expr
    | ABinOp of expr * binary_op * expr
    | ACall of string * (expr list)

  and expr = {a_loc: expr'; a_etype: var_type;}

  and stmt = 
    | AEmpty 
    | AVarDef of symbol_entry * expr
    | AAssign of symbol_entry * expr
    | AIf of expr * stmt * (stmt option)
    | AWhile of expr * stmt
    | AReturn of expr option
    | ABlock of stmt list
    | AExprStmt of expr
    | ABreak
    | AContinue
    
  type param = Ast.param

  type func_def = {
    a_ftype: func_type;
    a_fname: string;
    a_params: symbol_entry list;
    a_body: stmt;
    frame_size: int;    (* 在语义分析的过程中算出的栈帧大小 *)
  }

  type program = func_def list

end

(* 定义分析环境 *)
type analysis_env = {
  scopes: (string, symbol_entry) Hashtbl.t list; (* 符号表栈 *)
  funcs: (string, func_def) Hashtbl.t;           (* 全局函数定义表 *)
  mutable stack_offset: int ref;                     (* 当前可用的栈偏移量 *)
  current_func: func_def option;                 (* 用于检查 return 类型 *)
  loop_depth: int;                               (* 用于检查 break/continue *)
}

open AnnotatedAst

(* 语义分析的辅助函数 *)
let enter_scope env = {
  env with scopes = (Hashtbl.create 10) :: env.scopes
}

let declare_var env name vtype = 
  let current_scope = (List.hd env.scopes) in
    if Hashtbl.mem current_scope name then 
      semantic_error ("Variable '" ^ name ^ "' is already declared in this scope");
    
    (* 计算变量的偏移量 *)
    env.stack_offset:= !(env.stack_offset) - 4;
    let entry = {sym_name = name; sym_type = vtype; sym_offset = !(env.stack_offset) } in
    Hashtbl.add current_scope name entry;
    entry

let find_var env name = 
  let rec find_in_scopes = function
  | [] -> semantic_error ("Undefined variable: " ^ name)
  | current_scope::parent_scopes -> 
    try Hashtbl.find current_scope name 
    with Not_found -> find_in_scopes parent_scopes 
in find_in_scopes env.scopes


(* 语义分析的核心函数 *)

(* 表达式分析 *)
let rec analyze_expr (env: analysis_env) (expr: Ast.expr) :AnnotatedAst.expr = 
  match expr with
  | Constant n  -> {a_loc = AConstant  n; a_etype = TInt}
  | Var name  -> 
    let entry = find_var env name in 
    {a_loc = AVar (name,entry); a_etype = entry.sym_type}
  | UnaOp (op, e) ->
    let ae = analyze_expr env e in 
    (match op, ae.a_etype with
    | (Neg|Pos|Not), TInt -> {a_loc = AUnaOp (op, ae); a_etype = TInt}
    | (Neg|Pos|Not), TVoid -> semantic_error "a void expression can not be used to do Binop operation!"
    )
  | BinOp (e1, op , e2) ->
    let ae1 = analyze_expr env e1 in
      let ae2 = analyze_expr env e2 in
      if ae1.a_etype <> TInt || ae2.a_etype <> TInt 
        then semantic_error "Operands of binary operator must be type int!";
      {a_loc = ABinOp(ae1,op,ae2); a_etype = TInt}
  | Call (fname, args) ->
      try
        let func = Hashtbl.find env.funcs fname in
        if List.length args <> List.length func.params then
          semantic_error ("Function '" ^ fname ^ "' expects " ^ string_of_int (List.length func.params) ^ 
                          " arguments, but got " ^ string_of_int (List.length args));
        
        let a_args = List.map (analyze_expr env) args in
        List.iter2 (fun arg_expr (expected_type, _) ->
          if arg_expr.a_etype <> expected_type then
            semantic_error ("Type mismatch in argument for function '" ^ fname ^ "'")
        ) a_args func.params;

         match func.ftype with
        | TIntReturn -> { a_loc = ACall(fname, a_args); a_etype = TInt }
        | TVoidReturn -> { a_loc= ACall(fname, a_args); a_etype = TVoid }
      with Not_found -> semantic_error ("Undefined function: " ^ fname)

(* === 语句分析 === *)
and analyze_stmt env (stmt: Ast.stmt) : AnnotatedAst.stmt =
  match stmt with
  | Empty -> AEmpty
  | ExprStmt e -> AExprStmt (analyze_expr env e)
  | VarDef (vtype, name, init) ->
      let a_init = analyze_expr env init in
      if a_init.a_etype <> vtype then
        semantic_error ("Type mismatch in declaration of '" ^ name ^ "'");
      let entry = declare_var env name vtype in
      AVarDef (entry, a_init)
  | Assign (name, e) ->
      let entry = find_var env name in
      let ae = analyze_expr env e in
      if entry.sym_type <> ae.a_etype then
        semantic_error ("Type mismatch in assignment to '" ^ name ^ "'");
      AAssign (entry, ae)
  | If (cond, then_stmt, else_opt) ->
      let a_cond = analyze_expr env cond in
      if a_cond.a_etype <> TInt then
        semantic_error "If condition must be of type int";
      let a_then = analyze_stmt (enter_scope env) then_stmt in
      let a_else = Option.map (fun s -> analyze_stmt (enter_scope env) s) else_opt in
      AIf (a_cond, a_then, a_else)
  | While (cond, body) ->
      let a_cond = analyze_expr env cond in
      if a_cond.a_etype <> TInt then
        semantic_error "While condition must be of type int";
      let loop_env = { env with loop_depth = env.loop_depth + 1 } in
      let a_body = analyze_stmt (enter_scope loop_env) body in
      AWhile (a_cond, a_body)
  | Return expr_opt ->
      let current_func = Option.get env.current_func in
      (match current_func.ftype, expr_opt with
       | TIntReturn, Some e ->
           let ae = analyze_expr env e in
           if ae.a_etype <> TInt then
             semantic_error "Return type mismatch: expected int";
           AReturn (Some ae)
       | TIntReturn, None -> semantic_error "A non-void function must return a value"
       | TVoidReturn, Some _ -> semantic_error "A void function cannot return a value"
       | TVoidReturn, None -> AReturn None)
  | Break -> if env.loop_depth = 0 then semantic_error "Break used outside of loop"; ABreak
  | Continue -> if env.loop_depth = 0 then semantic_error "Continue used outside of loop"; AContinue
  | Block stmts ->
      let block_env = enter_scope env in
      let a_stmts = List.map (analyze_stmt block_env) stmts in
      ABlock a_stmts

let rec stmt_always_returns = function 
  | AReturn _ -> true
  | ABlock stmts -> List.exists stmt_always_returns stmts
  | AIf (_, then_stmt, Some else_stmt) ->
      stmt_always_returns then_stmt && stmt_always_returns else_stmt
  | _ -> false
  

let analyze_func_def env (func: Ast.func_def) : AnnotatedAst.func_def =
let func_env = {
  scopes = [Hashtbl.create 20];
  funcs = env.funcs;
  stack_offset = ref 0; (* 栈偏移量从每个函数的 0 开始重新计算 *)
  current_func = Some func;
  loop_depth = 0;
} in

(* debug for current func *)
print_endline ("# -----------Debug for" ^ func.fname ^ "------------");

(* 为RA和旧S0预留空间 *)
func_env.stack_offset := !(func_env.stack_offset) -8;
(* 为被调用者保存寄存器预留空间 *)
let open Reg in func_env.stack_offset := !(func_env.stack_offset) - (4 * List.length callee_saved_regs);

(* 声明参数 *)
let annotated_params = List.map (fun (ptype, pname) ->
  declare_var func_env pname ptype
) func.params in

(* 分析函数体 *)
let a_body = analyze_stmt (enter_scope func_env) func.body in
(* 检查返回值路径 *)
if func.ftype = TIntReturn && not (stmt_always_returns a_body) then
  semantic_error ("Function '" ^ func.fname ^ "' must return a value on all control paths");

print_endline ("# after the analyze_stmt:" ^ (string_of_int !(func_env.stack_offset))); 

(* 计算最终的栈帧大小 *)
let raw_size = abs !(func_env.stack_offset) in
let frame_size = ((raw_size + 15) / 16) * 16 in (* ABI规则: 16字节对齐 *)
print_endline ("# frame_size:" ^ (string_of_int frame_size));

{ a_ftype = func.ftype; a_fname = func.fname; a_params = annotated_params; 
  a_body = a_body; frame_size = frame_size }

let analyze_program (program: Ast.program) : AnnotatedAst.program =
let env = {
  scopes = [Hashtbl.create 50]; (* 全局作用域 *)
  funcs = Hashtbl.create 50;
  stack_offset = ref 0;
  current_func = None;
  loop_depth = 0;
} in
(* 第一遍：注册所有函数定义 *)
List.iter (fun func ->
  if Hashtbl.mem env.funcs func.fname then
    semantic_error ("Function '" ^ func.fname ^ "' is already defined");
  Hashtbl.add env.funcs func.fname func
) program;

(* 检查 main 函数的合法性 *)
(try
  let main_func = Hashtbl.find env.funcs "main" in
  if main_func.params <> [] then semantic_error "Main function cannot have parameters"
with Not_found ->
  semantic_error "Program must have a main function");

(* 第二遍：逐个分析每个函数 *)
List.map (analyze_func_def env) program

(* 主入口函数 *)
let analyze program =
  try
    Ok (analyze_program program)
  with Semantic_error msg ->
    Error msg