(* lib/typechecker.ml *)
open Ast

exception Type_error of string

let type_error msg = raise (Type_error msg)

(* 类型环境 *)
type type_env = {
  scopes: (string, var_type) Hashtbl.t list;
  funcs: (string, func_def) Hashtbl.t;
  current_func: func_def option;
}


(* === 作用域管理函数 === *)
let enter_scope env =
  { env with scopes = (Hashtbl.create 10) :: env.scopes }

let exit_scope env =
  match env.scopes with
  | _ :: parent_scopes -> { env with scopes = parent_scopes }
  | [] -> failwith "Compiler error: Cannot exit the global scope"

let declare_var env name vtype =
  match env.scopes with
  | current_scope :: _ ->
      if Hashtbl.mem current_scope name then
        type_error ("Variable '" ^ name ^ "' is already declared in this scope")
      else
        Hashtbl.add current_scope name vtype
  | [] -> failwith "Compiler error: No scope available for variable declaration"

let get_var_type env name =
  let rec find_in_scopes = function
    | [] -> type_error ("Undefined variable: " ^ name)
    | current_scope :: parent_scopes ->
        try Hashtbl.find current_scope name
        with Not_found -> find_in_scopes parent_scopes
  in
  find_in_scopes env.scopes

(* === 类型检查核心函数 === *)

let rec check_expr env expr : var_type =
  match expr with
  | Constant _ -> TInt
  | Var name -> get_var_type env name
  | UnaOp (op, e) ->
      let t = check_expr env e in
      (match op, t with
       | (Neg | Not), TInt -> TInt
       (* 'Pos' is a valid unary operator from your AST but results in no type change *)
       | Pos, TInt -> TInt)
  | BinOp (e1, _, e2) ->
      let t1 = check_expr env e1 in
      let t2 = check_expr env e2 in
      if t1 <> TInt || t2 <> TInt then
        type_error "Operands of binary operator must be of type int";
      TInt
  | Call (fname, args) ->
      try
        let func = Hashtbl.find env.funcs fname in
        if List.length args <> List.length func.params then
          type_error ("Function '" ^ fname ^ "' expects " ^
                      string_of_int (List.length func.params) ^ " arguments, but got " ^
                      string_of_int (List.length args));
        (* CORRECTED: The lambda now correctly handles an expr and a param tuple *)
        List.iter2 (fun arg (expected_type, _) ->
          let arg_type = check_expr env arg in
          if arg_type <> expected_type then
            type_error ("Type mismatch in argument for function '" ^ fname ^ "'")
        ) args func.params;
        match func.ftype with
        | TIntReturn -> TInt
        | TVoidReturn -> type_error "Cannot use a void function's result as a value"
      with Not_found -> type_error ("Undefined function: " ^ fname)

let rec stmt_always_returns = function
  | Return _ -> true
  | Block stmts -> List.exists stmt_always_returns stmts
  | If (_, then_stmt, Some else_stmt) ->
      stmt_always_returns then_stmt && stmt_always_returns else_stmt
  | _ -> false

let rec check_stmt env stmt =
  match stmt with
  | Empty -> ()
  | ExprStmt e -> ignore (check_expr env e)
  | VarDef (vtype, name, init) ->
      let init_type = check_expr env init in
      if init_type <> vtype then
        type_error ("Type mismatch in declaration of '" ^ name ^ "'");
      declare_var env name vtype
  | Assign (name, e) ->
      let var_type = get_var_type env name in
      let expr_type = check_expr env e in
      if var_type <> expr_type then
        type_error ("Type mismatch in assignment to '" ^ name ^ "'")
  | If (cond, then_stmt, else_opt) ->
      if check_expr env cond <> TInt then
        type_error "If condition must be of type int";
      check_stmt (enter_scope env) then_stmt;
      (match else_opt with
       | Some else_stmt -> check_stmt (enter_scope env) else_stmt
       | None -> ())
  | While (cond, body) ->
      if check_expr env cond <> TInt then
        type_error "While condition must be of type int";
      check_stmt (enter_scope env) body
  | Return expr_opt ->
      let current_func =
        match env.current_func with
        | Some f -> f
        | None -> failwith "Compiler error: return statement outside of a function"
      in
      (match current_func.ftype, expr_opt with
       | TIntReturn, Some e ->
           if check_expr env e <> TInt then
             type_error "Return type mismatch: expected int"
       | TIntReturn, None -> type_error "A non-void function must return a value"
       | TVoidReturn, Some _ -> type_error "A void function cannot return a value"
       | TVoidReturn, None -> ())
  | Break -> ()
  | Continue -> ()
  | Block stmts ->
      let block_env = enter_scope env in
      List.iter (check_stmt block_env) stmts

let check_func_def env func =
  let func_env = {
    scopes = [Hashtbl.create 10];
    funcs = env.funcs;
    current_func = Some func;
  } in
  List.iter (fun (ptype, pname) ->
    Hashtbl.add (List.hd func_env.scopes) pname ptype
  ) func.params;
  check_stmt func_env func.body;
  if func.ftype = TIntReturn && not (stmt_always_returns func.body) then
    type_error ("Function '" ^ func.fname ^ "' must return a value on all control paths")

let type_check (program: program) =
  let env = {
    scopes = [Hashtbl.create 50];
    funcs = Hashtbl.create 50;
    current_func = None;
  } in
  List.iter (fun func ->
    if Hashtbl.mem env.funcs func.fname then
      type_error ("Function '" ^ func.fname ^ "' is already defined");
    Hashtbl.add env.funcs func.fname func
  ) program;
  List.iter (check_func_def env) program;
  try
    let main_func = Hashtbl.find env.funcs "main" in
    if main_func.ftype <> TIntReturn then
      type_error "Main function must return int";
    if main_func.params <> [] then
      type_error "Main function cannot have parameters"
  with Not_found ->
    type_error "Program must have a main function"

let type_check_program program =
  try
    type_check program
  with Type_error msg ->
    Printf.eprintf "Type error: %s\n" msg;
    exit 1