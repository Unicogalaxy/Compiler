(* main.ml *)
(* The main entry point for the ToyC compiler. *)
(* For this stage, it parses a source file and prints the resulting AST. *)

open Parse_project
open Ast

(* A module dedicated to printing the AST in a readable format. *)
module Ast_printer = struct
  let string_of_op = function
    | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
    | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">="
    | And -> "&&" | Or -> "||"

  let string_of_uop = function
    | Neg -> "-" | Not -> "!" | Pos -> "+"

  let rec print_expr indent expr =
    let p s = print_endline (indent ^ s) in
    match expr with
    | Constant n -> p ("Constant(" ^ string_of_int n ^ ")")
    | Var s -> p ("Var(" ^ s ^ ")")
    | UnaOp (op, e) ->
        p ("UnOp(" ^ string_of_uop op ^ ")");
        print_expr (indent ^ "  ") e
    | BinOp (e1, op, e2) ->
        p ("BinOp(" ^ string_of_op op ^ ")");
        print_expr (indent ^ "  ") e1;
        print_expr (indent ^ "  ") e2
    | Call (fname, args) ->
        p ("Call(" ^ fname ^ ")");
        List.iter (print_expr (indent ^ "  ")) args

  let rec print_stmt indent stmt =
    let p s = print_endline (indent ^ s) in
    let new_indent = indent ^ "  " in
    match stmt with
    | Empty -> p "Empty;"
    | ExprStmt e ->
        p "ExprStmt";
        print_expr new_indent e
    | VarDef (_, name, init) ->
        p ("VarDecl(int " ^ name ^ ")");
        print_expr new_indent init
    | Assign (name, e) ->
        p ("Assign(" ^ name ^ ")");
        print_expr new_indent e
    | If (cond, then_stmt, else_opt) ->
        p "If";
        print_expr new_indent cond;
        p (new_indent ^ "Then");
        print_stmt (new_indent ^ "  ") then_stmt;
        (match else_opt with
         | Some else_stmt ->
             p (new_indent ^ "Else");
             print_stmt (new_indent ^ "  ") else_stmt
         | None -> ())
    | While (cond, body) ->
        p "While";
        print_expr new_indent cond;
        print_stmt (new_indent ^ "  ") body
    | Return e_opt ->
        p "Return";
        (match e_opt with
         | Some e -> print_expr new_indent e
         | None -> ())
    | Break -> p "Break"
    | Continue -> p "Continue"
    | Block stmts ->
        p "Block";
        List.iter (print_stmt new_indent) stmts

  let print_func_def func =
    let ftype_str = match func.ftype with TIntReturn -> "int" | TVoidReturn -> "void" in
    print_endline ("Function: " ^ ftype_str ^ " " ^ func.fname);
    List.iter (fun (ptype, pname) ->
      let ptype_str = match ptype with TInt -> "int" in
      print_endline ("  Param: " ^ ptype_str ^ " " ^ pname)
    ) func.params;
    print_stmt "  " func.body;
    print_endline ""

  let print_program (prog: program) =
    print_endline "--- Abstract Syntax Tree ---";
    List.iter print_func_def prog;
    print_endline "--------------------------"
end

let () =
  (* Check for the correct number of command-line arguments *)
  if Array.length Sys.argv <> 2 then
    begin
      prerr_endline "Usage: ./main <source_file.tc>";
      exit 1
    end;

  let filename = Sys.argv.(1) in
  let in_channel = try open_in filename with
    | Sys_error msg ->
        prerr_endline ("Error: Cannot open file '" ^ msg ^ "'");
        exit 1
    in

  let lexbuf = Lexing.from_channel in_channel in
  try
    (* Run the parser on the lexer buffer *)
    let ast = Parser.comp_unit Lexer.token lexbuf in
    close_in in_channel;
    (* If parsing is successful, print the AST *)
    Ast_printer.print_program ast
  with
  (* | Lexer.Error msg ->
      (* Catch lexical errors *)
      Printf.eprintf "Lexical error: %s at line %d, column %d\n"
        msg
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
      close_in_noerr in_channel;
      exit 1 *)
  | Parsing.Parse_error ->
      (* Catch syntax errors *)
      Printf.eprintf "Syntax error at line %d, column %d, near token '%s'\n"
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        (Lexing.lexeme lexbuf);
      close_in_noerr in_channel;
      exit 1