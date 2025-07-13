(* main.ml *)
(* This version reads from standard input (stdin)
   and writes the resulting assembly to standard output (stdout). *)

open Parse_project
open Printf

let () =
  (* 1. 直接从标准输入 stdin 创建词法分析器缓冲 *)
  let lexbuf = Lexing.from_channel stdin in

  try
    (* 2. 运行解析器获取AST *)
    let ast = Parser.comp_unit Lexer.token lexbuf in

    (* 3. 运行类型检查器 *)
    Typechecker.type_check_program ast;

    (* 4. 运行代码生成器 *)
    let assembly_code = Codegen.codegen_program ast in

    (* 5. 将最终的汇编代码打印到标准输出 *)
    print_endline assembly_code

  with
  | Lexer.Error msg ->
      (* 错误信息输出到标准错误流 (stderr) *)
      eprintf "Lexical error at line %d, column %d: %s\n"
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        msg;
      exit 1
  | Parsing.Parse_error ->
      (* 错误信息输出到标准错误流 (stderr) *)
      eprintf "Syntax error at line %d, column %d, near token '%s'\n"
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        (Lexing.lexeme lexbuf);
      exit 1
  | Typechecker.Type_error msg ->
      (* 错误信息输出到标准错误流 (stderr) *)
      eprintf "Type error: %s\n" msg;
      exit 1
  | Failure msg ->
      (* 其他编译器内部错误 *)
      eprintf "Compiler failure: %s\n" msg;
      exit 1