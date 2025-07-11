(* main.ml *)
(* The main entry point for the ToyC compiler. *)
(* This version connects all parts: Parser -> Typechecker -> Codegen *)

open Parse_project
(* open Ast *)
open Printf


let () =
  (* 1. Check for the correct number of command-line arguments *)
  if Array.length Sys.argv <> 2 then
    begin
      eprintf "Usage: %s <source_file.tc>\n" Sys.argv.(0);
      exit 1
    end;

  let filename = Sys.argv.(1) in
  let in_channel =
    try open_in filename
    with Sys_error msg ->
      eprintf "Error: Cannot open file '%s': %s\n" filename msg;
      exit 1
  in

  let lexbuf = Lexing.from_channel in_channel in
  try
    (* 2. Run the parser on the lexer buffer to get the AST *)
    let ast = Parser.comp_unit Lexer.token lexbuf in
    close_in in_channel;

    (* 3. Run the type checker on the AST. It will exit on error. *)
    Typechecker.type_check_program ast;

    (* 4. If type checking succeeds, run the code generator. *)
    let assembly_code = Codegen.codegen_program ast in

    (* 5. Print the final assembly code to standard output. *)
    print_endline assembly_code

  with
  | Lexer.Error msg ->
      (* Catch lexical errors *)
      eprintf "Lexical error at line %d, column %d: %s\n"
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        msg;
      close_in_noerr in_channel;
      exit 1
  | Parsing.Parse_error ->
      (* Catch syntax errors *)
      eprintf "Syntax error at line %d, column %d, near token '%s'\n"
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        (Lexing.lexeme lexbuf);
      close_in_noerr in_channel;
      exit 1
  | Failure msg ->
      (* Catch other compiler errors, like from codegen *)
      eprintf "Compiler failure: %s\n" msg;
      exit 1