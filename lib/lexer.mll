(* lexer.mll *)
{
  open Parser (* Import token definitions from the parser *)
  (* CORRECTED: Define a custom exception for the lexer. *)
  exception Error of string
}

(* Helper rule for digits *)
let digit = ['0'-'9']
let digits = digit+

(* Main lexer rule *)
rule token = parse
  (* Whitespace and Comments - to be ignored *)
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t' '\r']    { token lexbuf } (* Ignore whitespace *)
  | "//" [^'\n']*  { token lexbuf }  (* 跳过整行后，继续 lex *)
  | "/*" { comment lexbuf}  (* 忽略多行注释 *)

  (* Punctuators *)
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | ';'     { SEMI }
  | ','     { COMMA }

  (* Operators *)
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | "&&"    { AND }
  | "||"    { OR }
  | "!"     { NOT }
  | "="     { ASSIGN }
  | "=="    { EQ }
  | "!="    { NEQ }
  | "<"     { LT }
  | "<="    { LTE }
  | ">"     { GT }
  | ">="    { GTE }

  (* Keywords *)
  | "int"       { INT }
  | "void"      { VOID }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "break"     { BREAK }
  | "continue"  { CONTINUE }
  | "return"    { RETURN }

  (* Identifiers and Numbers *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID lxm }
  | digits as lxm                                               { NUMBER (int_of_string lxm) }

  (* End of File *)
  | eof     { EOF }

  (* Error case *)
  | _ as chars   {raise (Error (Printf.sprintf "Illegal character: %c" chars))}

and comment = parse 
  | "*/"                 { token lexbuf }
  | eof                  { raise (Error "Unterminated multi-line comment") } 
  | _                    { comment lexbuf } (* 处理其他所有字符 *)
