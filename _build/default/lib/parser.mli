type token =
  | NUMBER of (
# 7 "./lib/parser.mly"
        int
# 6 "./lib/parser.mli"
)
  | ID of (
# 8 "./lib/parser.mly"
        string
# 11 "./lib/parser.mli"
)
  | INT
  | VOID
  | IF
  | ELSE
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMI
  | COMMA
  | ASSIGN
  | OR
  | AND
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | NOT
  | EOF

val comp_unit :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
