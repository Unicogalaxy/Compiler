type token =
  | NUMBER of (
# 7 "./lib/parser.mly"
        int
# 6 "./lib/parser.ml"
)
  | ID of (
# 8 "./lib/parser.mly"
        string
# 11 "./lib/parser.ml"
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

open Parsing
let _ = parse_error;;
# 3 "./lib/parser.mly"
  open Ast
# 48 "./lib/parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* VOID *);
  261 (* IF *);
  262 (* ELSE *);
  263 (* WHILE *);
  264 (* BREAK *);
  265 (* CONTINUE *);
  266 (* RETURN *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* LBRACE *);
  270 (* RBRACE *);
  271 (* SEMI *);
  272 (* COMMA *);
  273 (* ASSIGN *);
  274 (* OR *);
  275 (* AND *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* LTE *);
  280 (* GT *);
  281 (* GTE *);
  282 (* PLUS *);
  283 (* MINUS *);
  284 (* TIMES *);
  285 (* DIV *);
  286 (* MOD *);
  287 (* NOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUMBER *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\003\000\005\000\005\000\
\006\000\006\000\013\000\008\000\009\000\009\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\011\000\011\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\006\000\000\000\001\000\
\001\000\003\000\002\000\003\000\000\000\002\000\001\000\002\000\
\005\000\004\000\007\000\005\000\005\000\002\000\002\000\002\000\
\003\000\001\000\001\000\001\000\003\000\004\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\005\000\050\000\000\000\002\000\000\000\
\001\000\003\000\000\000\000\000\000\000\000\000\000\000\009\000\
\011\000\000\000\000\000\013\000\006\000\010\000\000\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\015\000\000\000\000\000\014\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\023\000\000\000\024\000\
\000\000\000\000\031\000\032\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\043\000\044\000\045\000\
\030\000\000\000\018\000\000\000\000\000\000\000\000\000\017\000\
\000\000\021\000\000\000\019\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\014\000\015\000\037\000\038\000\
\023\000\039\000\068\000\069\000\016\000"

let yysindex = "\255\255\
\005\255\000\000\000\000\000\000\000\000\001\000\000\000\021\255\
\000\000\000\000\027\255\041\255\048\255\043\255\055\255\000\000\
\000\000\051\255\041\255\000\000\000\000\000\000\038\255\000\000\
\007\255\075\255\046\255\073\255\071\255\079\255\097\255\099\255\
\000\000\000\000\099\255\099\255\000\000\000\000\190\255\099\255\
\099\255\092\255\099\255\099\255\000\000\000\000\086\255\000\000\
\206\255\136\255\000\000\000\000\000\000\099\255\099\255\099\255\
\099\255\099\255\099\255\099\255\099\255\099\255\099\255\099\255\
\099\255\099\255\049\000\113\255\118\255\222\255\099\255\155\255\
\174\255\000\000\000\000\061\000\093\255\101\000\101\000\076\255\
\076\255\076\255\076\255\121\255\121\255\000\000\000\000\000\000\
\000\000\099\255\000\000\244\255\080\255\080\255\049\000\000\000\
\146\255\000\000\080\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\141\255\000\000\000\000\156\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\157\255\
\000\000\000\000\000\000\000\000\000\000\000\000\117\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\042\255\000\000\158\255\000\000\000\000\000\000\
\000\000\000\000\000\000\130\000\044\255\120\000\125\000\001\255\
\080\000\090\000\100\000\023\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\115\255\000\000\
\065\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\165\000\000\000\000\000\000\000\167\255\154\000\
\000\000\227\255\000\000\000\000\168\000"

let yytablesize = 404
let yytable = "\001\000\
\009\000\049\000\050\000\097\000\098\000\051\000\052\000\003\000\
\004\000\100\000\067\000\070\000\037\000\072\000\073\000\037\000\
\037\000\040\000\037\000\037\000\037\000\037\000\011\000\041\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\012\000\024\000\025\000\
\026\000\092\000\027\000\013\000\028\000\029\000\030\000\031\000\
\032\000\017\000\020\000\033\000\034\000\048\000\018\000\034\000\
\043\000\048\000\034\000\034\000\095\000\034\000\034\000\020\000\
\035\000\020\000\020\000\020\000\036\000\020\000\019\000\020\000\
\020\000\020\000\020\000\020\000\042\000\020\000\020\000\020\000\
\024\000\025\000\026\000\044\000\027\000\045\000\028\000\029\000\
\030\000\031\000\032\000\020\000\020\000\046\000\034\000\020\000\
\040\000\024\000\047\000\024\000\047\000\062\000\063\000\064\000\
\065\000\066\000\035\000\032\000\071\000\032\000\036\000\048\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\035\000\089\000\035\000\049\000\036\000\
\028\000\036\000\049\000\028\000\028\000\090\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\075\000\064\000\065\000\066\000\099\000\
\007\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\093\000\008\000\
\046\000\047\000\010\000\021\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\094\000\022\000\000\000\000\000\000\000\000\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\053\000\000\000\000\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\074\000\000\000\000\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\091\000\000\000\000\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\000\000\000\000\000\000\000\000\
\000\000\000\000\096\000\003\000\004\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\028\000\000\000\000\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\041\000\000\000\000\000\041\000\041\000\000\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\042\000\000\000\000\000\042\000\042\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\038\000\000\000\000\000\038\000\038\000\
\000\000\038\000\038\000\038\000\038\000\039\000\000\000\000\000\
\039\000\039\000\000\000\039\000\039\000\039\000\039\000\040\000\
\000\000\000\000\040\000\040\000\000\000\040\000\040\000\040\000\
\040\000\000\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\035\000\000\000\000\000\035\000\035\000\
\036\000\035\000\035\000\036\000\036\000\033\000\036\000\036\000\
\033\000\033\000\000\000\033\000"

let yycheck = "\001\000\
\000\000\031\000\032\000\093\000\094\000\035\000\036\000\003\001\
\004\001\099\000\040\000\041\000\012\001\043\000\044\000\015\001\
\016\001\011\001\018\001\019\001\020\001\021\001\002\001\017\001\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\011\001\001\001\002\001\
\003\001\071\000\005\001\003\001\007\001\008\001\009\001\010\001\
\011\001\002\001\013\001\014\001\015\001\012\001\012\001\012\001\
\011\001\016\001\015\001\016\001\090\000\018\001\019\001\013\001\
\027\001\001\001\002\001\003\001\031\001\005\001\016\001\007\001\
\008\001\009\001\010\001\011\001\002\001\013\001\014\001\015\001\
\001\001\002\001\003\001\011\001\005\001\015\001\007\001\008\001\
\009\001\010\001\011\001\027\001\013\001\015\001\015\001\031\001\
\011\001\001\001\002\001\001\001\002\001\026\001\027\001\028\001\
\029\001\030\001\027\001\011\001\017\001\011\001\031\001\015\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\027\001\012\001\027\001\012\001\031\001\
\012\001\031\001\016\001\015\001\016\001\016\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\012\001\028\001\029\001\030\001\006\001\
\012\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\012\001\012\001\
\012\001\012\001\006\000\018\000\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\012\001\019\000\255\255\255\255\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\015\001\003\001\004\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\015\001\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\012\001\255\255\255\255\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\012\001\255\255\255\255\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\012\001\255\255\255\255\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\012\001\255\255\255\255\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\012\001\
\255\255\255\255\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\012\001\255\255\255\255\015\001\016\001\
\012\001\018\001\019\001\015\001\016\001\012\001\018\001\019\001\
\015\001\016\001\255\255\018\001"

let yynames_const = "\
  INT\000\
  VOID\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  BREAK\000\
  CONTINUE\000\
  RETURN\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMI\000\
  COMMA\000\
  ASSIGN\000\
  OR\000\
  AND\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  NOT\000\
  EOF\000\
  "

let yynames_block = "\
  NUMBER\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.func_def list) in
    Obj.repr(
# 53 "./lib/parser.mly"
                    ( List.rev _1 )
# 310 "./lib/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.func_def) in
    Obj.repr(
# 58 "./lib/parser.mly"
    ( [_1] )
# 317 "./lib/parser.ml"
               : Ast.func_def list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.func_def list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.func_def) in
    Obj.repr(
# 60 "./lib/parser.mly"
    ( _2 :: _1 )
# 325 "./lib/parser.ml"
               : Ast.func_def list))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "./lib/parser.mly"
          ( TIntReturn )
# 331 "./lib/parser.ml"
               : Ast.func_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "./lib/parser.mly"
            ( TVoidReturn )
# 337 "./lib/parser.ml"
               : Ast.func_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Ast.func_type) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.param list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 70 "./lib/parser.mly"
    ( { ftype = _1; fname = _2; params = _4; body = _6 } )
# 347 "./lib/parser.ml"
               : Ast.func_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "./lib/parser.mly"
    ( [] )
# 353 "./lib/parser.ml"
               : Ast.param list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.param list) in
    Obj.repr(
# 77 "./lib/parser.mly"
    ( List.rev _1 )
# 360 "./lib/parser.ml"
               : Ast.param list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 82 "./lib/parser.mly"
    ( [_1] )
# 367 "./lib/parser.ml"
               : Ast.param list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.param list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 84 "./lib/parser.mly"
    ( _3 :: _1 )
# 375 "./lib/parser.ml"
               : Ast.param list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "./lib/parser.mly"
    ( (TInt, _2) )
# 382 "./lib/parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt list) in
    Obj.repr(
# 94 "./lib/parser.mly"
    ( Block (List.rev _2) )
# 389 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "./lib/parser.mly"
    ( [] )
# 395 "./lib/parser.ml"
               : Ast.stmt list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 101 "./lib/parser.mly"
    ( _2 :: _1 )
# 403 "./lib/parser.ml"
               : Ast.stmt list))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "./lib/parser.mly"
    ( Empty )
# 409 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 108 "./lib/parser.mly"
    ( ExprStmt _1 )
# 416 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 110 "./lib/parser.mly"
    ( VarDef(TInt, _2, _4) )
# 424 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 112 "./lib/parser.mly"
    ( Assign(_1, _3) )
# 432 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 114 "./lib/parser.mly"
    ( If(_3, _5, Some _7) )
# 441 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 116 "./lib/parser.mly"
    ( If(_3, _5, None) )
# 449 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 118 "./lib/parser.mly"
    ( While(_3, _5) )
# 457 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "./lib/parser.mly"
    ( Break )
# 463 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "./lib/parser.mly"
    ( Continue )
# 469 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "./lib/parser.mly"
    ( Return None )
# 475 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 126 "./lib/parser.mly"
    ( Return (Some _2) )
# 482 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 128 "./lib/parser.mly"
    ( _1 )
# 489 "./lib/parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 133 "./lib/parser.mly"
    ( Constant _1 )
# 496 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 135 "./lib/parser.mly"
    ( Var _1 )
# 503 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 137 "./lib/parser.mly"
    ( _2 )
# 510 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 139 "./lib/parser.mly"
    ( Call(_1, _3) )
# 518 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 141 "./lib/parser.mly"
    ( UnaOp(Neg, _2) )
# 525 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 143 "./lib/parser.mly"
    ( UnaOp(Not, _2) )
# 532 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 145 "./lib/parser.mly"
    ( BinOp(_1, Or, _3) )
# 540 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 147 "./lib/parser.mly"
    ( BinOp(_1, And, _3) )
# 548 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 149 "./lib/parser.mly"
    ( BinOp(_1, Eq, _3) )
# 556 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 151 "./lib/parser.mly"
    ( BinOp(_1, Neq, _3) )
# 564 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 153 "./lib/parser.mly"
    ( BinOp(_1, Lt, _3) )
# 572 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 155 "./lib/parser.mly"
    ( BinOp(_1, Lte, _3) )
# 580 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 157 "./lib/parser.mly"
    ( BinOp(_1, Gt, _3) )
# 588 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 159 "./lib/parser.mly"
    ( BinOp(_1, Gte, _3) )
# 596 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 161 "./lib/parser.mly"
    ( BinOp(_1, Add, _3) )
# 604 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 163 "./lib/parser.mly"
    ( BinOp(_1, Sub, _3) )
# 612 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 165 "./lib/parser.mly"
    ( BinOp(_1, Mul, _3) )
# 620 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 167 "./lib/parser.mly"
    ( BinOp(_1, Div, _3) )
# 628 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 169 "./lib/parser.mly"
    ( BinOp(_1, Mod, _3) )
# 636 "./lib/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "./lib/parser.mly"
    ( [] )
# 642 "./lib/parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 176 "./lib/parser.mly"
    ( List.rev _1 )
# 649 "./lib/parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 181 "./lib/parser.mly"
    ( [_1] )
# 656 "./lib/parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 183 "./lib/parser.mly"
    ( _3 :: _1 )
# 664 "./lib/parser.ml"
               : Ast.expr list))
(* Entry comp_unit *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let comp_unit (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
;;
