type token =
  | INT of (int)
  | P
  | C
  | N
  | F
  | MOINS
  | EOF
  | ZERO
  | EOL
  | TRUC

open Parsing;;
# 2 "parser.mly"

# 17 "parser.ml"
let yytransl_const = [|
  258 (* P *);
  259 (* C *);
  260 (* N *);
  261 (* F *);
  262 (* MOINS *);
    0 (* EOF *);
  263 (* ZERO *);
  264 (* EOL *);
  265 (* TRUC *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\004\000\004\000\004\000\002\000\002\000\
\006\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\008\000\001\000\002\000\002\000\002\000\001\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\001\000\002\000\003\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\023\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\009\000\001\000\007\000\000\000\016\000\011\000\
\010\000\012\000\013\000\014\000\015\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\022\000\003\000\
\000\000\000\000\021\000\000\000\019\000\005\000\006\000\020\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\040\000\041\000\042\000\019\000"

let yysindex = "\005\000\
\000\255\000\000\004\255\009\255\000\000\006\255\000\000\026\255\
\030\255\009\255\009\255\009\255\009\255\009\255\009\255\009\255\
\000\000\009\255\000\000\000\000\000\000\031\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\255\036\255\
\032\255\001\000\025\255\037\255\000\000\033\255\000\000\000\000\
\001\000\001\000\000\000\025\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\031\000\037\000\219\255\221\255\027\000\009\000"

let yytablesize = 265
let yytable = "\043\000\
\037\000\003\000\004\000\046\000\047\000\001\000\009\000\003\000\
\048\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\035\000\030\000\008\000\004\000\032\000\036\000\038\000\
\039\000\022\000\008\000\031\000\033\000\044\000\021\000\034\000\
\045\000\008\000\020\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\000\000\004\000\000\000\000\000\036\000\038\000\
\039\000"

let yycheck = "\035\000\
\000\000\002\001\003\001\041\000\042\000\001\000\003\001\002\001\
\044\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\000\011\000\012\000\013\000\014\000\015\000\
\016\000\001\001\018\000\001\000\003\001\001\001\006\001\007\001\
\008\001\004\001\008\000\005\001\001\001\001\001\008\000\008\001\
\008\001\002\001\006\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001"

let yynames_const = "\
  P\000\
  C\000\
  N\000\
  F\000\
  MOINS\000\
  EOF\000\
  ZERO\000\
  EOL\000\
  TRUC\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'commentaire_liste) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'suite) in
    Obj.repr(
# 23 "parser.mly"
                            (_2)
# 171 "parser.ml"
               : int list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'suite) in
    Obj.repr(
# 24 "parser.mly"
                            (_1)
# 178 "parser.ml"
               : int list))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'liste) in
    Obj.repr(
# 27 "parser.mly"
                            ( _5::_6::_8 )
# 187 "parser.ml"
               : 'suite))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
                       ( [] )
# 193 "parser.ml"
               : 'liste))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'liste) in
    Obj.repr(
# 32 "parser.mly"
                       ( _1@_2 )
# 201 "parser.ml"
               : 'liste))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'commentaire) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'liste) in
    Obj.repr(
# 33 "parser.mly"
                       ( _2 )
# 209 "parser.ml"
               : 'liste))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'commentaire) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'commentaire_liste) in
    Obj.repr(
# 39 "parser.mly"
                                  ( _2 )
# 217 "parser.ml"
               : 'commentaire_liste))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'commentaire) in
    Obj.repr(
# 40 "parser.mly"
                                  ( _1 )
# 224 "parser.ml"
               : 'commentaire_liste))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 43 "parser.mly"
             ( _2 )
# 231 "parser.ml"
               : 'commentaire))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 46 "parser.mly"
                 ( _2 )
# 238 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 47 "parser.mly"
                 ( _2 )
# 245 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 48 "parser.mly"
                 ( _2 )
# 252 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 49 "parser.mly"
                 ( _2 )
# 259 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 50 "parser.mly"
                 ( _2 )
# 266 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 51 "parser.mly"
                 ( _2 )
# 273 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 52 "parser.mly"
                 ( _2 )
# 281 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'texte) in
    Obj.repr(
# 53 "parser.mly"
                 ( _2 )
# 288 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                 ( [] )
# 294 "parser.ml"
               : 'texte))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
             ( [0] )
# 300 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 59 "parser.mly"
                     ( (-_2)::_3 )
# 308 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 60 "parser.mly"
                     (  _1::_2 )
# 316 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
        ( [] )
# 322 "parser.ml"
               : 'clause))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int list)
