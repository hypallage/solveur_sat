type token =
  | VAR of (int)
  | AND
  | OR
  | IMPLIES
  | NOT
  | LPAREN
  | RPAREN
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr_tseitin.formule
