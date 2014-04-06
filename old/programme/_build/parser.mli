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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int list
