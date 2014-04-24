type token =
  | INT of (int)
  | P
  | C
  | N
  | F
  | E
  | EDGE
  | EOF
  | ZERO
  | EOL
  | TRUC

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int list
