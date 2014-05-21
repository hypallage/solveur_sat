type token =
  | INT of (int)
  | EOF
  | EOL
  | TRUC
  | OU
  | EGAL
  | DISEGAL
  | NON

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int list
