{
open Parser;;        (* le type "token" est défini dans parser.mli *)
exception Incorrect ;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                       lus les caractères *)
  | '/' '\\'            { AND }
  | '\\' '/'             { OR }
  | '~'             { NOT }
  | "=>"             { IMPLIES }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ['0'-'9']+ as s { VAR (int_of_string s) }
  | eof             { EOF } 
  | '\n'            { EOF } 
  | _               { raise Incorrect }


