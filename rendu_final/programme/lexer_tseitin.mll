{
open Parser_tseitin;;        (* le type "token" est d�fini dans parser.mli *)
exception Incorrect ;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                       lus les caract�res *)
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


