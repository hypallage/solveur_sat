{
open Parser;;        (* le type "token" est défini dans parser.mli *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | 'p'               { P }
  | 'c'               { C }
  | 'n'               { N }
  | 'f'               { F } 
  | '-'               { MOINS }
  | '0'               { ZERO } 
  | ['0'-'9']+ as s   { INT (int_of_string s) }
  | '\n'              { EOL }
  | eof               { EOF }  
  | _                 { TRUC }
