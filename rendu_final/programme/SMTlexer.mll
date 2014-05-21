{
open SMTparser;;        (* le type "token" est d�fini dans parser.mli *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                      lus les caract�res *)
  | "\194\172"        { NON }
  | "="               { EGAL }
  | "!"               { DISEGAL }
  | "\\/"             { OU }
  | ['0'-'9']+ as s   { INT (int_of_string s) }
  | '\n'              { EOL }
  | eof               { EOF }  
  | _                 { TRUC }
