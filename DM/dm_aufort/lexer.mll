{
open Parser;;     
}

rule token = parse   
  | [' ' '\t']        { token lexbuf } (* on saute les blancs *)
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
