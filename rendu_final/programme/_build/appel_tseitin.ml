open Expr_tseitin ;;
open Tseitin ;;

(* Ecriture dans un fichier dans un format .cnf *)

let ecrire_chiffre fichier x = 
  output_string fichier ( string_of_int x) ;;

let passe_une_ligne fichier = 
  output_string fichier "\n" ;;

let espace fichier = 
  output_string fichier " " ;;

let rec ecrire_clause c fichier = 
  match c with 
  [] -> ecrire_chiffre fichier 0 ;
        passe_une_ligne fichier
 |x::q -> (*output_binary_int fichier x ;*)
          ecrire_chiffre fichier x ;
          espace fichier ;
          ecrire_clause q fichier ;;

let ecrire f fichier nb_var nb_clauses =
  output_string fichier "p cnf " ;
  ecrire_chiffre fichier nb_var ;
  espace fichier ;
  ecrire_chiffre fichier nb_clauses ; 
  passe_une_ligne fichier ;
  let rec aux f = match f with
  [] -> () 
 |c::q -> ecrire_clause c fichier ; 
          aux q ;
  in aux f ;; 
  
(* Le programme final *)

let compile e nom_fichier =
    affiche_formule e;
    print_newline();
    let (f,nb_var,nb_clauses) = transfo_de_tseitin e 
    and fichier = open_out nom_fichier
    in ecrire f fichier nb_var nb_clauses ;
    close_out fichier ;;

let lexbuf_tseitin file = Lexing.from_channel file ;;

let parse_tseitin file = Parser_tseitin.main Lexer_tseitin.token (lexbuf_tseitin file) ;;

let calc file =
  try (
    let result = parse_tseitin file in
    compile result "ex-tseitin.cnf" ; flush stdout )
  with _ -> print_string "erreur de saisie\n" ;;
