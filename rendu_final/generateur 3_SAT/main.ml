(* Generer des tests *)


(* On cree des clauses aleatoires, puis des formules *)
(* Pour l'instant, le choix du nombre de variables et 
   du nombre de clauses ne nous interesse pas. *)

let random_variable n = 
  let x = (Random.int n) + 1
  and signe = Random.int 2 in 
  if signe = 0 then (-x) else x ;;

let rec insere x l = 
  match l with
  [] -> true 
| t::q -> if abs t = abs x then false
           else insere x q ;;

let rec random_clause nb_var_clau nb_var = 
  if nb_var_clau = 0 then [] 
  else let l = random_clause (nb_var_clau-1) nb_var
  in let boo = ref true 
  and x = ref 0 in
  while (!boo) do 
    x := random_variable nb_var ;
    if insere (!x) l then
    boo := false ;
  done ; !x::l ;;

(* Choix du nombre de variables dans une clause
   aleatoire : plusieurs possibilites *)

(* Un exemple *) 

let random_nb_var nb_max = (* totalement aleatoire *)
  (Random.int nb_max) + 1 ;; 
   
  
let rec random_formule nb_clauses nb_max = 
  if nb_clauses = 0 then [] 
  else let c = random_clause 3 nb_max in 
  c::(random_formule (nb_clauses-1) nb_max) ;;

(* Generation de fichier d'exemple : avec bash ! *) 
 
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


let exec nb_clauses nb_var = 
  let f = random_formule nb_clauses nb_var in 
  let fichier = open_out "ex.cnf" in 
  ecrire f fichier nb_var nb_clauses ;; 

let _ =
  let data = Sys.argv in 
  Random.self_init () ;
  let nb_var = int_of_string (data.(1)) 
  and nb_clauses = int_of_string (data.(2)) 
  in exec nb_clauses nb_var ;; 

  
 

