(* Une moulinette de tests *)

(*
open Propagation;;
open Atomes;;
open Pari_backtrack;;
open Pretraitement;;
open Types;;
open Main ;;
*)

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

let rec random_clause n = 
  if n = 0 then [] 
  else let l = random_clause (n-1) 
  in let boo = ref true 
  and x = ref 0 in
  while boo do 
    x := random_variable n in 
    if insere x l then
    boo := false ;
  done ; !x::l ;;

(* Choix du nombre de variables dans une clause
   aleatoire : plusieurs possibilites *)

(* Un exemple *) 

let choix_nb_var _ = 
   let x = Random.float 1 in 
   if x < 0.7 then 3
   else if x < 0.8 then 4
   else if x < 0.9 then 5   
   else if x < 0.95 then 2
   else 6 ;;
  
let rec random_formule nb_clauses nb_max = 
  if nb_clauses = 0 then [] 
  else let nb_var = choix_nb_var nb_max in  
  let c = random_clauses nb_var nb_max in 
  c::(random_formule nb_clauses nb_max) ;;

(* Generation de fichier d'exemple *) 
 
(*
let init_fichier fichier variable methode1 methode2 = 
  output_string fichier ( concat " " [variable;methode1;methode2] ) ;;

let un_test nb_clauses nb_max(* de variables *) = 
  let f = random_formule nb_clauses nb_max *)




(* Les tests *)

let print_clause l = 
  let rec aux l =  
  match l with
  [] -> print_string "]" ;
 |t::q -> print_int t ;
          match q with
          [] -> print_string "]" ;
         |_ -> print_string ";" ; aux q 
  in print_string "[" ; 
  aux l ;;

let print_formule l = 
  let rec aux l =  
  match l with
  [] -> print_string "]" ;
 |c::q -> print_clause c ;
          match q with
          [] -> print_string "]" ;
         |_ -> print_string ";" ; aux q 
  in print_string "[" ; 
  aux l ;;

let test1 () =   
  print_string "Entrer le nombre de clauses souhaite \n" ;
  let nb_clauses = read_int() in
  print_string "\n Entrer le nombre de variables \n" 
  let nb_var = read_int() in 
  let f = ranndom_formule nb_clauses nb_var in 
  print_formule f ;;
  
 

