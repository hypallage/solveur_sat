(*///// TRANSFORMATION DE TSEITIN /////*)

open Expr ;;

(* Types et donnees *)

(* Pour generer des variables fraiches, on incremente un compteur initialise
  au prealable au nombre de variables de la clause *)

let max a b = 
  if a > b then a else b ;;

let rec nb_var_ds_formule f = match f with
   Var(x) -> abs x  
 | And(f1,f2) -> max (nb_var_ds_formule f1) (nb_var_ds_formule f2)
 | Or(f1,f2) -> max (nb_var_ds_formule f1) (nb_var_ds_formule f2)
 | Implies(f1,f2) -> max (nb_var_ds_formule f1) (nb_var_ds_formule f2)
 | Not(f1) -> nb_var_ds_formule f1 ;;

let variable_fraiche compteur = 
  incr compteur ;
  !compteur ;;

(* Transformation de Tseitin *)

let crochet_and xi xi1 xi2 =
   [[-xi;xi1];[-xi;xi2];[xi;-xi1;-xi2]] ;;

let crochet_or xi xi1 xi2 =
   [[-xi;xi1;xi2];[xi;-xi1];[xi;-xi2]] ;;

let crochet_implies xi xi1 xi2 =
   [[-xi;xi1];[-xi;-xi2];[xi;-xi1;xi2]] ;;

let rec tseitin f compteur = 
  match f with
   Var(x) -> (x,[],0)
 | And(f1,f2) -> let ((xi1,t1,n1),(xi2,t2,n2)) = (tseitin f1 compteur,tseitin f2 compteur )
                 and xi = variable_fraiche compteur in 
                 (xi,t1@t2@(crochet_and xi xi1 xi2),n1+n2+3)               
 | Or(f1,f2) -> let ((xi1,t1,n1),(xi2,t2,n2)) = (tseitin f1 compteur,tseitin f2 compteur )
                and xi = variable_fraiche compteur in 
                (xi,t1@t2@(crochet_or xi xi1 xi2),n1+n2+3)   
 | Implies(f1,f2) -> let ((xi1,t1,n1),(xi2,t2,n2)) = (tseitin f1 compteur,tseitin f2 compteur )
                     and xi = variable_fraiche compteur in 
                     (xi,t1@t2@(crochet_implies xi xi1 xi2),n1+n2+3)   
 | Not(f1) -> let (xi,t,n) = tseitin f1 compteur in (-xi,t,n) ;;

let transfo_de_tseitin f = 
  let compteur = ref ( nb_var_ds_formule f) in
  let (q,t,n) = tseitin f compteur in 
  ([[q]]@t,!compteur,n+1) ;;

(* on renvoit compteur = nb_var
   ainsi que n+1 = nb_clauses *)





