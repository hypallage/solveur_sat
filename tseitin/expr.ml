(* un type pour des expressions arithmétiques simples *)

type formule = 
  Var of int
 | And of formule * formule
 | Or of formule * formule 
 | Implies of formule * formule 
 | Not of formule 

(* fonction d'affichage *)

(* formule quelconque *)

let rec affiche_formule e =
  let aff_aux_binaire s a b = 
      begin
	print_string s;
	affiche_formule a;
	print_string ", ";
	affiche_formule b;
	print_string ")"
      end
  and aff_aux_unaire s a = 
      begin
	print_string s;
	affiche_formule a;
	print_string ")"
      end
  in
  match e with
  | Var k -> print_int k
  | And(f1,f2) -> aff_aux_binaire "And(" f1 f2
  | Or(f1,f2) -> aff_aux_binaire "Or(" f1 f2
  | Implies(f1,f2) -> aff_aux_binaire "Implies(" f1 f2 
  | Not(f1) -> aff_aux_unaire "Not(" f1 

(* formule cnf *)

let affiche_clause c = 
  let rec aux c = 
  match c with
  [] -> print_string "]" ;
 |x::q -> print_int x ;
          match q with
          [] -> print_string "]" ;
         |_ -> print_string ";" ;
               aux q
  in print_string "[" ;
  aux c ;;

let affiche_cnf f = 
  let rec aux1 f = 
  match f with
  [] -> print_string "]" ;
 |c::q -> affiche_clause c ;
          match q with
          [] -> print_string "]" ;
         |_ -> print_string ";" ;
               aux1 q
  in print_string "[" ;
  aux1 f ;;
  
