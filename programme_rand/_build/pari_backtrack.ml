open Types;;
open Atomes;;
open Propagation;;

(*////// PARI RAND ///////*)

(* On choisit une variable au hasard parmi celles qui ne sont pas encore 
   affectees *)

let les_var_inconnues tab_aff = 
  let l = ref [] 
  and n = Array.length tab_aff in 
  for i = 1 to (n-1) do
  if (tab_aff.(i) = Inconnu) then
  l := i::!l ;
  done;
  !l ;;

let rec k_ieme l k = 
  match l with
  [] -> failwith " pas de k-ieme element"
  |t::q -> if k = 1 then t
  else k_ieme q (k-1) ;;

let pari_rand tab_aff = 
  let var_inconnues = les_var_inconnues tab_aff in 
  let n = List.length var_inconnues in
  let rand = (Random.int n) + 1 in 
  k_ieme var_inconnues rand ;; 

(*
La fonction diminue le nombre de littéraux faux dans la clause.
*)

let rec dimlitfliclau nb_lit_f liclau = match liclau with
   | [] -> ()
   | t :: q -> affect nb_lit_f t (nb_lit_f.(t) - 1);
      dimlitfliclau nb_lit_f q
;;


let evalue var valeur = 
  match (var,valeur) with
  (_,Inconnu) -> Inconnu
 |_ -> if var > 0 then valeur
       else if valeur = Vrai then Faux
                             else Vrai 
;;
 

let rec valeur_de_clause c affectation = 
  match c with
  [] -> Faux
 | var::q -> let valeur = evalue var (affectation.(abs var)) in 
    match valeur with 
    Vrai -> Vrai 
   |Faux -> valeur_de_clause q affectation   
   |Inconnu -> Inconnu 
;;

(* Lors du back-track, on remet une variable a Inconnu,
   il faut alors reevaluer les clauses pour savoir lesquelles
   repasser a Inconnu *)

let rec recupere_quelles_clauses l affectation tabclau tap tan tdp tdn t_cl_v= 
   match l with 
   [] -> ()
  |i::q -> let c = tabclau.(i) in 
           match valeur_de_clause c affectation with 
	     Faux -> raise Probleme
          | Vrai -> recupere_quelles_clauses q affectation tabclau tap tan tdp tdn t_cl_v
          | Inconnu ->
	  t_cl_v.(i) <-Inconnu;
          des_clau c tdp tdn tap tan i;
          recupere_quelles_clauses q affectation tabclau tap tan tdp tdn t_cl_v;
;;


let rec retour_sur deductions tap tan tdp tdn tab_aff tabclau nb_lit_f t_cl_v= 
  match deductions with
   [] -> ()
  |var::q -> tab_aff.(abs var) <- Inconnu ;
             if var > 0 then
             begin
             recupere_quelles_clauses tdp.(var) tab_aff tabclau tap tan tdp tdn t_cl_v;
             dimlitfliclau nb_lit_f tdn.(var);
	     dimlitfliclau nb_lit_f tan.(var)
             end
             else
             begin 
             recupere_quelles_clauses tdn.(-var) tab_aff tabclau tap tan tdp tdn t_cl_v;
             dimlitfliclau nb_lit_f tdp.(-var);
	     dimlitfliclau nb_lit_f tap.(-var)
             end ;
             retour_sur q tap tan tdp tdn tab_aff tabclau nb_lit_f t_cl_v;;
