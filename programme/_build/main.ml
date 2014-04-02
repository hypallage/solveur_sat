open Propagation;;
open Atomes;;
open Pari_backtrack;;
open Pretraitement;;
open Types;;
(*/// Affichage de la solution ///*)

let print_litteral i l = match l with
  Vrai -> print_int (i) ;
 |Faux -> print_int(-i) ;
 |Inconnu -> print_int(i) ;; (* aucune imp si Inconnu *)

let rec affiche tab nb_var =
  print_string("s SATISFIABLE\n") ;
  for i = 1 to nb_var do 
  print_string("v ") ;
  print_litteral i (tab.(i));
  print_string("\n");
  done ;;

let fin t_cl_v=
  let rec f n = 
    if (n=Array.length t_cl_v)
    then raise Satisfiable
    else (if t_cl_v.(n)=Vrai
          then f (n+1)  
          else ())
  in f 1 ;;

let uneafec tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff  paris tabclau =
conflitoupas nb_lit nb_lit_f t_cl_v;
match deduction tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff tabclau with
		| None -> let b = pari tab_aff in
				paris:= (b,[])::!paris; (*print_string "Je parie sur "; print_int b ;print_string "\n";*)
				b
		| Some a -> (*print_string "Je deduis " ; print_int a ; print_string "\n" ;*)
                               match !paris with
				| [] -> a
				| (b,l) :: q -> paris:=(b,a::l)::q ; a ;;

	
let uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau =
let a = uneafec tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau in	
          		(if (abs a=a)
			then tab_aff.(abs a)<-Vrai
			else tab_aff.(abs a)<-Faux;
			propage a tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit_f tabclau t_cl_v)
;;


let back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau = match !paris with
| [] -> raise Insatisfiable
| (a,ded)::q -> if a<0
		then raise Probleme
		else 	((*print_string "Retour sur le pari " ; print_int a; print_string "\n";*)
			retour_sur (a::ded) tab_act_pos tab_act_neg tab_des_pos tab_des_neg tab_aff tabclau nb_lit_f t_cl_v;
			tab_aff.(abs a) <- if a > 0 then Faux else Vrai ;
			propage (-a) tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit_f tabclau t_cl_v;
			match q with
			  | [] -> ()
			  | (d,ded1)::q1 -> paris:=(d,(-a)::ded1)::q1) ;
;;		


let lexbuf = Lexing.from_channel stdin;;

let parse () = Parser.main Lexer.token lexbuf;;

let printb b= match b with
true -> print_string "true"
| false -> print_string "false"
;;
let rec contain l a= match l with
| [] -> false
| t::q -> if t=a
	then true
	else contain q a
;;


let resoud () = let entree = parse () in
  let (nb_clause,n,tabclau) = construire_formule entree in
  let tab_aff = init_affectation n 
  and (tab_act_pos,tab_act_neg,nb_lit) = init_les_tab tabclau n nb_clause 
  and tab_des_pos = Array.create (n+1) [] 
  and tab_des_neg = Array.create (n+1) [] 
  and nb_lit_f = Array.create (nb_clause+1) 0 
  and t_cl_v = Array.create (nb_clause+1) Inconnu 
  and paris = ref [] 
  and i = ref 0 in
   try(
	while (true)
 	do
		(try (
			fin t_cl_v;
			uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau)
		with
		| Conflit -> back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau);
	i:=!i+1;
	done ) 
   with
   Satisfiable -> affiche tab_aff (Array.length tab_aff -1)
   | Insatisfiable -> print_string " s UnSATISFIABLE"
;;


let _ =resoud ();;



