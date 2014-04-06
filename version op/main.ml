open Propagation;;
open Atomes;;
open Pari_backtrack;;
open Pretraitement;;
open Types;;
(*/// Affichage de la solution ///*)

let print_litteral i l = match l with
  Vrai -> print_int (i) ;
 |Faux -> print_int(-i) ;
 |Inconnu -> print_int i ;;(* aucune imp si Inconnu *)

let rec affiche tab nb_var =
  print_string("s SATISFIABLE\n") ;
  for i = 1 to nb_var do 
  print_string("v ") ;
  print_litteral i (tab.(i));
  print_string("\n");
  done ;;

let fin t_cl_v=
	let rec f n = if (n=Array.length t_cl_v)
		then raise Satisfiable
		else (if t_cl_v.(n)=Vrai
			then f (n+1)
			else ())
	in f 1
;;

let uneafec tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff  paris tabclau pariheu=
conflitoupas nb_lit nb_lit_f t_cl_v;
match deduction tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff tabclau with
		| None -> let b=pariheu tab_aff nb_lit nb_lit_f tabclau t_cl_v tab_act_pos tab_act_neg in
				paris:= (b,[])::!paris;print_string "on parie sur ";print_int b;print_string "\n";
				b
		| Some a -> match !paris with
				| [] ->a
				| (b,l) :: q -> paris:=(b,a::l)::q;print_string "on déduit ";print_int a;print_string "\n";
						a
;;

	
let uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu=
let a=uneafec tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu in	
			(if (abs a=a)
			then tab_aff.(abs a)<-Vrai
			else tab_aff.(abs a)<-Faux;
			propage a tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit_f tabclau t_cl_v)
;;


let back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau=match !paris with
| [] -> raise Insatisfiable
| (a,ded)::q -> print_string "On backtrack sur "; print_int a;print_string "\n";
			(retour_sur (a::ded) tab_act_pos tab_act_neg tab_des_pos tab_des_neg tab_aff tabclau nb_lit_f t_cl_v;
			tab_aff.(abs a) <- if a > 0 then Faux else Vrai ;
			propage (-a) tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit_f tabclau t_cl_v;
			match q with
			  | [] -> paris:=[]
			  | (d,ded1)::q1 -> paris:=(d,(-a)::ded1)::q1)
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


let resoud pariheu = let entree = parse () in
  let (nb_clause,n,tabclau) = construire_formule entree in
  let tab_aff = init_affectation n 
  and (tab_act_pos,tab_act_neg,nb_lit) = init_les_tab tabclau n nb_clause 
  and tab_des_pos = Array.create (n+1) [] 
  and tab_des_neg = Array.create (n+1) [] 
  and nb_lit_f = Array.create (nb_clause+1) 0 
  and t_cl_v = Array.create (nb_clause+1) Inconnu 
  and paris = ref [] 
  and i=ref 0 in
   try(
	while (true)
 	do
		(try (
			fin t_cl_v;
			uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu)
		with
		| Conflit -> back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau);
	i:=!i+1;
	done ) 
   with
   Satisfiable -> affiche tab_aff (Array.length tab_aff -1)
   | Insatisfiable -> print_string "s UNSATISFIABLE\n"
;;


let _ =Random.self_init;resoud pari_moms;;




