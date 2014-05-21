open Propagation;;
open Atomes;;
open Pari_backtrack;;
open Pretraitement;;
open Types;;
open Appel_tseitin;;
open Appel_graphe;;
open SMT_pretraitement;;
open SMTsolveur;;

exception Fin;;
(* Affichage d'une valeur*)

let print_litteral i l = match l with
  Vrai -> print_int (i) ;
 |Faux -> print_int(-i) ;
 |Inconnu -> print_int (-i) ;;(* intéressant pour le coloriage de graphe *)

(* La fonction itère sur tout le tableau pour afficher la solution *)
let rec affiche tab nb_var =
  print_string("s SATISFIABLE\n") ;
  for i = 1 to nb_var do 
  print_string("v ") ;
  print_litteral i (tab.(i));
  print_string("\n");
  done ;;

(* Cette fonction sert à vérifier si toutes les clauses sont vérifiés *)

let fin t_cl_v=
	let rec f n = if (n=Array.length t_cl_v)
		then raise Satisfiable
		else (if t_cl_v.(n)=Vrai
			then f (n+1)
			else ())
	in f 1
;;


(*
La fonction uneaffec fait une affectation. Elle peut le faire de deux manières différentes :
soit elle déduit une valeur d'un pari précédent, soit elle pari sur une nouveau litéral. Elle 
peut déclencher une execption. En effet, c'est la fonction qui se rend d'un conflit et de ce fait,
elle déclenche l'execption Conflit.
*)

let uneafec tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff  paris tabclau pariheu ded=
conflitoupas nb_lit nb_lit_f t_cl_v;
match ded tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff tabclau with
		| None -> (	let b=pariheu tab_aff nb_lit nb_lit_f tabclau t_cl_v tab_act_pos tab_act_neg in
				paris:= (b,[])::!paris;
				b)
		| Some a -> match !paris with
				| [] -> a
				| (b,l) :: q -> paris:=(b,a::l)::q;
						a
;;

(* La fonction uneetape propage l'affectation trouvé par uneaffec *)
	
let uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu ded=
let a=uneafec tab_act_pos tab_act_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu ded in	
			(if (abs a=a)
			then tab_aff.(abs a)<-Vrai
			else tab_aff.(abs a)<-Faux;
			propage a tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit_f tabclau t_cl_v)
;;


let back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau=match !paris with
| [] -> raise Insatisfiable
| (a,ded)::q -> 	(retour_sur (a::ded) tab_act_pos tab_act_neg tab_des_pos tab_des_neg tab_aff tabclau nb_lit_f t_cl_v;
			tab_aff.(abs a) <- if a > 0 then Faux else Vrai ;
			propage (-a) tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit_f tabclau t_cl_v;
			match q with
			  | [] -> paris:=[]
			  | (d,ded1)::q1 -> paris:=(d,(-a)::ded1)::q1)
;;		
let lexbuf file=let f=open_in file in
Lexing.from_channel f;;

let parse file = Parser.main Lexer.token (lexbuf file);;

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

(* /// DPLL normal ////*)
let resoud pariheu file = let entree = parse file in
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
			uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu deduction)
		with
		| Conflit -> back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau);
	i:=!i+1;
	done;
	tab_aff ) 
   with
   Satisfiable -> affiche tab_aff (Array.length tab_aff -1);tab_aff.(0) <- Vrai;tab_aff
   | Insatisfiable -> print_string "s UNSATISFIABLE\n";tab_aff.(0) <- Faux;tab_aff
;;


let rand = ref false and dlis = ref false and moms= ref false and moms2= ref false and tseitin=ref false and graphe=ref 0 
and cl=ref false and cli=ref false and smt=ref false;;

let set_graphe nb = graphe:= nb;;

(* /// SMT ////*)
let resoud_smt pariheu file =
let entree = parse_SMT file in
let nblitc =ref 0 and liclau = ref [] and f=open_out "smt.cnf" and nombclau = ref 0 and rep = ref [0] and nbmax= ref 0 in
creeclauses entree liclau nblitc nombclau rep nbmax;
creefichcnf f (List.tl !rep) nblitc nombclau;
close_out f;
let tab_eq_clau=creetab (!liclau) nombclau nblitc in 
let (nb_clause,n,tabclau) = construire_formule (parse "smt.cnf")  in
let tab_aff = init_affectation n 
and (tab_act_pos,tab_act_neg,nb_lit) = init_les_tab tabclau n nb_clause 
and tab_des_pos = Array.create (n+1) [] 
and tab_des_neg = Array.create (n+1) [] 
and nb_lit_f = Array.create (nb_clause+1) 0 
and t_cl_v = Array.create (nb_clause+1) Inconnu 
and paris = ref [] in
   try
	while (true)
 	do
		try (
			fin t_cl_v;
			uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu deduction_SMT)
		with
		| Conflit -> back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau
		| Satisfiable -> let tab_taille =Array.create (!nbmax+1) 0 
				and tab_rep = Array.create (!nbmax+1) 0 in
				(metnbtab tab_rep;
				unifie tab_rep tab_taille tab_eq_clau tab_aff;
				if (verifie tab_rep tab_eq_clau tab_aff)
				then (print_endline "c bon";
				complete_ver tab_rep tab_eq_clau tab_aff;raise Fin)
				else print_endline "rate";back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau)
		
	done;
	tab_aff;
   with
   | Fin -> tab_aff
   | Insatisfiable -> print_string "smt UNSATISFIABLE\n";tab_aff
;;


let rand = ref false and dlis = ref false and moms= ref false and moms2= ref false and tseitin=ref false and graphe=ref 0 
and cl=ref false and cli=ref false and smt=ref false;;

let set_graphe nb = graphe:= nb;;

let _ =
Random.self_init ();
let speclist = [("-rand", Arg.Set rand, "Execute le programme avec l'heuristique rand");
("-dlis", Arg.Set dlis, "Execute le programme avec l'heuristique dlis");
("-moms", Arg.Set moms, "Execute le programme avec l'heuristique moms");
("-moms2", Arg.Set moms2, "Execute le programme avec l'heuristique moms2");
("-tseitin", Arg.Set tseitin, "Execute le programme à partir d'une formule quelconque");
("-graphe", Arg.Int  (set_graphe), "Regarde si le graphe donné en entrée est k-coloriable et donc attends un entier");
("-cl",Arg.Set cl, "Rajoute l'heuristique d'apprentisage de clause");
("-cli",Arg.Set cli, "Rajoute l'heuristique d'apprentisage de clause et le lance en mode interactif");
("-smt",Arg.Set smt, "Lance le programme en lui faisant executer du SMT")
]
in let usage_msg = "Sat Solveur réalisé par Patrice Coudert et Wiliam Aufort"
in Arg.parse speclist (fun anon -> ()) usage_msg;
if (!graphe!=0)
then (if (!rand)
	then (let (a,b)=cree_formule (!graphe) Sys.argv.(4) and c=resoud pari_rand "ex_graphe.cnf" in
				      (ecrit_dot (c) (!graphe) a b);c)
	else if (!dlis)
		then (let (a,b)=cree_formule (!graphe) Sys.argv.(4) and c=resoud pari_dlis "ex_graphe.cnf" in
				      (ecrit_dot (c) (!graphe) a b);c)
		else  if (!moms)
			then (let (a,b)=cree_formule (!graphe) Sys.argv.(4) and c=resoud pari_moms "ex_graphe.cnf" in
				      (ecrit_dot (c) (!graphe) a b);c)
			else if (!moms2)
				then (let (a,b)=cree_formule (!graphe) Sys.argv.(4) and c=resoud pari_moms2 "ex_graphe.cnf" in
				      (ecrit_dot (c) (!graphe) a b);c)
				else (let (a,b)=cree_formule (!graphe) Sys.argv.(3) and c=resoud pari "ex_graphe.cnf" in
				      (ecrit_dot (c) (!graphe) a b);c))
else if (!tseitin)
	then (if (!rand)
		then (let file=open_in Sys.argv.(3) in calc file;resoud pari_rand "ex-tseitin.cnf")
		else if (!dlis)
			then (let file=open_in Sys.argv.(3) in calc file;resoud pari_dlis "ex-tseitin.cnf")
			else  if (!moms)
				then (let file=open_in Sys.argv.(3) in calc file;resoud pari_moms "ex-tseitin.cnf")
				else if (!moms2)
					then (let file=open_in Sys.argv.(3) in calc file;resoud pari_moms2 "ex-tseitin.cnf")
					else (let file=open_in Sys.argv.(2) in calc file;resoud pari "ex-tseitin.cnf"))
	else (if (!smt)
		then (if (!rand)
			then (resoud_smt pari_rand Sys.argv.(3))
			else if (!dlis)
				then resoud_smt pari_dlis Sys.argv.(3)
				else  if (!moms)
					then (resoud_smt pari_moms Sys.argv.(3))
					else if (!moms2)
						then resoud_smt pari_moms2 Sys.argv.(3) 
						else (resoud_smt pari Sys.argv.(2)))
		else (if (!rand)
			then (resoud pari_rand Sys.argv.(2))
			else if (!dlis)
				then resoud pari_dlis Sys.argv.(2)
				else  if (!moms)
					then (resoud pari_moms Sys.argv.(2))
					else if (!moms2)
						then resoud pari_moms2 Sys.argv.(2) 
						else (resoud pari Sys.argv.(1))))
;;





