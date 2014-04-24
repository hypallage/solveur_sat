open Propagation;;
open Atomes;;
open Pari_backtrack;;
open Pretraitement;;
open Types;;
open Appel_tseitin;;
open Appel_graphe;;
(*/// Affichage de la solution ///*)

let print_litteral i l = match l with
  Vrai -> print_int (i) ;
 |Faux -> print_int(-i) ;
 |Inconnu -> print_int (-i) ;;(* aucune imp si Inconnu *)

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
		| None -> (	let b=pariheu tab_aff nb_lit nb_lit_f tabclau t_cl_v tab_act_pos tab_act_neg in
				paris:= (b,[])::!paris;
				b)
		| Some a -> match !paris with
				| [] -> a
				| (b,l) :: q -> paris:=(b,a::l)::q;
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
			uneetape tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau pariheu)
		with
		| Conflit -> back tab_act_pos tab_act_neg tab_des_pos tab_des_neg nb_lit nb_lit_f t_cl_v tab_aff paris tabclau);
	i:=!i+1;
	done;
	tab_aff ) 
   with
   Satisfiable -> affiche tab_aff (Array.length tab_aff -1);tab_aff.(0) <- Vrai;tab_aff
   | Insatisfiable -> print_string "s UNSATISFIABLE\n";tab_aff.(0) <- Faux;tab_aff
;;


let rand = ref false and dlis = ref false and moms= ref false and moms2= ref false and tseitin=ref false and graphe=ref 0;;

let set_graphe nb = graphe:= nb;;

let _ =
let speclist = [("-rand", Arg.Set rand, "Execute le programme avec l'heuristique rand");
("-dlis", Arg.Set dlis, "Execute le programme avec l'heuristique dlis");
("-moms", Arg.Set moms, "Execute le programme avec l'heuristique moms");
("-moms2", Arg.Set moms2, "Execute le programme avec l'heuristique moms2");
("-tseitin", Arg.Set tseitin, "Execute le programme à partir d'une formule quelconque");
("-graphe", Arg.Int  (set_graphe), "Regarde si le graphe donné en entrée est k-coloriable et donc attends un entier")
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
	else (if (!rand)
		then (resoud pari_rand Sys.argv.(2))
		else if (!dlis)
			then resoud pari_dlis Sys.argv.(2)
			else  if (!moms)
				then (resoud pari_moms Sys.argv.(2))
				else if (!moms2)
					then resoud pari_moms2 Sys.argv.(2) 
					else (print_endline Sys.argv.(1);resoud pari Sys.argv.(1)))
;;





