
open Types;;




let lexbuf_SMT file=let f=open_in file in
Lexing.from_channel f;;

let parse_SMT file = SMTparser.main SMTlexer.token (lexbuf_SMT file);;


(* 
Ces fonctions ont pour but des créer les littéraux qui sont tous une égalité
(3 = 6 par exemple). La fonction egal sert juste si deux littéraux sont les 
mêmes. La fonction appartient l'utilise pour savoir si le littéral a déja 
été attribué. 
*)

let egal el1 el2=
(first el1 = first el2) && (second el2 = second el1) || (first el1 = second el2) && (first el2 = second el1);;

let appartient liclau el nblitc= 
let rec app lclau cpt=match lclau with
| [] -> 0
| t :: q -> if (egal t el)
	then cpt
	else app q (cpt-1)
	in app (!liclau) (!nblitc)
;;

(* Les fonctions suivantes manipulent beaucoup de référence car je n'ai pas trouvé
comment rajouter une ligne au début d'un fichier après l'avoir écrit. Le nombre de
clauses et le nombres de littéraux sont des référence car ils sont créés au fur et à
mesure que la lecture de leur apparition.La fonction cree1clause sert en fait à gérer
les littéraux. liclau est la liste des littéraux créés jusqu à présent. La fonction
appartient regarde alors si le littéraux (t1,t2) a déja été créé et le crée sinon.
rep est exactement ce qu'on écrira dans le fichier d'entrée de DPLL.
*) 

let cree1clause entree (liclau: (int* int) list ref) nblitc rep nbmax= 
let t1=(List.hd entree) and q1= (List.tl entree) in
let t2=(List.hd q1) and q2= (List.tl q1) in
let nbl = ref (appartient liclau (abs t1,abs t2) nblitc) in
if (abs t1< abs t2)
then (if (abs t2> !nbmax)
	then nbmax:= abs t2)
else (if (abs t1> !nbmax)
	then nbmax:=abs t1);
if (!nbl = 0)
	then ( liclau := (abs t1,abs t2) :: (!liclau);
		nblitc := !nblitc +1;
		nbl := !nblitc);
if (abs t1= t1)
	then (  rep := (!nbl::!rep);
      		q2)
	else (  rep := (-(!nbl)::!rep);
      		q2)
;;

(* La fonction creeclauses se sert de la précédente pour parcourir toute l'entrée *)


let rec creeclauses entree liclau nblitc nombclau rep nbmax= match entree with
| [] -> ()
| t :: q when t= 0 -> ( rep:= (0 :: !rep);
			nombclau := (!nombclau + 1);
			creeclauses q liclau nblitc nombclau rep nbmax)
| _  -> let l=cree1clause entree liclau nblitc rep nbmax in
	creeclauses l liclau nblitc nombclau rep nbmax
;;


let rec print_lp licla = match licla with
| [] -> ()
| t::q -> (print_int (first t);
	print_string " ";
	print_int (second t);
	print_endline "";
	print_lp q)
;;

(* La fonction creefichcnf crée le fichier cnf. La fonction auxiliaire écrit les clauses.
L'appel rajoute l'en tête du fichier cnf.
*)

let creefichcnf f lis nblitc nombclau =
	let rec ecriclau lis = match lis with
	| [] -> ()
	| t ::q -> if (t=0)
		then (output_string f (string_of_int t);
			output_string f "\n";
			ecriclau q)
		else (output_string f (string_of_int t);
			output_string f " ";
			ecriclau q)
	in
	( output_string f "p cnf ";
	  output_string f (string_of_int (!nblitc));
	  output_string f " ";
	  output_string f (string_of_int (!nombclau));
          output_string f "\n";
	  ecriclau lis)
;;

let creetab liste nombclau nblitc=
let rep = Array.create (!nblitc+1) (0,0) in
let rec remp ind li= match li with
	| [] ->  ()
	| t:: q -> (rep.(ind) <- t;remp (ind-1) q)
in (remp (!nblitc) liste;rep)
;;


let affichetab tab =
for i=1 to (Array.length tab-1)
do
(
print_int i;
print_string ": ";
print_int (first tab.(i));
print_string " ";
print_int (second tab.(i));
print_endline "")
done
;;


(*
let _=
let entree = parse_SMT Sys.argv.(1) in
affiche_entree_brute (entree);
if (egal (3,2) (4,2) ) then print_endline "titi" else print_endline "toto";
let nblitc =ref 0 and liclau = ref [] and f=open_out "smt.cnf" and nombclau = ref 0 and rep = ref [0] in
creeclauses entree liclau nblitc nombclau rep;
print_lp (!liclau);
print_int (!nblitc);
print_endline "";
print_int (!nombclau);
print_endline "";
affiche_entree_brute (!rep);
creefichcnf f (List.tl !rep) nblitc nombclau;
close_out f;
let tab=creetab (!liclau) nombclau nblitc in (print_endline "rer";affichetab tab);;
*)
