exception Erreur;; (*se déclenche quand on a qu'un sommet et qu'on veut écrire une arrête*)
open Types;;

(* Ceci apelle le lexeur et le parseur*) 
let lexbuf_graphe file=let f=open_in file in
Lexing.from_channel f;;
let parse_graphe file = Parser_graphe.main Lexer_graphe.token (lexbuf_graphe file) ;;

(* La fonction ecr_sommet sert à ecrire la clause qui force un sommet à être colorié dans
une des k couleurs. *)
let ecr_sommet nb_sommet k_color fichier=
let rec cl_so ind=
if (ind=nb_sommet+1)
then ()
else (for i=1 to k_color
	do
	(output_string fichier (string_of_int (i+k_color*(ind-1)));
	output_string fichier " ")
	done;
	output_string fichier " ";
	output_string fichier "0\n";
	cl_so (ind+1))
in cl_so 1
;;

(* La fonction ecr_arrrete set à écrire les clauses qui forcent que deux sommet reliés
par cet arrêtes ne soit pas de la même couleur. *)

let rec ecr_arrete nb_sommet k_color nb_arrete liste_arrete fichier=
match liste_arrete with
| [] -> ()
| t::q -> match q with
	| [] -> raise Erreur
	| d::s -> for i=1 to k_color
			do
			output_string fichier (string_of_int (-(i+k_color*(t-1))));
			output_string fichier " ";
			output_string fichier (string_of_int (-(i+k_color*(d-1))));
			output_string fichier " 0\n"
			done;
			ecr_arrete nb_sommet k_color nb_arrete s fichier
;;

(* La fonction ci dessous apelle juste les deux précédentes *)

let ecrire nb_sommet k_color nb_arrete liste_arrete fichier=
ecr_sommet nb_sommet k_color fichier;
ecr_arrete nb_sommet k_color nb_arrete liste_arrete fichier
;;

(* la fonction cree_formule crée l'ensemble des clauses et crée un fichier .cnf qui
est exatement le probleme SAT équivalent au problème de coloriage. *)

let cree_formule k_color file =
let l = parse_graphe file in
let nb_sommet = List.hd l 
and nb_arrete = List.hd (List.tl  l)
and liste_arrete = List.tl (List.tl l) in
let f=open_out "ex_graphe.cnf" in
output_string f "p cnf ";
output_string f (string_of_int (nb_sommet*k_color));
output_string f " ";
output_string f (string_of_int (nb_sommet+nb_arrete*k_color));
output_string f "\n";
ecrire nb_sommet k_color nb_arrete liste_arrete f;
close_out f;
(nb_sommet,liste_arrete)
;;

(* la fonction ecrit_dot ecrit dans un fichier la coloration trouvé par DPLL. Il n'y a qu'une
couleur par sommet car colorier un sommet ne vérifie qu'une clause. Ainsi DPLL n'est pas tenté
de colorier un sommet de deux couleur différentes. *)

let ecrit_dot tab_aff k_color nb_sommet liste_arrete=
let f = open_out "ex_graphe.dot" in
	let rec ecr_som ind =
		if (ind != nb_sommet +1)
			then (for i=1 to k_color
			do
				if (tab_aff.((ind-1)*k_color+i)=Vrai)
				then (
				output_string f "node  [style=filled,color=\" 0.";
		      		output_string f (string_of_int (100/k_color*(i-1)));
		      		output_string f " 1.0 1.0\"];\n";
				output_string f (string_of_int ind);
		      		output_string f "\n";
		      		)
				else ()
			done;
			ecr_som (ind+1))
		else ()
	in let rec ecr_arr liste=match liste with
	| [] -> ()
	| t::q -> (match q with
		| [] -> raise Erreur
		| d::s -> output_string f (string_of_int t);
		          output_string f " -- ";
			  output_string f (string_of_int d);
		          output_string f "\n";
			  ecr_arr s)
	
	in
output_string f "graph graphe_rep {\n";
ecr_som 1;
ecr_arr liste_arrete;
output_string f "}";
close_out f
;;

	







