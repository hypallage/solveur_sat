exception Erreur;;
open Types;;
		
let lexbuf_graphe file=let f=open_in file in
Lexing.from_channel f;;

let parse file = Parser_graphe.main Lexer_graphe.token (lexbuf_graphe file);;

let affiche_entree_brute l = 
  let rec aux l = 
  begin
    match l with 
      [] -> print_string "] \n"
    |t::q -> begin 
             print_int t ;
             print_string "," ;
             aux q ;
             end
  end
  in
  print_string "[" ;
  aux l
;;

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

	
let ecrire nb_sommet k_color nb_arrete liste_arrete fichier=
ecr_sommet nb_sommet k_color fichier;
ecr_arrete nb_sommet k_color nb_arrete liste_arrete fichier
;;

let cree_formule k_color file =
let l = parse file in
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





