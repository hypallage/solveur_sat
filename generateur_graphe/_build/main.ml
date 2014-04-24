(* Generer des graphe *)

let cree_fich f nb_sommets tab=
for i=1 to nb_sommets
do
	for j=1 to (i-1)
	do
	if (tab.(i*nb_sommets+j) =0)
		then ()
		else (output_string f "e ";
			output_string f (string_of_int i);
  			output_string f " ";
			output_string f (string_of_int j);
  			output_string f "\n")
	done
done
;;

let cree_tab nb_sommets proba=
let tab=Array.create ((nb_sommets+1)*(nb_sommets+1)) 0 in
let nb_arrete=ref 0 in
for i=1 to nb_sommets
do
	for j=1 to (i-1)
	do
	if ((Random.int 100) > proba)
		then ()
		else (nb_arrete := !nb_arrete +1;tab.(i*nb_sommets+j)<-1)
	done
done;
(tab,!nb_arrete)
;;

let _ =
  Random.self_init () ;
  let nb_sommets = int_of_string (Sys.argv.(1)) 
  and proba = int_of_string (Sys.argv.(2))  in 
  let f=open_out "ex.col" in
  output_string f "p edge ";
  output_string f (string_of_int nb_sommets);
  output_string f " ";
  let (a,b)=cree_tab nb_sommets proba in
  output_string f (string_of_int b); 
  output_string f "\n";
  cree_fich f nb_sommets a;
  close_out f ;; 

  
 

