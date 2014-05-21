(* Generer des clique de taille Sys.argv.(1) et une clause d'au plus p littéral qui parlent de
manière fausse de la clique (2!=3 par exemple). On met la clause 1 != n+1 pour que le programme
sans heuristique de pari parie sur 1 = n+1 avant les autres. *)

let creeclique f n=
for i=2 to n
do
output_string f (string_of_int 1);
output_string f " = ";
output_string f (string_of_int i);
output_string f "\n"
done
;;

let derclause f n p =
output_string f (string_of_int 1);
	output_string f " != ";
	output_string f (string_of_int (n+1));
for i=1 to p
do
let a=Random.int (n-1)
and b=Random.int (n-1) in
if (a!=b)
then (output_string f " \\/ ";
	output_string f (string_of_int (a+2));
	output_string f " != ";
	output_string f (string_of_int (b+2)));
done
;;


let _ = 
Random.self_init () ;
let f=open_out Sys.argv.(3) in
creeclique f (int_of_string Sys.argv.(1));
derclause f (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2))
;;
 

  
 

