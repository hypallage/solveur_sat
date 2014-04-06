(*
On liste ici toutes les données qu'on a à notre disposition.
nb_var: int array qui contient le nombre de variable par clauses.
nb_cl_f: int array qui contient le nombre de clause vrai dans l'affectation courante.
nb_lit_f: int array qui contient le nombre de littéraux faux dans l'affectation courante.

tab_act_pos: int list array contient le numero des clauses actives où les variables apparaissent positivement. 
tab_act_neg: int list array contient le numero des clauses actives où les variables apparaisent négativement.
tab_des_neg: int list array contient le numero des clauses inactives où les variables apparaissent négativement.
tab_des_pos: int list array contient le numero des clauses inactives où les variables apparaissent positivement.

tab_clau : int list array contient l'ensemble des clauses.

(int * int list) list sera la liste des paris et déductions qui en ont découlés.
*)
(*
Nos types qui ne sont pas standard.
*)
type clause = int list;;
type val_verite= Vrai | Faux | Inconnu;;
type affectation= val_verite array;;

(*
Deux fonctions de base qui seront utilisés par presque toutes les fonctions.
*)
let affect tableaff numero value=
tableaff.(numero)<-value;;

let rec suprime l numero=match l with
| [] -> failwith "Pas de supression possible"
| t::q -> (if t=numero
	then q
	else t::(suprime q numero))
;;
