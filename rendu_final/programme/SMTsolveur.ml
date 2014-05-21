open Types;;

(* On programme içi une structure d'union-find pour vérifier si une affectation est correcte. Pour
cela, on met un tableau de représentant appelés tab_rep et un tableau de la taille des structures 
tab_tai qui n'est utile que si c'est effectivement le représentant d'une structure. La version
ci-dessous est la version fine dont l'analyse de Tarjan nous dit que la complexité amortie est en "O(1)". *)

(* fonction d'union qui prend le tableau des représentant le tableau des taille et les 2 représentant *)
let union tab_rep tab_tai rep1 rep2=
if (tab_tai.(rep1)<tab_tai.(rep2))
then (tab_rep.(rep1)<-rep2;tab_tai.(rep2)<-tab_tai.(rep2)+1)
else (tab_rep.(rep2)<-rep1;tab_tai.(rep1)<-tab_tai.(rep1)+1)
;;

(*la fonction find se fait en 2 étapes. La première consiste à remonter la branche pour trouver le 
représentant. La deuxième affecte le représentant à tout les variables parcourus*)

let rec find_down tab_rep el liste=
if (tab_rep.(el)=el)
then (el::liste)
else (find_down tab_rep tab_rep.(el) (el::liste))
;;


let rec find_up tab_rep liste rep= match liste with
| [] -> ()
| t::q -> (tab_rep.(t)<-rep; find_up tab_rep q rep)
;;

let find tab_rep el=
let l=find_down tab_rep el [] in
find_up tab_rep (List.tl l) (List.hd l);(List.hd l)
;;

(* Le code ci-dessous sert à créer et gerer les clauses piur le solveur de théorie *)

let unifie tab_rep tab_tai tab_eq_clau_the tab_ver=
for i=1 to (Array.length tab_ver - 1) do
	(if (tab_ver.(i) = Vrai)
	then (let a=find tab_rep (first tab_eq_clau_the.(i)) in
	    let b=find tab_rep (second tab_eq_clau_the.(i)) in
 		(if (a <> b)
 		then union tab_rep tab_tai a b)
	     )
	)
done
;;

let verifie tab_rep tab_eq_clau_the tab_ver=
let rec verif ind=
	if (ind =Array.length tab_ver)
	then true
	else (if (tab_ver.(ind) = Faux)
		then (let a=find tab_rep (first tab_eq_clau_the.(ind))
 			and b=find tab_rep (second tab_eq_clau_the.(ind)) in
 			(if (a <> b)
 			then verif (ind+1)
			else false
			)
		      )
		 else verif (ind+1))
in verif 1
;;

let complete_ver tab_rep tab_eq_clau_the tab_ver=
let rec complete ind=
	if (ind =Array.length tab_ver)
	then ()
	else (if (tab_ver.(ind) = Inconnu)
		then (let a=find tab_rep (first tab_eq_clau_the.(ind))
 			and b=find tab_rep (second tab_eq_clau_the.(ind)) in
 			(if (a <> b)
 			then (print_int (first tab_eq_clau_the.(ind)); print_string " != "; 
				print_int (second tab_eq_clau_the.(ind));print_endline " ";tab_ver.(ind)<-Faux;complete (ind+1))
			else (print_int (first tab_eq_clau_the.(ind)); print_string " = "; 
				print_int (second tab_eq_clau_the.(ind));print_endline " ";tab_ver.(ind)<-Vrai;complete (ind+1)))
		     )
		else if (tab_ver.(ind) = Vrai)
			then (print_int (first tab_eq_clau_the.(ind)); print_string " = "; 
				print_int (second tab_eq_clau_the.(ind));print_endline " ";complete (ind+1))
			else (print_int (first tab_eq_clau_the.(ind)); print_string " != "; 
				print_int (second tab_eq_clau_the.(ind));print_endline " ";complete (ind+1))
	      )
in complete 1
;;

let resoud_th tab_rep tab_tai tab_eq_clau_the tab_ver=
unifie tab_rep tab_tai tab_eq_clau_the tab_ver;
if (verifie tab_rep tab_eq_clau_the tab_ver)
then (complete_ver tab_rep tab_eq_clau_the tab_ver;true)
else false
;;

let construit_clau tab_ver=
let rec constr ind rep=
	if (ind =Array.length tab_ver)
	then rep
	else (if (tab_ver.(ind) = Vrai)
		then constr (ind+1) (ind::rep)
		else (if (tab_ver.(ind) = Faux)
 			then constr (ind+1) (-ind::rep)
			else constr (ind+1) rep)
		  )
		
in constr 1 []
;;

let metnbtab tab=
for i=0 to (Array.length tab -1)
do
tab.(i) <- i;
done
;;

(*
let tab_taille =Array.create (!nbmax) 0 
and tab_rep = Array.create (!nbmax) 0 in
(metnbtab tab_rep;
let t=[|0;1;2;3;4;5;6;7;8|];;
let tai=[|0;1;1;1;1;1;1;1;1|];;
let ver=[|Faux;Vrai;Vrai;Vrai;Vrai;Vrai;Vrai;Faux;Faux;Faux;Faux;Faux;Inconnu;Inconnu;Inconnu;Inconnu|];;

let _=unifie t tai tab_eq ver;
print_endline "arbre";
print_int t.(1);print_endline " ";
print_int t.(2);print_endline " ";
print_int t.(3);print_endline " ";
print_int t.(4);print_endline " ";
print_int t.(5);print_endline " ";
print_int t.(6);print_endline " ";
print_int t.(7);print_endline " ";
print_int t.(8);print_endline " ";
if (verifie t tab_eq ver)
then print_endline "c bon"
else print_endline "rate";
complete_ver t tab_eq ver
;;

let _= union t tai 1 6; print_int t.(6);print_int t.(1);print_int tai.(6); print_int tai.(1);print_endline " ";
find t 4;print_int t.(1);print_int t.(2);print_int t.(3);print_int t.(4);print_int t.(5);print_int t.(6);print_int t.(7);print_int t.(8);print_int (first (1,2));;
*)





