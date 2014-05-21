open Types;;
open Atomes;;

(* Fonction de pari sans heuristiques *)
(*
La fonction de pari cherche la prochaine variable sur laquelle parier. c'est
la première à Inconnu.Si on en trouve pas, alors on a oublié de desactiver
une clause.
*)

let pari tab_aff nbvar nblitf tabclau t_cl_v tab_act_pos tab_act_neg =
   let rec p ind = if (ind = Array.length tab_aff)
      then raise Clausenondes
      else match tab_aff.(ind) with
         | Inconnu -> ind
         | _ -> p (ind+1)
   in p 1
;; 

(* Heuristique rand*)
(* On choisit une variable au hasard parmi celles qui ne sont pas encore 
   affectees *)

let les_var_inconnues tab_aff = 
  let l = ref [] 
  and n = Array.length tab_aff in 
  for i = 1 to (n-1) do
  if (tab_aff.(i) = Inconnu) then
  l := i::!l ;
  done;
  !l ;;

let rec k_ieme l k = 
  match l with
  [] -> failwith " pas de k-ieme element"
  |t::q -> if k = 1 then t
  else k_ieme q (k-1) ;;

let pari_rand tab_aff nbvar nblitf tabclau t_cl_v tab_act_pos tab_act_neg = 
  let var_inconnues = les_var_inconnues tab_aff in 
  let n = List.length var_inconnues in
  let rand = (Random.int n) + 1 in 
  k_ieme var_inconnues rand ;;
(*Heuristique MOMS sans tenir compte des variables fausses*)
(* La fonction cherche les clauses non satisfaites de tailles minimum et la taille minimum*)
let clautaillmin nbvar t_cl_v =
	let rec clatm1 numclau min rep =
		if (numclau = Array.length t_cl_v)
			then rep
			else (match t_cl_v.(numclau) with
			| Vrai -> clatm1 (numclau+1) min rep
			| _ -> 	if (min=0)
				then clatm1 (numclau+1) nbvar.(numclau) [numclau]
				else 	if (nbvar.(numclau)>min)
					then clatm1 (numclau+1) min rep
					else 	(if (nbvar.(numclau)<min)
						then clatm1 (numclau+1) nbvar.(numclau) [numclau]
						else clatm1 (numclau+1) min (numclau::rep)))
	in clatm1 1 0 []
;;

(*La fonction remplit le tableau d'occurence des variables dans les clauses trouvé par la fonction
précédent*)
let rec remp tab_pos tab_neg clau=match clau with
|[] -> ()
|t::q -> if t>0
	then (tab_pos.(t) <- (tab_pos.(t) +1);remp tab_pos tab_neg q)
	else (tab_neg.(abs t) <- (tab_neg.(abs t) +1);remp tab_pos tab_neg q)
;;
 
let occur_liste liste tabclau tab_act_pos=
let tab_occur_pos=Array.create (Array.length tab_act_pos) 0 
and tab_occur_neg=Array.create (Array.length tab_act_pos) 0 in
	let rec ocl lis tab_pos tab_neg=match lis with
	| [] -> (tab_pos,tab_neg)
	| t::q -> remp tab_pos tab_neg (tabclau.(t));ocl q tab_pos tab_neg
	in ocl liste tab_occur_pos tab_occur_neg
;; 

let pari_moms tab_aff nbvar nblitf tabclau t_cl_v tab_act_pos tab_act_neg=
let clmins = clautaillmin nbvar t_cl_v in
let (pos,neg)= occur_liste clmins tabclau tab_act_pos in
	let rec p numvar max pc= if (numvar= Array.length tab_aff)
				then pc
				else match tab_aff.(numvar) with
					| Vrai ->  p (numvar+1) max pc
					| Faux ->  p (numvar+1) max pc
					| Inconnu -> if pos.(numvar) > neg.(numvar)
							then (if pos.(numvar) > max
								then p (numvar+1) pos.(numvar) numvar
								else p (numvar+1) max pc)
							else (if neg.(numvar) > max
								then p (numvar+1) neg.(numvar) (-numvar)
								else p (numvar+1) max pc)
in (p 0 0 0 )
;;


(* Heuristique MOMS en prenant les littéraux faux dans une clause *)
let clautaillmin_2 nbvar nblitf t_cl_v =
	let rec clatm numclau min rep =
		if (numclau = Array.length t_cl_v)
			then rep
			else (match t_cl_v.(numclau) with
			| Vrai -> clatm (numclau+1) min rep
			| _ -> 	if (min=0)
				then clatm (numclau+1) (nbvar.(numclau)-nblitf.(numclau)) [numclau]
				else 	if ((nbvar.(numclau)-nblitf.(numclau))>min)
					then clatm (numclau+1) min rep
					else 	(if (nbvar.(numclau)-nblitf.(numclau))<min
						then clatm (numclau+1) (nbvar.(numclau)-nblitf.(numclau)) [numclau]
						else clatm (numclau+1) min (numclau::rep)))
	in clatm 1 0 []
;;
 
let pari_moms2 tab_aff nbvar nblitf tabclau t_cl_v tab_act_pos tab_act_neg=
let clmins = clautaillmin_2 nbvar nblitf t_cl_v in
let (pos,neg)= occur_liste clmins tabclau tab_act_pos in
	let rec p numvar max pc= if (numvar= Array.length tab_aff)
				then pc
				else match tab_aff.(numvar) with
					| Vrai ->  p (numvar+1) max pc
					| Faux ->  p (numvar+1) max pc
					| Inconnu -> if pos.(numvar) > neg.(numvar)
							then (if pos.(numvar) > max
								then p (numvar+1) pos.(numvar) numvar
								else p (numvar+1) max pc)
							else (if neg.(numvar) > max
								then p (numvar+1) neg.(numvar) (-numvar)
								else p (numvar+1) max pc)
in p 1 0 0
;;

(* Heuristique DLIS *)
let pari_dlis tab_aff nbvar nblitf tabclau t_cl_v tab_act_pos tab_act_neg=
	let rec p numvar max pc= if (numvar= Array.length tab_aff)
				then pc
				else match tab_aff.(numvar) with
					| Vrai ->  p (numvar+1) max pc
					| Faux ->  p (numvar+1) max pc
					| Inconnu -> let pos = List.length tab_act_pos.(numvar)
							and neg = List.length tab_act_neg.(numvar) in
						if (pos > neg)
							then (if pos > max
								then p (numvar+1) pos numvar
								else p (numvar+1) max pc)
							else (if neg > max
								then p (numvar+1) neg (-numvar)
								else p (numvar+1) max pc)
in p 1 0 0
;;




(*
La fonction diminue le nombre de littéraux faux dans la clause.
*)
let rec dimlitfliclau nb_lit_f liclau =match liclau with
   | [] -> ()
   | t :: q -> affect nb_lit_f t (nb_lit_f.(t) - 1);
      dimlitfliclau nb_lit_f q
;;


let evalue var valeur = 
  match (var,valeur) with
  (_,Inconnu) -> Inconnu
 |_ -> if var > 0 then valeur
       else if valeur = Vrai then Faux
                             else Vrai 
;;
 

let rec valeur_de_clause c affectation = 
  match c with
  [] -> Inconnu
 | var::q -> let valeur = evalue var (affectation.(abs var)) in 
    match valeur with 
    Vrai -> Vrai 
   |_ -> valeur_de_clause q affectation    
;;

(* Lors du back-track, on remet une variable a inconnu,
   if faut alors reevaluer les clauses pour savoir lesquelles
   repasser a Inconnu *)

let rec recupere_quelles_clauses l affectation tabclau tap tan tdp tdn t_cl_v= 
   match l with 
   [] -> ()
  |i::q -> let c = tabclau.(i) in 
           match valeur_de_clause c affectation with 
	     Faux -> raise Probleme
          |Vrai -> recupere_quelles_clauses q affectation tabclau tap tan tdp tdn t_cl_v
          |Inconnu ->
	  t_cl_v.(i) <-Inconnu;
          des_clau c tdp tdn tap tan i;
          recupere_quelles_clauses q affectation tabclau tap tan tdp tdn t_cl_v;
;;


let rec retour_sur deductions tap tan tdp tdn tab_aff tabclau nb_lit_f t_cl_v= 
  match deductions with
   [] -> ()
  |var::q -> tab_aff.(abs var) <- Inconnu ;
             if var > 0 then
             begin
             recupere_quelles_clauses tdp.(var) tab_aff tabclau tap tan tdp tdn t_cl_v;
             dimlitfliclau nb_lit_f tdn.(var);
	     dimlitfliclau nb_lit_f tan.(var)
             end
             else
             begin 
             recupere_quelles_clauses tdn.(-var) tab_aff tabclau tap tan tdp tdn t_cl_v;
             dimlitfliclau nb_lit_f tdp.(-var);
	     dimlitfliclau nb_lit_f tap.(-var)
             end ;
             retour_sur q tap tan tdp tdn tab_aff tabclau nb_lit_f t_cl_v;;

