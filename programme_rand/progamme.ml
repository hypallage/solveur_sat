(*/// Les fonctions de pretraitement ///*)

(*/// Les tests ///*)

let c0 = [2;4] ;;
let c1 = [] ;;
let tauto = ref 0 ;;
let c2 = met_dans_clause 3 c1 tauto ;;
let c3 = met_dans_clause (-1) c2 tauto ;;
let c4 = met_dans_clause (-2) c3 tauto ;;
let c5 = met_dans_clause (-1) c4 tauto ;;
let c6 = met_dans_clause 1 c5 tauto ;;
!tauto ;;
(* met_dans_clause ok *)
inclusion c2 c4 ;;
let a = [-1;3] ;;
let b = [3] ;;
inclusion a b ;;
let entree = [2;3;1;0;-1;2;0;2;-2;0] ;;
let l = [1;0;-1;2;0;2;2;0] ;;
initialise l 2 3 ;;
let f = construire_formule entree ;; 
init_les_tab [|[];[-1;2];[-2]|] 2 2 ;; 
let e = [4;8;1;2;-3;0;3;-1;2;0;1;2;3;0;4;3;2;0;1;-2;3;0;-4;-1;3;0;-4;-1;3;1;0;-4;-1;3;2;0] ;;
let f1 = construire_formule e ;;
init_les_tab (snd f1) 4 6 ;;
let tab = [|Inconnu;Vrai;Vrai;Faux|] ;;
affiche tab 3 ;;

(*/// L'algo final ///*)

(* 1) init les structures de donn�es annexes 
   2) tant nb_clause_cachees non nul faire
      a) rechercher les clauses unitaires et les litteraux � une seule polarite
      b) si on n'en trouve pas, faire un pari sur une variable 
      c) recommencer � l'etape a)
   3) si on a un conflit ( clause vide ), on remonte sur le dernier pari fait,
      puis on reprend l'etape 2 
   4) afficher la solution : s SATISFIABLE + valuation si admet une solution
                             s UNSATISFIABLE sinon. *)

let affect tableaff numero val=
tableaff.(numero)<-val;;

let rec suprime l numero=match l with
| [] -> failwith "Pas de supression possible"
| t::q -> (if t=numero
	then q
	else t::(suprime q numero))
;;


(*/// Propagation ///*)

(*
La fonction polunique sert a regarder si on a plus qu'une polarisation pour une variable 
donnee. La fonction cherpoluni recherche si on a cree une polarisation unique(ou plus de 
variable mis � vrai par defaut). Bien sur on regarde ceci que pour les variables non 
affectees.
*)
let polunique tab_pos tab_neg nb_var = match tab_pos.(nb_var), tab_neg.(nb_var) with
   | [], _ -> false
   | _, [] -> true
   | _ -> raise poldouble;;


let cherpoluni tab_act_pos tab_act_neg tab_aff =
   let rec chpu n =
      if (n = vect_length tab_act_pos)
      then None
      else match tab_aff.(n) with
         | Inconnu -> (try if (polunique tab_act_pos tab_act_neg n)
                  then Some (n)
                  else Some (- n)
               with
               | poldouble -> chpu (n + 1))
         | _ -> chpu (n + 1)
   in chpu 1
;;

(*
La fonction affecness sert a regarder si on peut faire une deduction de 
notre pari precedent c'est a dire si il ne reste qu'une variable � Inconnu dans une
clause sans que celle-ci soit vraie. On prend le tableau des clauses vraies.
*)
let affecness nb_var_f nb_var tab_cl_v =
   let rec affness ind =
      if (ind = vect_length nb_var)
      then None
      else match tab_cl_v.(ind) with
         | Inconnu -> (if (nb_var.(ind) = nb_var_f.(ind) + 1)
               then Some (ind)
               else affness (ind + 1))
         | Vrai -> affness (ind + 1)
         | _ -> raise probleme
   in affness 1
;;


(*
Cette fonction sert a trouver l'affectation necessaire pour valider une clause. Un pre-requis
est souhaitable : il ne doit avoir qu'une seule variable � Inconnu (celle qu'on veut affecter) et
les autres doivent etre a faux. Si la clause a une variable a vrai, alors on renvoie l'execption 
clausevrai (car aucune affectation n'est utile). A l'inverse, si tous les litteraux sont faux dans 
l'affectation courante, on renvoie insatisfiable.
*)


let rec trouvaff clause (tab_aff: affectation) = match clause with
   | [] -> raise insatisfiable
   | t :: q -> match tab_aff.(abs t) with
      | Faux -> (if (abs t <> t)
            then raise clausevrai
            else trouvaff q tab_aff)
      | Inconnu -> t
      | Vrai -> (if (abs t = t)
            then raise clausevrai
            else trouvaff q tab_aff)

;;


(*
Ces deux fonctions servent a changer l'activite d'une clause. La premiere met la variable var
� l'etat inverse(desactive si elle etait activee et inversement). On met pour cela le tableau
ou la variable apparait en premier. La deuxieme fonction fait ceci pour toutes les variables 
de la clause.
*)


let des_var tab_a tab_d (nbclau:int) var=
(affect tab_a var (suprime tab_a.(var) nbclau);
            affect tab_d var (nbclau :: tab_d.(var)))
            ;;

let rec des_clau clause tab_a_pos tab_a_neg tab_d_pos tab_d_neg (nbclau: int) = match clause with
   | [] -> ()
   | t :: q -> if (abs t = t)
      then (des_var tab_a_pos tab_d_pos nbclau t;
            des_clau q tab_a_pos tab_a_neg tab_d_pos tab_d_neg nbclau)
      else (des_var tab_a_neg tab_d_neg nbclau (abs t);
            des_clau q tab_a_pos tab_a_neg tab_d_pos tab_d_neg nbclau)
;;

(*
Cette fonction sert a voir si il y a un conflit c'est � dire si tous les litteraux d'une clause
sont faux. On leve alors conflit et on lancera le backtracking.
*)

let conflitoupas nb_var nb_var_f tab_cl_v =
   let rec coupas ind =
      if (ind = vect_length nb_var)
      then ()
      else match tab_cl_v.(ind) with
         | Inconnu -> (if (nb_var.(ind) = nb_var_f.(ind))
               then raise conflit
               else coupas (ind + 1))
         | Vrai -> coupas (ind + 1)
         | _ -> raise probleme
   in coupas 1
;;

(*
Les fonctions suivantes sont celles qui font la propagation. metliclauvrai met une liste de clause a 
vrai et donc les desactive. Elle travaillera sur la liste des clauses qu'on satisfait avec notre
pari. La fonction auglitfliclau augmente le nombre de litteraux faux dans les clauses. Elle 
travaillera au contraire sur les clauses dont on met un litteral � faux par notre pari.
*)

let rec metliclauvrai tap tan tdp tdn linuclau tabclau tab_cl_v = match linuclau with
   | [] -> ()
   | t :: q -> (tab_cl_v.(t)<- Vrai;
         des_clau tabclau.(t) tap tan tdp tdn t;
         metliclauvrai tap tan tdp tdn q tabclau tab_cl_v)
;;

let rec auglitfliclau nb_lit_f liclau = match liclau with
   | [] -> ()
   | t :: q -> affect nb_lit_f t (nb_lit_f.(t) + 1);
      auglitfliclau nb_lit_f q
;;

(*/// Les paris et le back-track ///*)

let pari tab_aff =
   let rec p ind = if (ind = vect_length tab_aff)
      then raise clausenondes
      else match tab_aff.(ind) with
         | Inconnu -> ind
         | _ -> p (ind+1)
   in p 1
;;

(*//// La partie backtrack ////*)

(* On a fait une liste de paris sur des variables, 
  qui ont entraine la liste des consequences associees *)


(* on ne precise pas la valeur du pari, elle est contenue dans la 
   solution courante *)

let init_pari () = [(0,[])] ;; (* ou (0,[]) si jamais on doit backtracker
 avec un seul pari ... *)
let nb_var = 3 ;;

let inverse_valeur x sol = 
  let b = sol.(x) in 
  match b with
  Vrai -> sol.(x) <- Faux
| Faux -> sol.(x) <- Vrai 
| Inconnu -> failwith " impossible : un pari a ete fait dessus " ;;

let annule_consequences csqcs = () ;;
(* il faut remodifier toutes les structures de donnees mises en place *)

let gere_contradiction les_paris sol = 
  match les_paris with
  [] -> failwith " ne doit pas arriver "
 |(0,_)::_ -> failwith " insatisfiable " (* contradiction au plus haut niveau *)
 |(x,csqcs)::q -> inverse_valeur x sol ;
                  annule_consequences csqcs ; 
              match q with
              [] -> failwith " ne doit pas arriver car x <> 0" 
            |(y,l)::q2 -> (y,x::l)::q2 ;; 


(* L'algo *)

(*let est_satisfiable cl_vr = 
  let n = vect_length cl_vr in 
  try(
  for i = 1 to (n-1) do 
  if cl_vr.(i) = Inconnu then raise Exit
  done ;
  true)
  with Exit -> false ;;

let ajoute_consequence


let etape f nb_clause nb_var affectation tap tan tdp tdn nb_lit nb_faux cl_vr paris = 
  (* premiere etape : formule satisfiable ? *)
  if est_satisfiable cl_vr then raise solvable
  else 
  (* deuxieme etape : y a t-il un conflit ? *)
  conflitoupas nb_lit nb_faux cl_vr ;
  (* etape 3 : recherche d'une polarite unique pour une variable *)
  let res1 = cherpoluni tap tan affectation in 
  match res1 with
    Some n -> affectation.(n) <- Vrai ;
              metliclauvrai tap tan tdp tdn tap.(x) f cl_v;
              let (pari,csqcs) = hd !paris in
              paris := (pari,x::csqcs)::(tl !paris) ;
   | None  -> 
  (* etape 4 : recherche d'une clause reduite a un litteral *)
  let res2 = affecness nb_faux nb_lit cl_vr in
  match res2 with
    Some n -> affectation.(n) <- Vrai ;
              metliclauvrai tap tan tdp tdn tap.(x) f cl_v;
              let (pari,csqcs) = hd !paris in
              paris := (pari,x::csqcs)::(tl !paris) ;
   | None -> let x = pari cl_vr in
      paris := (x,[])::(!paris) (* on ajoute le pari *)
      affectation.(x) <- Vrai ;     (* on affecte *)
      metliclauvrai tap tan tdp tdn (tap.(x)) f cl_v ;;

let solve entree = 
  (* Les initialisations : construction formule, et 
   structures de donnees annexes *)
  let (nb_clause,nb_var,f) = constuire_formule entree in*
  let affectation = init_affectation nb_var 
  and (tap,tan,nb_lit) = init_les_tab f nb_var nb_clauses 
  and tdp = make_vect (nb_var+1) [] 
  and tdn = make_vect (nb_var+1) [] 
  and nb_faux = make_vect (nb_clauses+1) 0 
  and cl_vr = make_vect (nb_clauses+1) 0 
  and paris = ref [(0,[])] in
  (* le debut des boucles *)
  try(
   while(true) do 
   etape f nb_clause nb_var affectation tap tan tdp tdn nb_lit nb_faux cl_vr paris;
   )
   with
   solvable -> affiche affectation nb_var
  |conflit -> 
  |... 
   
   *)

