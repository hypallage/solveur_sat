open Atomes;;
(*
Ce sont les exceptions qu'on utilise. impossible, probleme et clausevrai refl�tent des erreurs �
l'execution. Tandis que poldouble indique juste que on a une cluse avec la variable positive 
et une avec la variable � faux et conflit qu'on doit backtracker.insatisfiable et satisfiable sont la r�ponse de
l'algorithme.
*)
open Types;;
exception Conflit;;
exception Poldouble;;
exception Clausevrai;;
exception Impossible;;
exception Satisfiable;;


exception Insatisfiable;;

(* Prints pour le debuggage *)

let printval valeur = match valeur with
| Vrai -> print_string "Vrai"
| Faux -> print_string "Faux"
| Inconnu -> print_string "Inconnu";;

let print_tab_int t = 
  let n = Array.length t in
  print_string "[" ;
  for i = 1 to (n-2) do 
  print_int t.(i) ;
  print_string ";" ;
  done ;
  print_int t.(n-1) ;
  print_string "] \n" ;;

let print_tab_aff t =
  let n = Array.length t in
  print_string "[" ;
  for i = 1 to (n-2) do 
  printval t.(i) ;
  print_string ";" ;
  done ;
  printval t.(n-1) ;
  print_string "] \n" ;;

let print_list_val l = 
  let rec aux l =  
  match l with
  [] -> print_string "]" ;
 |t::q -> printval t ;
          match q with
          [] -> print_string "]" ;
         |_ -> print_string ";" ; aux q 
  in print_string "[" ; 
  aux l ;;

let print_list_int l = 
  let rec aux l =  
  match l with
  [] -> print_string "]" ;
 |t::q -> print_int t ;
          match q with
          [] -> print_string "]" ;
         |_ -> print_string ";" ; aux q 
  in print_string "[" ; 
  aux l ;;
  
let print_tab_cl_vr t = 
  let n = Array.length t
  and cl_inconnues = ref [] in    
  for i = 1 to (n-1) do 
  if t.(i) = Inconnu then cl_inconnues := i::(!cl_inconnues) ;
  done ;
  print_list_int (!cl_inconnues) ;;

(*
La fonction polunique sert � regarder si on a plus qu'une polarisation pour une variable 
donn�e. La fonction cherpoluni recherche si on a cr�� une polarisation unique(ou plus de 
variable mis � vrai par d�faut).Bien sur on regarde ceci que pour les variables non 
affect�es.
*)

let polunique tab_pos tab_neg var = match tab_pos.(var), tab_neg.(var) with
   | [], _ -> false 
   | _, [] -> true
   | _ -> raise Poldouble
;;


let cherpoluni tab_act_pos tab_act_neg tab_aff =
   let rec chpu n =
      if (n = Array.length tab_act_pos)
      then None
      else match tab_aff.(n) with
         | Inconnu -> (try if (polunique tab_act_pos tab_act_neg n)
                  then ((*print_string "polarite unique " ;*) Some (n))
                  else ((*print_string "polarite unique " ;*) Some (-n))
               with
               | Poldouble -> chpu (n + 1))
         | _ -> chpu (n + 1)
   in chpu 1
;;

(*
La fonction affecness sert � regarder si on peut faire une d�duction de 
notre pari precedent c'est � dire s'il ne reste qu'une variable � Inconnu dans une
clause sans que celle-ci soit vrai. On prend le tableau des clauses vraies.
*)

let affecness nb_var_f nb_var tab_cl_v =
   let rec affness ind =
      if (ind = Array.length nb_var)
      then None
      else match tab_cl_v.(ind) with
         | Inconnu -> (if (nb_var.(ind) = nb_var_f.(ind) + 1)
               then Some (ind)
               else affness (ind + 1))
         | Vrai -> affness (ind + 1)
         | _ -> raise Probleme
   in (*print_string "clauses encore non satisfaites : " ;
      print_tab_cl_vr tab_cl_v ; *)
      affness 1 ;;

(*
Cette fonction sert � trouver l'affectation necessaire pour valider une clause. Un pr�-requis
est souhaitable : il ne doit avoir qu'une seule variable � Inconnu (celle qu'on veut affecter) et
les autres doivent �tre � faux. Si la clause a une variable � vrai, alors on renvoie l'exception 
clausevrai (car aucune affectation n'est utile). A l'inverse, si toutes les litt�raux sont faux 
dans l'affectation courante, on renvoie insatisfiable.
*)

let rec trouvaff clause (tab_aff: affectation) = 
   (*print_string "tab_aff =" ;
   print_tab_aff tab_aff ;
   print_list_int clause ;*)
   match clause with
   | [] -> raise (*Impossible*) Conflit
   | t :: q ->  (*print_string "Affectation necessaire ?" ; 
                print_int t ;
                print_string "\n";
                print_string "La valeur actuelle de cette variable est : " ;
                printval tab_aff.(abs t);
                print_string "\n";*)
                match tab_aff.(abs t) with
      | Faux -> (if (abs t <> t)
            then raise  (* Clausevrai *) Conflit
            else trouvaff q tab_aff)
      | Inconnu -> t
      | Vrai -> (if (abs t = t)
            then raise  (* Clausevrai *) Conflit 
            else trouvaff q tab_aff)

;;



(*
Cette fonction sert � voir si il y a un conflit c'est � dire si toute les litt�raux d'une clause
sont faux. On l�ve alors conflit et on lancera le backtracking.
*)

let conflitoupas nb_var nb_var_f tab_cl_v =
   let rec coupas ind =
      if (ind = Array.length nb_var)
      then ()
      else match tab_cl_v.(ind) with
         | Inconnu -> (if (nb_var.(ind) = nb_var_f.(ind))
               then ((*print_int (ind);*) raise Conflit)
               else coupas (ind + 1))
         | Vrai -> coupas (ind + 1)
         | _ -> raise Probleme
   in coupas 1
;;

(*
La fonction auglitfliclau augmente le nombre de litt�raux faux dans les clauses. Elle 
travaillera au contraire sur les clauses dont on met un litt�ral � faux par notre pari.
*)

let rec metliclauvrai tab_a_pos tab_a_neg tab_d_pos tab_d_neg linuclau tabclau tab_cl_v= match linuclau with
   | [] -> ()
   | t :: q -> (tab_cl_v.(t)<- Vrai;
         des_clau tabclau.(t) tab_a_pos tab_a_neg tab_d_pos tab_d_neg t;
         metliclauvrai tab_a_pos tab_a_neg tab_d_pos tab_d_neg q tabclau tab_cl_v)
;;

let rec auglitfliclau nb_lit_f liclau = match liclau with
   | [] -> ()
   | t :: q -> affect nb_lit_f t (nb_lit_f.(t) + 1);
      auglitfliclau nb_lit_f q
;;

(*
Cette fonction sert a regarder si il y a encore des deductions possibles
sans faire de paris supplementaires. On renvoie None dans le cas ou on a
pas de deductions.
*)

let deduction tab_act_pos tab_act_neg nb_var nb_lit_f t_cl_v t_aff tabclau =
   match cherpoluni tab_act_pos tab_act_neg t_aff with
   | None -> (match affecness nb_lit_f nb_var t_cl_v with
         | None -> None
         | Some t -> Some (trouvaff tabclau.(t) t_aff ))
   | d -> d
;;

(*
La fonction suivante propage un r�sultat en d�sactivant les clauses que le r�sultat
met a vrai et augmente le nombre de litt�raux faux dans la clause.
*)

let propage res tap tan tdp tdn nb_l_f t_cl_v tabclau=
   if (abs res = res)
   then (metliclauvrai tap tan tdp tdn tap.(abs res) t_cl_v tabclau;
         auglitfliclau nb_l_f tan.(abs res);
	 auglitfliclau nb_l_f tdn.(abs res))
   else (metliclauvrai tap tan tdp tdn tan.(abs res) t_cl_v tabclau;
         auglitfliclau nb_l_f tap.(abs res);
	 auglitfliclau nb_l_f tdp.(abs res))
;;


(*
Ce sont les tests pour cette partie

let t=[|Vrai;Vrai;Inconnu;Vrai;Vrai|];;
let tap=[|[];[1;3;5];[1;3;4;2];[3;4;5;6;2];[4]|];;
let tan=[|[];[2;6];[];[1];[6]|];;

cherpoluni tap tan t;;


let tap=[|[];[1;3;5];[1;3;4;2];[3;4;5;6;2];[4]|];;
let tan=[|[];[2;6];[5];[1];[6]|];;
let tdp=[|[];[];[];[];[]|];;
let tdn=[|[];[];[];[];[]|];;

des_var tap tdp 3 2;;
tap;;
tdp;;


let l=[3;-1;2];;
let tap=[|[];[1;3;5];[1;3;4;2];[3;4;5;6;2];[4]|];;
let tan=[|[];[2;6];[5];[1];[6]|];;
let tdp=[|[];[];[];[];[]|];;
let tdn=[|[];[];[];[];[]|];;

des_clau l tap tan tdp tdn 2;;

tap;;
tan;;
tdp;;
tdn;;
des_clau l tdp tdn tap tan 2;;
tap;;
tan;;
tdp;;
tdn;;


let l=[-1;2;-3;-5;-6];;
let t=[|Vrai;Vrai;Faux;Inconnu;Vrai;Vrai;Vrai|];;
trouvaff l t;;

let t=[|Vrai;Vrai;Vrai;Inconnu;Inconnu;Vrai;Vrai|];;
let lit_f =[| 0; 2; 4 ; 4 ; 7 ; 5; 6  |];;
let var = [| 0; 3 ; 5 ; 6 ; 9 ; 7; 8 |];;

affecness lit_f var t;;

let t=[|Vrai;Vrai;Vrai;Inconnu;Inconnu;Vrai;Vrai|];;
let var_f =[| 0; 2; 4 ; 5 ; 8 ; 5; 6  |];;
let var = [| 0; 3 ; 5 ; 5 ; 9 ; 7; 8 |];;

conflitoupas var var_f t;;

let l=[1;5;6];;
let lit_f =[| 0; 2; 4 ; 4 ; 8 ; 5; 6 |];;
auglitfliclau lit_f l;;
lit_f;;

let t=[|Vrai;Faux;Vrai;Vrai;Inconnu;Vrai;Vrai|];;
pari t;;

let t_cl_v= [|Inconnu;Inconnu;Inconnu;Inconnu;Inconnu;Inconnu;Inconnu|];;
let t_aff = [|Inconnu;Inconnu;Inconnu;Inconnu;Inconnu|];;
let nb_v =[|0;3;3;3;3;3;3|];;
let nb_l_f =[|0;0;0;0;0;0;0|];;
let tabclau = [| []; [1;2;-3] ;
						[3;-1;2] ;
						[1;2;3] ;
						[4;3;2] ;
						[1;-2;3] ;
						[-4;-1;3] |];;


let tap=[|[];[1;3;5];[1;3;4;2];[3;4;5;6;2];[4]|];;
let tan=[|[];[2;6];[5];[1];[6]|];;
let tdp=[|[];[];[];[];[]|];;
let tdn=[|[];[];[];[];[]|];;

let lipari = [];;
let paric =0;;
let ded=[];;

deduction tap tan nb_v nb_l_f t_cl_v t_aff;;

pari t_cl_v;;
t_aff.(1) <- Vrai;;

propage 1 tap tan tdp tdn nb_l_f tabclau t_cl_v;;

t_cl_v;;
t_aff;;
tap;;
tan;;
tdp;;
tdn;;

nb_l_f;;

conflitoupas nb_v nb_l_f t_cl_v;;

deduction tap tan nb_v nb_l_f t_cl_v t_aff;;

tap;;
tan;;
tdp;;
tdn;;

cherpoluni tap tan t_aff;;

let ded=[2];;


propage 2 tap tan tdp tdn nb_l_f tabclau t_cl_v;;
t_aff.(2)<-Vrai;;

t_cl_v;;
t_aff;;
tap;;
tan;;
tdp;;
tdn;;

conflitoupas nb_v nb_l_f t_cl_v;;

deduction tap tan nb_v nb_l_f t_cl_v t_aff;;

propage 3 tap tan tdp tdn nb_l_f tabclau t_cl_v;;
t_aff.(3)<-Vrai;;

t_cl_v;;
t_aff;;
tap;;
tan;;
tdp;;
tdn;;

*)


