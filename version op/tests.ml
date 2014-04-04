(*/// Les tests ///*)

(* William *)

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

(* Patrice *)

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

(* Exemple de deroulement de l'algo *)
let a = 1;;
let t_cl_v= [|Inconnu;Inconnu;Inconnu;Inconnu;Inconnu;Inconnu;Inconnu|];;
est_satisfiable t_cl_v ;;
let t_aff = [|Inconnu;Inconnu;Inconnu;Inconnu;Inconnu|];;
let nb_v =[|0;3;3;3;3;3;3|];;
let nb_l_f =[|0;0;0;0;0;0;0|];;
let tabclau = [| []; 
            [1;2;-3] ;
						[-1;-2;3] ;
						[1;2;3] ;
						[-2;3;4] ;
						[1;-2;3] ;
						[-1;3;-4] |];;


let tap=[|[];[1;3;5];[1;3;4;2];[3;4;5;6;2];[4]|];;
let tan=[|[];[2;6];[5];[1];[6]|];;
let tdp=[|[];[];[];[];[]|];;
let tdn=[|[];[];[];[];[]|];;

(*let lipari = [];;
let paric =0;;
let ded=[];;*)

cherpoluni tap tan t_aff;;

affecness nb_l_f nb_v t_cl_v;;


pari t_cl_v;;

paric=1;;
t_aff.(1) <- Vrai;;
t_aff;;
tap.(1);;
metliclauvrai tap tan tdp tdn tap.(1) tabclau t_cl_v;;
t_cl_v;;
tap;;
tan;;
tdp;;
tdn;;

auglitfliclau nb_l_f tan.(1);;

nb_l_f;;

conflitoupas nb_v nb_l_f t_cl_v;;

cherpoluni tap tan t_aff;;

let ded=[2];;

(* Ces derniers tests nous ont aider pour visualiser 
      le deroulemement de l'algorithme *)


