(*
Cette exception est levée quand faux apparait dans la table de vérité des clauses.
*)
open Types;;
exception Probleme;;
exception Clausenondes;;

(*
Ces deux fonctions servent à changer l'activité d'une clause. La première met la variable var
à l'état inverse (desactivé si elle était activée et inversement). On met pour cela le tableau
où la variable apparait en premier. La deuxième fait ceci pour toute les variables de la clause.
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



