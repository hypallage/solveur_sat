
(* un type pour l'entrée *)
(* Le prétraitement sera effectué ensuite *)

type entree = int list 

(* Un littéral peut être soit positif, soit négatif *)

type litteral = Pos | Neg 
(* Un type pour les clauses : liste de litteraux, liste car taille 
  variable même si nombre de variables fixées *)

type clause =  (int * litteral) list

(* une formule est une liste de clause, car on va faire des ajouts et des
  suppressions *)

type formule = clause list 


(* Pour le choix du type seau, cf algo.ml *)

type seau = clause list array

type consequence = (litteral * clause ) list array

(* Encorer une fois, le nombre de seaux est connu d'avance : c'est le nombre de variables *)





  
