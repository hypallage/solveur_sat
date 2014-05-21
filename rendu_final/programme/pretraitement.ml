(*/// Convertir l'entree en une formule ////*)

open Types


let affiche_entree_brute l = 
  let rec aux l = 
  begin
    match l with 
      [] -> print_string "] \n"
    |t::q -> begin 
             print_int t ;
             print_string "," ;
             aux q ;
             end
  end
  in
  print_string "[" ;
  aux l 

let rec met_dans_clause x c tauto = 
  if !tauto = 1 then [] 
  else match c with
  [] -> [x] 
 |y::q -> if (abs x = abs y) then 
             begin
             if x = y then c (* doublon *)
             else (tauto := 1 ; [] )
             end
          else if ((abs x)<(abs y)) then
             x::c
          else y::(met_dans_clause x q tauto ) ;;


let rec inclusion_avec_ordre c1 c2 = (* inclusion avec c1 inclus dans c2 ou rien *) 
  match (c1,c2) with
  ([],[]) -> 1
 |(_,[]) -> 0 
 |([],_) -> 1
 |(x1::q1,x2::q2) -> if x1 = x2 then inclusion_avec_ordre q1 q2 
                     else if x1 = -x2 then 0
                     else if (abs x1)<(abs x2) then 0
                     else inclusion_avec_ordre c1 q2 ;;

let rec inclusion c1 c2 = match (c1,c2) with 
  ([],[]) -> 2 (* egalite *)
 |([],_) -> 1  (* c1 inclus dans c2 *)
 |(_,[]) -> -1 (* c2 inclus dans c1 *)
 |(x1::q1,x2::q2) -> if x1 = x2 then inclusion q1 q2  
                     else if x1 = -x2 then 0 
                     else if (abs x1) < (abs x2) then 
                     -(inclusion_avec_ordre c2 q1) (*c2 potentiellement dans c1 *)
                     else 
                     inclusion_avec_ordre c1 q2 ;;(*c1 potentiellement dans c2 *)


(*on insere dans la formule en regardant les inclusions eventuelles *)

 let rec insere c formule n = 
  match formule with
   []   -> (n+1,[c]) 
 |c1::q -> let k = inclusion c1 c in match k with
           0  -> let (n,f) = insere c q n in 
                     (n,c1::f)
         | 1 -> (* c1 inclus dans c, on ne prend pas en compte c *) (n,formule)
         | 2 -> (n,formule) (*egalite *)
         | -1 -> (* c inclus dans c1, on vire c1 et on insere c dans le reste *)
                 insere c q (n-1)  
         | _ -> failwith " ce numero ne doit pas arriver " ;;


let rec traite entree nb_var nb_clauses formule c n1 n2 tauto =
  (* c est la clause en memoire *)
  (* n1 est le nombre de clauses lue, a comparer avec nb_clauses *)
  (* n2 est le nombre de clauses retenues *)
  (* tauto est un indicateur de tautologie *)
  match entree with 
  [] -> if (!n1 = nb_clauses) then (!n2,formule) else failwith "pas le bon nombre de clauses"
 |t::q -> if t = 0 then begin
          if (!tauto = 1) then (*tautologie*) 
          (tauto := 0 ; incr n1 ;
          traite q nb_var nb_clauses formule [] n1 n2 tauto)
    else let (n,f) = insere c formule !n2 in
      (incr n1 ; n2 := n ;(* on incremente toujours ici *)
      traite q nb_var nb_clauses f [] n1 n2 tauto)
    end 
  else traite q nb_var nb_clauses formule (met_dans_clause t c tauto) n1 n2 tauto ;;

let transforme f n = 
  let tab = Array.create (n+1) [] in 
  let rec aux f i tab = 
    match f with
    [] -> tab
  |c::q -> tab.(i) <- c ;   
           aux q (i+1) tab
  in aux f 1 tab ;;

let initialise entree nb_var nb_clauses = 
  let n1 = ref 0 
  and n2 = ref 0 
  and tauto = ref 0 in
  let (n,f) = traite entree nb_var nb_clauses [] [] n1 n2 tauto
  in (n,nb_var,transforme f n) ;;

let construire_formule entree = 
  match entree with
 |nb_var::nb_clauses::q -> initialise q nb_var nb_clauses
 |_ -> failwith "ne doit pas arriver" ;;

(*///// Initialiser les structures de données annexes /////*)

let rec lire_clause c pos neg nb_lit i = match c with
  [] -> () 
 |var::q -> (if (var>0) then pos.(var) <- i::(pos.(var))
                        else neg.(-var) <- i::(neg.(-var)) ) ;
            nb_lit.(i) <- nb_lit.(i) +1 ;
            lire_clause q pos neg nb_lit i ;;

let remplir nb_clauses f pos neg nb_lit i = 
  for i = 1 to nb_clauses do 
    lire_clause f.(i) pos neg nb_lit i ; 
  done ;
  (pos,neg,nb_lit) ;;

let init_les_tab f nb_var nb_clauses =
  let pos = Array.create (nb_var+1) []
  and neg = Array.create (nb_var+1) [] 
  and nb_lit = Array.create (nb_clauses+1) 0 in
  remplir nb_clauses f pos neg nb_lit 1;;

let init_affectation nb_var = 
  let tab = Array.create (nb_var+1) Inconnu in 
  tab ;;


