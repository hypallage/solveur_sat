open Types
exception Tautologie 



(* Contient les differentes phases de l'algorithme *)


(*/////// Distribution dans les seaux ////////*) 

let insere_clause_dans_seau c plus moins = 
  let (x,typ) = List.hd c in 
  if typ = Pos then  
  plus.(x) <- c::(plus.(x)) 
               else
  moins.(x) <- c::(moins.(x))

let init_les_seaux f nb_var = 
  let plus = Array.create (nb_var+1) []
  and moins = Array.create (nb_var+1) [] in
  let rec aux f =  match f with
  [] -> (plus,moins) 
 |c::q -> begin
          insere_clause_dans_seau c plus moins ;
          aux q
          end
  in aux f 

(*/////// Gestion des resolvants /////////*)

let resolvant c1 c2 = 
  try (
  let rec aux c1 c2 = match (c1,c2) with
  ([],_) ->  c2
 |(_,[]) -> c1
 |((x1,typ1)::q1,(x2,typ2)::q2) -> 
   if x1 > x2 then
   (x1,typ1)::(aux q1 c2)
   else
   if x2 > x1 then 
   (x2,typ2)::(aux c1 q2)
   else
   if ((x1=x2)&&(typ1=typ2)) then 
   (x1,typ1)::(aux q1 q2)
   else raise Tautologie
   in aux (List.tl c1) (List.tl c2) )  (* Il faut enlever le litteral le plus grand quand on fusionne *)
   with Tautologie -> [(0,Pos)] 

let traite_les_resolvants i plus moins =
  let clauses_pos = plus.(i) 
  and clauses_neg = moins.(i) in
  let rec aux_un c l  = 
  match l with
  [] -> ()
 |c2::q -> let r = resolvant c c2 in 
    if r = [] then failwith "pas de solution (clause vide)"
    else 
    if (fst(List.hd r) <> 0) (* pas d'exception tautologie *) 
    then insere_clause_dans_seau r plus moins ;
    aux_un c q
  in
  let rec aux l1 l2 = match l1 with
  [] -> ()
 |c::q -> begin 
          aux_un c l2 ;
          aux q l2
          end
  in aux clauses_pos clauses_neg 


(*//////// Descente dans les seaux //////*)

let vide_le_seau i plus moins =
  let fin = ref (0,[]) in
  let (l1,l2) = (plus.(i),moins.(i)) in
  match (l1,l2) with
  ([],[]) -> !fin
 | (_,[]) -> fin := (1,l1); !fin
 | ([],_) -> fin := (1,l2); !fin
 | _ -> if i <> 1 then
        begin
        traite_les_resolvants i plus moins ;
        !fin 
        end
        else (* si i = 1 et que les deux seaux ne sont pas vides, ils contiennent x1 et not(x1) contradiction *) 
        failwith "pas de solution (x1 et not(x1))" 

(*///// Evaluation sous forme normale DISJONCTIVE /////*)

(* ATTENTION : au moment de remonter, si c = x1,x2....xn, la contrainte associee sera
     not(x1) and not (x2) and .... and not(xn-1) ==> xn *)

let evalue c tab = 
  try(
    let rec aux c tab = match c with
    [] -> true
    |(n,litte)::q -> let booleen = tab.(n) in 
      match (booleen,litte) with
        (true,Neg) -> aux q tab
       |(false,Pos) -> aux q tab
       | _ -> raise Exit 
    in aux c tab)
  with Exit -> false 

let decision v (litte,c) = 
  let booleen = evalue c v in 
  if booleen then (* il faut modifier *)
  match litte with
    Pos -> 1
   | Neg -> 2
  else 0 

let add x l = match l with
  [] -> [x]
 |t::q -> if x = t then l else
          failwith "contradiction" 

let resultat_de_csqcs v i_eme_seau litte = 
  try( 
    let rec aux v i_eme_seau litte = match i_eme_seau with 
      [] -> []
      |c::q -> let l = aux v q litte
                       and n = decision v (litte,c) in
  if n = 0 then l
  else if n = 1 then add true l 
  else add false l 
   in aux v i_eme_seau litte )
  with _ -> failwith "contraction" 

(*/////// Remonter dans les seaux ///////*)

let rec init_possibilites i nb_var = (* initialise jusqu'au début du seau *)
 (* ex : sur la clause finale [x1,x2], i = 2 on peut fixer x1 et x2 et ensuite
    faire jouer les consequences *)
  if i = 0 then [(Array.create (nb_var+1) true)] 
  else let l = init_possibilites (i-1) nb_var in
  let rec aux l i nb_var = 
  match l with 
  [] -> [] 
  |tab::q -> let tab1 = Array.copy tab in 
                 tab1.(i) <- false ;
                 tab::tab1::(aux q i nb_var) 
  in aux l i nb_var

let met_a_jour v plus moins i = 
  try(
  let l1 = resultat_de_csqcs v plus.(i) Pos
  and l2 = resultat_de_csqcs v moins.(i) Neg
    in match (l1,l2) with
    ([],[]) -> let copie = Array.copy v in 
    copie.(i) <- false ;
    [v;copie]
   |([],booleen::_) -> v.(i) <- booleen ;
                       [v]
   |(booleen::_,[]) -> v.(i) <- booleen ;
                       [v]
   |(b1::_,b2::_)-> if b1 = b2 then 
                    begin
                    v.(i) <- b1 ;
                    [v]
                    end
                    else [])
  with _ (* contradiction *) -> []

let rec remonte l i nb_var trouve solution plus moins =
(* l = ensemble de valuations, i = seau dans lequel on se trouve*)
  if i = nb_var+1
  then begin
       trouve := true ; (* pas besoin de calculer le reste *)
       solution := List.hd l ;
       []
       end
  else match l with 
    [] -> []
 |v::q -> let c = met_a_jour v plus moins i in 
          if c = [] 
          then remonte q i nb_var trouve solution plus moins
          else
          (remonte c (i+1) nb_var trouve solution plus moins)@(remonte q i nb_var trouve solution plus moins)   

(*//// Temps d'execution d'une fonction a deux arguments /////*)

let time f a b = 
  let x = Sys.time() in
  let _ = f a b in
  let y = Sys.time() in 
  print_string("temps d'execution :");
  print_float(y -.x );
  print_string("\n") 


