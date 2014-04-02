open Types
open Algo
open Tests

(* afficher l'entrée de manière "brute" (test) *)

let affiche_entree_brute l = 
  let rec aux l = 
  begin
    match l with 
      [] -> print_string "]"
    |t::q -> begin 
             print_int t ;
             print_string "," ;
             aux q ;
             end
  end
  in
  print_string "[" ;
  aux l 

(* Affiche le resultat *)

let affiche_resultat l nb_var= 
  let une_valuation = List.hd l in 
  print_string "s SATISFIABLE \n" ;
  for i = 1 to nb_var do 
  print_string "v ";
  if (not une_valuation.(i)) then 
  print_string "-" ;
  print_int i ;
  print_string "\n";
  done;;
              

(* Convertir l'entrée en une formule *)

let rec ajoute (x,l) c = match c with
  [] -> [(x,l)] 
 |(y,l2)::q -> if x > y then (x,l)::c 
                        else (y,l2)::(ajoute (x,l) q)



let rec construire_formule l c (*clause courante *) = 
  match l with  
  [] -> [] 
  |x::q -> if x = 0 (* fin de clause *) then 
       [c]@(construire_formule q [])  
      else if x > 0 then 
       construire_formule q (ajoute (x,Pos) c)
      else  
       construire_formule q (ajoute (-x,Neg) c)

let rec convertir l = match l with
  [ ] -> failwith " pas du bon type 1 "
| [_] -> failwith "pas du bon type 2"
| n1::_::q -> (n1,construire_formule q [])
  
     
(* Afficher une formule (brute,uniquement pour les tests) *)

let affiche_clause c =
  print_string "[" ;
  let rec aux l = match l with
  [] -> print_string "]" 
|(x,l)::q -> begin
             print_string "(";
             print_int x ;
             begin 
             if l = Pos then print_string ",Pos)" 
                        else print_string ",Neg)"
             end ;
             aux q
             end
  in aux c  
  

let affiche_formule f = 
  let rec aux l = match l with
  [] -> print_string "]"
 |c::q -> begin
          affiche_clause c ; 
          aux q ; 
          end 
  in print_string "[" ;
  aux f 

(* L'ALGORITHME FINAL *)

let solve f nb_var =
  try(
    let (plus,moins) = init_les_seaux f nb_var
    and csqcs = Array.create (nb_var+1) [] 
    and i = ref nb_var 
    and continue = ref (0,[]) in 
    while ((!i<>0)&&((fst !continue)=0)) do 
    continue := vide_le_seau !i plus moins csqcs ;
    decr i 
    done ;
    (* Toutes les consequences sont maintenant initialisees, on peut partir 
    du rang 1 *)
    (* continue est une unique clause resultante de tous les seaux : c'est la 
       dernière consequence qui a ete traitée
       Elle contient en particulier sur sa tete le dernier seau traite :
       On peut jusque la fixer les variables au hasard, et ensuite commencer
       a remonter *)
    incr i ; (* On revient sur le dernier seau *)
    let j = depart_pour_init !continue in 
    let les_possibilites = ref (init_possibilites (j-1) nb_var) in
    while (!i<>nb_var+1) do
    ( les_possibilites := met_a_jour !les_possibilites csqcs !i ;
      if !les_possibilites = [] then raise Exit ;
      incr i )
    done ;
    affiche_resultat !les_possibilites nb_var ;)
    with _-> print_string "s UNSATISFIABLE \n" ;;
    (* on remonte en suivant les contraintes *) 
    (* On s'est arrete au seau k, apres i <- i-1, son indice est k+1 *)
    (* On peut fixer toutes les valeurs de x1....xk-1 au hasard,
     et propager les contraintes a partir du rang k *)


(* Le programme *)

let compile e =
  begin
    let (n,formule) = convertir e in 
     solve formule n ;
    print_newline() ;
  end

let lexbuf = Lexing.from_channel stdin

let parse () = Parser.main Lexer.token lexbuf

let calc () =
  try (
      let result = parse () in 
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	compile result; flush stdout  ) 
  with  _ -> (print_string "erreur de saisie\n")
;;

let _ = calc()
