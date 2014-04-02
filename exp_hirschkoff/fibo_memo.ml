
let memo f =
  let h = Hashtbl.create 29 in
  fun x ->
    try Hashtbl.find h x with
	Not_found ->
	  let y = f x in
	  Hashtbl.add h x y;
	  y

(* version en "recursion ouverte" *)
let memoize f =                                                 
  let hash = Hashtbl.create 23 in
  let rec f' a =                                                       
    try Hashtbl.find hash a
    with Not_found ->
      let b = f f' a in                                                   
      Hashtbl.add hash a b; b
  in f'

(* une fonction pour ralentir, car la memoization est trop efficace *)
let rec rame k =
  let rame1 () = for i = 0 to 300000 do
      let _ =  [3;4;5] in () done in
  for i = 1 to k do rame1() done

let fibo fibo = (* noter l'argument qui s'appelle fibo! et pas de rec *)
  function
    | 0 | 1 -> 1
    | n ->
      begin
	rame n;
	fibo (n-1) + fibo (n-2)
      end

let fibo_memo = memoize fibo 

let n = read_int ()

let _ = Printf.printf "%d\n" (fibo_memo n); flush stdout


