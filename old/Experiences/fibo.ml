
let r = ref [0]

let rec fibo n =
  if n<=1 then 1 else
    fibo (n-1) + fibo (n-2)

let n = read_int ()

let _ = Printf.printf "%d\n" (fibo n); flush stdout


