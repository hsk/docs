let a = 1
let add10 b = b + 10
let _ =
	Printf.printf "%d\n" (add10 20)

open Printf

let arr = Array.make 1 (-10)

let _ =
	printf "%d\n" arr.(0);
	arr.(0) <- 1;
	printf "%d\n" arr.(0)