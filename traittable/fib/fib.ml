let rec fib n =
  if n < 2 then
    1
  else
    fib(n-2)+fib(n-1)

class f(n) = object
  val _n = n
  method fib =
    if _n < 2 then
      1
    else
       ((new f(_n-2))#fib) + ((new f(_n-1))#fib)
end

let tim ():int = int_of_float(Sys.time() *. 1000.0)

let _ =
  let start = tim() in
  Printf.printf "%d\n" (fib(40));
  Printf.printf "%dms\n" ((tim())-start);
  let start = tim() in
  Printf.printf "%d\n" ((new f(40))#fib);
  Printf.printf "%dms\n" ((tim())-start);

