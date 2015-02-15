#load "unix.cma";;


let _ =
  Printf.printf "aaa@.\n";flush stdout;
  Unix.sleep 1;
  Printf.printf "bbb@.\n";flush stdout;
