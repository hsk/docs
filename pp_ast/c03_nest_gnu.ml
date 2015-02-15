type e =
  | EInt of int
  | EBlock of e list
  | EIf of e * e * e
  | EEmpty
  | EBin of string * e * e

let rec print_ls ?(sep=";\n") p = function
  | [] -> ""
  | [x] -> p x
  | x::xs -> Printf.sprintf "%s%s%s" (p x) sep (print_ls p xs)

let print e = 
  let rec print sp = function
  | EInt i -> Printf.sprintf "%s%d" sp i
  | EBlock ls -> Printf.sprintf "%s{\n%s\n%s}" sp (print_ls (print (sp^"  "))ls) sp
  | EIf(e1,e2,e3) ->
    Printf.sprintf "if(%s)\n%s\n%selse\n%s"
      (print "" e1) (print (sp^"  ") e2) sp (print (sp^"  ") e3)
  | EEmpty -> ""
  | EBin(op,e1,e2) -> Printf.sprintf "%s(%s %s %s)" sp (print "" e1) op (print "" e2)
  in print "" e
let _ =
  let e = EBin("+", EInt(1), EInt(2)) in
  Printf.printf "%s\n" (print e);
  let e2 = EBlock[e;e] in
  Printf.printf "%s\n" (print e2);
  let e3 = EIf(e,e,e) in
  Printf.printf "%s\n" (print e3);

  let e4 = EIf(e,e2,e2) in
  Printf.printf "%s\n" (print e4);
  
  let e5 = EBin("=", EInt(1), EIf(e,e2,e2)) in
  Printf.printf "%s\n" (print e5);
