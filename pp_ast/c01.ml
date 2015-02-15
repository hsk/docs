type e =
  | EInt of int
  | EBlock of e list
  | EIf of e * e * e
  | EEmpty
  | EBin of string * e * e

let rec print_ls ?(sep="; ") p = function
  | [] -> ""
  | [x] -> p x
  | x::xs -> Printf.sprintf "%s%s%s" (p x) sep (print_ls p xs)

let rec print = function
  | EInt i -> Printf.sprintf "%d" i
  | EBlock ls -> Printf.sprintf "{%s}" (print_ls print ls)
  | EIf(e1,e2,e3) ->
    Printf.sprintf "if(%s) %s else %s"
      (print e1) (print e2) (print e3)
  | EEmpty -> ""
  | EBin(op,e1,e2) -> Printf.sprintf "(%s %s %s)" (print e1) op (print e2)

let _ =
  let e = EBin("+", EInt(1), EInt(2)) in
  Printf.printf "%s\n" (print e);
  let e2 = EBlock[e;e] in
  Printf.printf "%s\n" (print e2);
  let e3 = EIf(e,e,e) in
  Printf.printf "%s\n" (print e3);

  let e4 = EIf(e,e2,e2) in
  Printf.printf "%s\n" (print e4);
