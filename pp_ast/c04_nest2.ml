type e =
  | EInt of int
  | EBlock of e list
  | EIf of e * e * e
  | EEmpty
  | EBin of string * e * e
  | ECall of e * e list
  | EVar of string

let rec print_ls ?(sep=";\n") p = function
  | [] -> ""
  | [x] -> p x
  | x::xs -> Printf.sprintf "%s%s%s" (p x) sep (print_ls p xs)

let print e = 
  let rec print sp = function
  | EInt i -> Printf.sprintf "%s%d" sp i
  | EVar i -> Printf.sprintf "%s%s" sp i
  | EBlock ls -> Printf.sprintf "%s{\n%s\n%s}" sp (print_ls (print (sp^"  "))ls) sp
  | EIf(e1,e2,e3) ->
    (Printf.sprintf "if (%s)" (print "" e1)) ^
    (print2 sp e2 "\n") ^
    (Printf.sprintf "%selse" sp) ^
    (print2 sp e3 "") ^
    ""
  | EEmpty -> ""
  | EBin(op,e1,e2) -> Printf.sprintf "%s(%s %s %s)" sp (print "" e1) op (print "" e2)
  | ECall(e1,es) -> Printf.sprintf "%s(%s)" (print sp e1) (print_ls (print "") es ~sep:", ")
  and print2 sp e ed = 
    match e with
    | EBlock ls -> " " ^ print sp e ^ (if ed <> "" then " " else "")
    | e -> "\n" ^ print (sp^"  ") e ^ ed

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
  let ec = ECall(EVar("ff"), [EInt(2);EInt(3)]) in  
  let e5 = EBin("=", EVar("a"), EIf(e,e2,ec)) in
  Printf.printf "%s\n" (print e5);
