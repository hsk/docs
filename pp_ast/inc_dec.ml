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
  let sp = ref "" in
  let ss = ref [] in
  let push() = ss := !sp :: !ss in
  let pop() =
    begin match !ss with
    | [] -> ()
    | x::xs -> sp:= x; ss := xs
    end
  in


  let inc1() = sp := !sp ^ "  " in



  let dec1() = sp := String.sub !sp 2 ((String.length !sp)-2) in

  let rec print = function
  | EInt i -> Printf.sprintf "%s%d" !sp i
  | EBlock ls ->
    let rc = Printf.sprintf "%s{\n" !sp in
    inc1();
    let rc = Printf.sprintf "%s%s\n" rc (print_ls print ls) in
    dec1();
    let rc = Printf.sprintf "%s%s}" rc !sp in
    rc
    
  | EIf(e1,e2,e3) ->
    push();sp:="";
    let rc = Printf.sprintf "if(%s)" (print e1) in
    pop();
    inc1();
    let rc = Printf.sprintf "%s\n%s" rc (print e2) in
    let rc = Printf.sprintf "%s\n%s" rc !sp in
    let rc = Printf.sprintf "%selse\n%s" rc (print e3) in
    dec1();
    rc
  | EEmpty -> ""
  | EBin(op,e1,e2) ->
    push();
    sp := "";
    let rc = Printf.sprintf "(%s %s %s)" (print e1) op (print e2) in
    pop();
    rc
  in print e
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
