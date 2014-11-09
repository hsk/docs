type p =
  | Ptn of string list

type rule =
  | Rule of string * p list

open Format


let rec print_p f = function
  | Ptn(ss) ->
    fprintf f "Ptn[";
    List.iter (fun s -> fprintf f "%s;" s) ss;
    fprintf f "];@?"

let rec print_rule f = function
  | Rule(s,ps) ->
    fprintf f "Rule(%s,[%a])@?"
      s
      (fun f -> List.iter (print_p f)) ps
