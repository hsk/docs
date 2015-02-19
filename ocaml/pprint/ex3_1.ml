type t =
  | Var of string
  | Tag of string * t list

let rec pp sp = function
 | Var(s) -> Printf.sprintf "%s%s\n" sp s
 | Tag(s,ts) ->
   let rec pps sp = function
     | [] -> ""
     | x::xs -> (pp sp x) ^ (pps sp xs)
   in
   Printf.sprintf "%s%s{\n%s%s}\n" sp s (pps ("  "^sp) ts) sp

let _ =
  let a = Tag("test",[Var "a";Var "b"]) in
  let a = Tag("test",[a;a]) in
  let a = Tag("test",[a;a]) in
  Printf.printf "%s" (pp "" a)
