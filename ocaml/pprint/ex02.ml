open Format

type t =
  | Var of string
  | Tag of string * t list

let rec pp ppf = function
 | Var(s) -> fprintf ppf "%s" s
 | Tag(s,ts) ->
   let rec pps ppf = function
     | [] -> ()
     | x::xs -> fprintf ppf "@\n%a%a" pp x pps xs
   in
   fprintf ppf "%s{@[<2>%a@]@\n}" s pps ts

let _ =
  let a = Tag("test",[Var "a";Var "b"]) in
  let a = Tag("test",[a;a]) in
  let a = Tag("test",[a;a]) in
  printf "%a\n" pp a
