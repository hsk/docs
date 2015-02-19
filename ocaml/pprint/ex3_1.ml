type t =
  | Var of string
  | Tag of string * t list

    let e = Tag("a",[
      Var "1";
      Tag("b",[
        Var "c";
        Var "d"
      ]);
      Var "2"
    ])

let rec pp sp = function
 | Var(s) -> Printf.sprintf "%s%s\n" sp s
 | Tag(s,ts) ->
   let rec pps sp = function
     | [] -> ""
     | x::xs -> (pp sp x) ^ (pps sp xs)
   in
   Printf.sprintf "%s%s{\n%s%s}\n" sp s (pps ("  "^sp) ts) sp

let _ =
  Printf.printf "%s" (pp "" e)
