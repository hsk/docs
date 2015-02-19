open Format

type t =
  | Var of string
  | Let of string * t * t

let rec pp ppf = function
 | Var(s) -> fprintf ppf "%s" s
 | Let(s,(Let _ as ts),s2) ->
   fprintf ppf "@[<2>let %s = @\n%a@]@\nin@\n%a" s pp ts pp s2
 | Let(s,ts,s2) ->
   fprintf ppf "let %s = %a in@\n%a" s pp ts pp s2

let _ =
  let a = Let("test",Var "a",Var "b")in
  let a = Let("test",a,a) in
  printf "%a\n" pp a

