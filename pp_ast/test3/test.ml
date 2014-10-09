open Format

let rec print_ls sep p ppf = function
  | [] -> ()
  | [x] -> p ppf x
  | x::xs ->
  	fprintf ppf "%a%s%a" p x sep (print_ls sep p) xs

type t =
  | EInt of int

let rec print sep ppf = function
  | EInt(i) -> fprintf ppf "EInt(%d)" i

let _ =
	fprintf std_formatter "%a@." (print "") (EInt 1)
