open Format

let rec print_ls sep p ppf = function
  | [] -> ()
  | [x] -> p ppf x
  | x::xs ->
    fprintf ppf "%a%s%a" p x sep (print_ls sep p) xs
