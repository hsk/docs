let rec string_of_coqstring cstr = 
  let buf = Buffer.create 256 in
  let rec cnv = function
    | [] -> Buffer.contents buf
    | c::cstr ->
      Buffer.add_char buf c;
      cnv cstr
  in
  cnv cstr

let () =
  Printf.printf "%s\n" (string_of_coqstring Hello.msg)

