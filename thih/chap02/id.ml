module Id = struct
  type id = string

  (* 数値に対するidを取得する *)
  let enumId (n:int) : id =
    "v" ^ string_of_int n
end

let _ =
  Printf.printf "%s\n" (Id.enumId 1)
