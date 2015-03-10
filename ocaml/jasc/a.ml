let debug fmt = Printf.ksprintf (fun s -> failwith s) fmt

let _ =
  debug "test%s" "aa"

