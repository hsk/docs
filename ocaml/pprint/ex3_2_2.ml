module PP = struct
  let rec to_s a sp = function
    | "@["::os -> to_s a (sp ^ "  ") os
    | "@\n"::os -> to_s (a ^ "\n" ^ sp) sp os
    | "@]"::os ->
      to_s a
        (String.sub sp 0 ((String.length sp) - 2))
        os
    | o::os -> to_s (a^o) sp os
    | [] -> a

  let buf = ref []

  let put (s:string) =
    buf := s::!buf

  let puts ss =
    List.iter put ss

  let get () =
    let ls = List.rev !buf in
    buf := [];
    to_s "" "" ls
end

let _ =
  PP.puts["@[";"tes";"("];
    PP.puts["@\n";"@[";"tes";"(";];
      PP.puts["@\n"; "1"];
    PP.puts["@]";"@\n";")"];
  PP.puts["@]";"@\n";")"];
  let str = PP.get() in
  Printf.printf "%s\n" str

