let read src =
  let ch = open_in src in
  let buf = Buffer.create 1024 in
  begin try
    while true do
      Buffer.add_char buf (input_char ch);
    done;
  with
  | _ ->  close_in ch;
  end;
  Buffer.contents buf

let cnv s =
  let str = read ("../"^s) in
  let str = Str.global_replace (Str.regexp "\\[@@deriving show\\]") "" str in
  let ch = open_out s in
  Printf.fprintf ch "%s" str;
  close_out ch

let _ =
  cnv "jData.ml";
  cnv "jCode.ml";
