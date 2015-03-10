let j2class src =
  let len = String.length src in
  if String.sub src (len - 2) 2 = ".j"
  then
    String.sub src 0 (len - 2) ^ ".class"
  else
    failwith ("filename is bad. " ^ src)

let lexbuf l =
	print_endline @@ "lexbuf";
	Parser.jas_file Lexer.token l

let file f =
  let inchan = open_in f in
  begin try
    Parser.sourcefile := Some f;
    let k = lexbuf (Lexing.from_channel inchan) in

    (*Javalib.unparse_class k (open_out (j2class f));*)
    (*JPrint.print_jasmin k stdout;*)
    close_in inchan
  with e ->
    close_in inchan;
    raise e
  end

let () =
  let usage = "jasc version 0.0.1\n  usage: jasc files" in
  let files = ref [] in
  Arg.parse [] (fun s -> files := !files @ [s]) usage;
  if !files = [] then print_endline usage else
  List.iter (fun f ->
    print_endline @@ "compile " ^ f;
    ignore (file f)
  ) !files
