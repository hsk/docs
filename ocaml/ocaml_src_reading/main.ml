let version = "0.0.1"

let parse input =
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let asts = Parser.use_file Lexer.token lexbuf in
  close_in inp;
  asts

open Lexing
open Lexer
let print_pos pos = 
  Printf.printf "pos_fname: %s pos_lnum %d pos_bol %d pos_cnum %d\n"
    pos.pos_fname
    pos.pos_lnum
    pos.pos_bol
    pos.pos_cnum


let trans input =
  let asts = parse input in

  let out = open_out (input ^ "1") in
  Pprintast.comments := Lexer.comments();
  let f = Format.formatter_of_out_channel out in
  List.iter (fun ast ->
    Pprintast.top_phrase f ast
  ) asts;
  List.iter (fun (x,_) ->
    Format.fprintf f "(*%s*)@." x;
  ) !Pprintast.comments;
  close_out out;
  let out = open_out (input ^ "2") in
  List.iter (fun ast ->
    Printast.top_phrase (Format.formatter_of_out_channel out) ast
  ) asts;
  close_out out;
  List.iter (fun (s,loc) ->
    Printf.printf "comment: (*%s*)\n" s;
    Printf.printf "loc_start: ";
    print_pos loc.Location.loc_start;
    Printf.printf "loc_end: ";
    print_pos loc.Location.loc_end;
  ) (Lexer.comments())
let get_ext name =
  if Str.string_match (Str.regexp ".*\\.\\([^.]*\\)$") name 0 then
    Str.matched_group 1 name
  else
    ""

let replace_ext name ext =
  Str.global_replace (Str.regexp "\\(\\.[^.]*$\\)") ext name

let _ =
  let ver = ref false in
  let files: string list ref = ref [] in
  Arg.parse
    [
      "-v", Arg.Unit(fun()->ver:=true), "version";
    ]
    (fun s -> files := !files @ [s])
    ("Camlup Compiler (C) Hiroshi Sakurai\n" ^
     Printf.sprintf "usage: %s [-run] ...filenames" Sys.argv.(0));
  if !ver then(
    Printf.printf "Camlup Comiler %s\n" version;
    exit(0)
  );
  List.iter (fun (name) ->
    trans name 
  ) !files

