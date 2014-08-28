(*

pritty printing
refs:

https://gist.github.com/ytomino/d3cff860d30d894dca1b
https://github.com/bobzhang/ocaml-book/blob/master/compiler/pipeline.org

usages:
ocaml -I `ocamlc -where`/compiler-libs ocamlcommon.cma ocamlbytecomp.cma a2.ml

*)
let read_type_exp src_string =
  let env = !Toploop.toplevel_env in
  let lb = Lexing.from_string src_string in
  match Parse.implementation lb  with
  | [{Parsetree.pstr_desc = Parsetree.Pstr_eval exp}] ->
      Ctype.init_def(Ident.current_time());
      Typecore.reset_delayed_checks ();
      let texp = Typecore.type_expression env exp
      in Typecore.force_delayed_checks (); texp
  | _ -> failwith "Only expressions are expected"

type e2 = A2
  | AInt of int
let _ =
  let te = read_type_exp "A2" in
  let value = AInt(2) in
  Printf.printf "--------\n";
  let env = !Toploop.toplevel_env in
        Toploop.print_value
                env
                (Obj.repr value)
                Format.std_formatter
                te.exp_type;
  Format.fprintf Format.std_formatter "@."

