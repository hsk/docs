open Ast

let parse input =
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in inp;
  ast  

  
let trans input output =
  let (starts,ast) = parse input in
  Format.fprintf Format.std_formatter "starts[%a]@."
    (fun f -> List.iter (fun l ->
      Format.fprintf Format.std_formatter "%s;" l)) starts;

  Format.fprintf Format.std_formatter "%a@."
  	(fun f ls -> List.iter (fun l -> Ast.print_rule f l; Format.fprintf Format.std_formatter "@.") ls) ast;

  (* sort *)
  Printf.printf "scc\n";
  let ls = List.map(function | Rule(l,pts) ->
    let pts = List.map( function (Ptn(ls)) ->
      ls
    ) pts in
    (l,List.flatten pts)
  ) ast in
  let map1 = Scc.mkMap ls in
  let map = Scc.mkMap2 ls in
  Printf.printf "map\n";
  Scc.print_map map;
  Printf.printf "scc\n";
  let ls = Scc.scc(map) in
  let ls = List.rev ls in
  List.iter(fun l -> 
    List.iter(fun l -> 
      Printf.printf "%s," l
    ) l;
    Printf.printf "\n"

  ) ls;
  let all = List.flatten ls in
  (*成分毎にループしてチェック*)
  let rec check uses = function
    | [] -> uses
    | x::xs ->
      (*xは同じ成分のリスト*)
      let b = List.exists (fun v->
        if List.mem v uses then true
        else List.exists (fun use->
          if not (Scc.M.mem use map1) then false
          else
          (
            let set = Scc.M.find use map1 in
            List.mem v set
          )
        ) uses
      ) x in
      if b then check (Scc.S.elements (Scc.S.union (Scc.mkSet x) (Scc.mkSet uses) )) xs
      else check uses xs
  in
  let ls = check starts ls in
  Format.fprintf Format.std_formatter "depends[%a] len=%d@."
    (fun f -> List.iter (fun l ->
      Format.fprintf Format.std_formatter "%s;" l)) ls
    (List.length ls)
    ;
  let nouse = Scc.S.diff (Scc.mkSet all) (Scc.mkSet ls) in
  let nouse = Scc.S.elements nouse in
  Format.fprintf Format.std_formatter "nouse[%a] len=%d@."
    (fun f -> List.iter (fun l ->
      Format.fprintf Format.std_formatter "%s;" l)) nouse
    (List.length nouse)


let get_ext name =
  if Str.string_match (Str.regexp ".*\\.\\([^.]*\\)$") name 0 then
    Str.matched_group 1 name
  else
    ""

let replace_ext name ext =
  Str.global_replace (Str.regexp "\\(\\.[^.]*$\\)") ext name

let _ =
  let files: string list ref = ref [] in
  let run = ref false in
  Arg.parse
    [("-run", Arg.Unit(fun()->run:=true), "run")]
    (fun s -> files := !files @ [s])
    ("Newml Compiler (C) Hiroshi Sakurai\n" ^
     Printf.sprintf "usage: %s [-run] ...filenames" Sys.argv.(0));
  List.iter (fun (name) ->
    let ext = get_ext name in
    match ext with
    | "mly" -> trans name (replace_ext name ".ml")
    | _ -> failwith "bad filetype."
  ) !files
