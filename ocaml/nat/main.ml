open Format

let rec eval = function
  | Ast.Z -> 0
  | Ast.S(n) -> (eval n) + 1

let main =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let ast = Parser.main Lexer.token lexbuf in
    fprintf std_formatter "%a@." Ast.print ast;
    fprintf std_formatter "%d@." (eval ast)
  done
