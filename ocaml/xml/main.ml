let parse str =
  Parser.main Lexer.xml_token (Lexing.from_string str)

let parse_exp str =
  Parser.exp Lexer.token (Lexing.from_string str)

let test str =
  let xml = parse str in
  Printf.printf "%s\n" (Ast.show_xml xml)

let teste str =
  let e = parse_exp str in
  Printf.printf "%s\n" (Ast.show_e e)

let () =
  test "<a>b</a>";
  test "<a><c>b</c></a>";
  test "<a><c1>b</c1><d1>ee</d1></a>";
  test "<a><c/><d/></a>";
  test "<a/>";
  test "<a k=\"v\" />";
  test "<a k=\"v\" aa=\"aa\"><!--aa--> bb</a>";
  test "<a k=\"v\" aa=\"aa\"><![CDATA[aa]]> bb&amp;</a>";
  test "<a k=\"v\" aa=\"aa\"><![CDATA[aa]]> bb&#128;</a>";
  test "<a k=\"v\" aa=\"aa\"><![CDATA[aa]]> bb&#xaaff;</a>";
  teste "1";
  teste "1*2+3*4+ <a k=\"k\">{1}</a>";
  teste "1*2+3*4+ <a k={1}>{1}</a>";
  teste "1*2+3*4+ <a {2+1}={1}>{1}</a>";

  print_endline "ok";
  ()
