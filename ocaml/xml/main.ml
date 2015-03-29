let parse str =
  Parser.main Lexer.token (Lexing.from_string str)

let test str =
  let str = parse str in
  print_endline str

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

  print_endline "ok";
  ()
