{
  open Parser
}

rule token = parse
  | '<' (['a'-'z'] ['a'-'z' '0'-'9']* as s) '>' { START s }
  | "</" (['a'-'z'] ['a'-'z' '0'-'9']* as s) '>' { STOP s }
  | '<' (['a'-'z'] ['a'-'z' '0'-'9']* as s) "/>" { SINGLE s }
  | [^ '>' '<']+ as s { STR s }
  | eof { EOF }
