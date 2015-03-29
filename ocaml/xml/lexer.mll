{
open Parser
}

let space = [' ' '\t']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '-']

rule token = parse
  | '<' (['a'-'z'] ['a'-'z' '0'-'9']* as s) space*
    {
      match attributes lexbuf with
      | l,false -> START(s, l)
      | l,true -> SINGLE(s, l)
    }
  | "</" (['a'-'z'] ['a'-'z' '0'-'9']* as s) '>' { STOP s }
  | "<!--" as s { COMMENT (s ^ comment lexbuf) }
  | "<![CDATA[" as s { CDATA (s ^ cdata lexbuf) }
  | eof { EOF }
  | "" { STR (str lexbuf) }

and attributes = parse
  | '>' { [], false }
  | "/>" { [], true }
  | (identchar+ as a) space* '=' space* '"' (([^ '"' '\\'] | '\\' ['"' '\''])* as s) '"' space*
    {
      let l,r = attributes lexbuf in
      ((a,s)::l,r)
    }
and comment = parse
  | "-->" as s { s }
  | _ as s { (String.make 1 s) ^ (comment lexbuf) }
  | eof { assert false }
and cdata = parse
  | "]]>" as s { s }
  | _ as s { (String.make 1 s) ^ (cdata lexbuf) }
  | eof { assert false }
and str = parse
  | "&amp;" { "&" ^ str lexbuf }
  | "&lt;"  { "<" ^ str lexbuf }
  | "&gt;"  { ">" ^ str lexbuf }
  | "&apos;" { "'" ^ str lexbuf } 
  | "&quot;" { "\"" ^ str lexbuf  }
  | "&#" ['0'-'9']+ ";" as s { s ^ str lexbuf }
  | "&#" ['X' 'x'] ['0'-'9' 'a'-'f' 'A'-'F']+ ";" as s { s ^ str lexbuf }
  | [^ '<' ] as s { (String.make 1 s) ^ str lexbuf }
  | "" { "" }
