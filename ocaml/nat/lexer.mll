{
open Parser
}

rule token = parse
  | [' ' '\t']     { token lexbuf }
  | 'S'            { S }
  | 'Z'            { Z }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '\n'           { EOL }