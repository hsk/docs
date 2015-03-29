%{
	let rec attrs = function
	  | [] -> ""
	  | (k,v)::xs -> Printf.sprintf " %s=%S" k v ^ attrs xs
%}

%token <string*(string*string)list> START
%token <string> STOP
%token <string*(string*string)list> SINGLE
%token <string> STR
%token EOF

%start main
%type <string> main

%%
main        : | xml_tag EOF { $1 }
xml_tag     : | START value STOP {
				  let (a, ls) = $1 in
	              if a <> $3 then failwith "end tag error";
	              "<"^a^(attrs ls)^ ">" ^ $2 ^ "</"^a^">"
	            }
	          | SINGLE {
				  let (a, ls) = $1 in
	              "<"^a^(attrs ls)^"/>"
	            }
value       : | xml_tag { $1 }
              | STR { $1 }
              | value xml_tag { $1 ^ $2 }
