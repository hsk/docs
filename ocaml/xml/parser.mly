%{
%}

%token <string> START
%token <string> STOP
%token <string> SINGLE
%token <string> STR
%token EOF

%start main
%type <string> main

%%
main        : | xml_tag EOF { $1 }
xml_tag     : | START value STOP {
	              if $1 <> $3 then failwith "end tag error";
	              "<"^$1^">" ^ $2 ^ "</"^$3^">"
	            }
	          | SINGLE {
	              "<"^$1^"/>"
	            }
value       : | xml_tag { $1 }
              | STR { $1 }
              | value xml_tag { $1 ^ $2 }
