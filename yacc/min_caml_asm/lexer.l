%{
#include <string>
#include <stdlib.h>

#include "ast.h"
#include "parser.h"
extern int yylineno;
void yyerror(const char* s, ...);
%}

%option noyywrap

digit [0-9]
space [ \t\n\r]
lower [a-z]
upper [A-Z]

%%

{space}+  { }
"("       { return LPAREN; }
")"       { return RPAREN; }
"{"       { return LBRACE; }
"}"       { return RBRACE; }
"true"    { yylval.integer = 1; return INT; }
"false"   { yylval.integer = 0; return INT; }
"define"  { return DEFINE; }
"="       { return EQ; }
"unit"    { return UNIT; }
"int"     { return T_INT; }
{digit}+  { yylval.integer = atoi(yytext); return INT; }
"nop"     { return NOP; }
"set"     { return SET; }
"setl"    { return SETL; }
"mov"     { return MOV; }
"neg"     { return NEG; }
"add"     { return ADD; }
"sub"     { return SUB; }
"ld"      { return LD; }
"st"      { return ST; }
"ifeq"    { return IFEQ; }
"ifle"    { return IFLE; }
"ifge"    { return IFGE; }
"call"    { return CALL; }
"save"    { return SAVE; }
"restore" { return RESTORE; }
"else"    { return ELSE; }
":"       { return COLON; }
","       { return COMMA; }
"_"       { yylval.string = new std::string(yytext); return IDENT; /*(Id.gentmp Type.Unit)*/ }
"->"      { return ARROW; }
({upper}|{lower}|"_"|"%")({digit}|{lower}|{upper}|"_")* {
            yylval.string = new std::string(yytext); return IDENT; }
.         { yyerror("Unknown token! %c\n", *yytext); }

%%

void
yyerror(const char *s, ...) {
  va_list ap;
  va_start(ap, s);
  std::fprintf(stderr, "Line %d: error: ", yylineno);
  std::vfprintf(stderr, s, ap);
}

extern int yyparse();

int
parse_input(FILE* f)
{
  yyrestart(f);
  return yyparse();
}

int
parse_string(const char* prog)
{
  yy_scan_string(prog);
  return yyparse();
}
